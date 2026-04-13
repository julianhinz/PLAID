#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_hs_3tg.sh
# -------------------------------------------------------------------
# End-to-end HS 3TG conflict minerals pipeline for a single model:
#   1. Download HS data (if needed)
#   2. Build prompts
#   3. Call LLM via OpenRouter
#   4. Parse outputs to CSV
#   5. Validate against EU 2017/821 benchmark
#
# Usage:
#   ./run_hs_3tg.sh MODEL             # HS H5 (default)
#   ./run_hs_3tg.sh MODEL H6          # specify HS version
#
# OUTPUTS
#   - LLM JSON:    temp/llm_3tg/<model>/<run_id>/*.json
#   - Parsed CSV:  output/indicators/3tg_hs6_*.csv
#   - Validation:  output/metrics/3tg_validation_report.md
# -------------------------------------------------------------------

MODEL="${1:?Need MODEL (e.g. openai/gpt-4o-mini)}"
HS_VER="${2:-H5}"

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  exit 1
fi

STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_ID="${HS_VER}-${STAMP}"

PROMPTS_FILE="temp/prompts/prompts_hs6_3tg_${HS_VER}.csv"

echo "=== HS 3TG Conflict Minerals Pipeline ==="
echo "Model        : $MODEL"
echo "Run ID       : $RUN_ID"
echo "HS version   : $HS_VER"
echo

echo "[1/5] Download HS data (if needed)"
Rscript code/common/download_hs.R

echo "[2/5] Build prompts"
Rscript code/create_hs_3tg/02_build_prompts_3tg.R "$HS_VER"

echo "[3/5] Call LLM"
Rscript code/common/call_llm.R "$MODEL" "$RUN_ID" --indicator 3tg --prompts "$PROMPTS_FILE"

echo "[4/5] Parse outputs"
Rscript code/create_hs_3tg/04_save_indicators.R "$MODEL" "$RUN_ID"

echo "[5/5] Validate against EU 2017/821 benchmark"
if [[ -f "input/benchmarks/eu_conflict_minerals_hs.csv" ]]; then
  Rscript code/create_hs_3tg/05_validate.R "$MODEL" "$RUN_ID"
else
  echo "  [skip] Benchmark file not found: input/benchmarks/eu_conflict_minerals_hs.csv"
  echo "  Place the EU Regulation 2017/821 Annex I concordance there to enable validation."
fi

echo
echo "Done."
echo "Parsed predictions in output/indicators/"
echo "Raw responses in   temp/llm_3tg/$(echo "$MODEL" | sed 's!/!_!g')/$RUN_ID/"
echo "Validation report: output/metrics/3tg_validation_report.md"
echo
