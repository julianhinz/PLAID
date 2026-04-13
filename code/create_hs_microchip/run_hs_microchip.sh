#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_hs_microchip.sh
# -------------------------------------------------------------------
# End-to-end HS microchip content pipeline for a single model:
#   1. Download HS data (if needed)
#   2. Build prompts
#   3. Call LLM via OpenRouter
#   4. Parse outputs to CSV
#   5. Validate against OECD ICT benchmark
#
# Usage:
#   ./run_hs_microchip.sh MODEL             # HS H5 (default)
#   ./run_hs_microchip.sh MODEL H6          # specify HS version
#
# OUTPUTS
#   - LLM JSON:   temp/llm_microchip/<model>/<run_id>/*.json
#   - Parsed CSV: output/indicators/microchip_hs6_*.csv
#   - Validation: output/metrics/microchip_validation_report.md
# -------------------------------------------------------------------

MODEL="${1:?Need MODEL (e.g. openai/gpt-5)}"
HS_VER="${2:-H5}"

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  exit 1
fi

STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_ID="${HS_VER}-${STAMP}"

PROMPTS_FILE="temp/prompts/prompts_hs6_microchip_${HS_VER}.csv"

echo "=== HS Microchip Content Pipeline ==="
echo "Model        : $MODEL"
echo "Run ID       : $RUN_ID"
echo "HS version   : $HS_VER"
echo

echo "[1/5] Download HS data (if needed)"
Rscript code/common/download_hs.R

echo "[2/5] Build prompts"
Rscript code/create_hs_microchip/02_build_prompts_microchip.R "$HS_VER"

echo "[3/5] Call LLM"
Rscript code/common/call_llm.R "$MODEL" "$RUN_ID" --indicator microchip --prompts "$PROMPTS_FILE"

echo "[4/5] Parse outputs"
Rscript code/create_hs_microchip/04_save_indicators.R "$MODEL" "$RUN_ID"

echo "[5/5] Validate against OECD ICT benchmark"
if [[ -f "input/benchmarks/oecd_ict_hs.csv" ]]; then
  Rscript code/create_hs_microchip/05_validate.R "$MODEL"
else
  echo "  [skip] Benchmark file not found: input/benchmarks/oecd_ict_hs.csv"
  echo "  Place the OECD ICT goods concordance there to enable validation."
fi

echo
echo "Done."
echo "Parsed predictions in output/indicators/"
echo "Raw responses in   temp/llm_microchip/$(echo "$MODEL" | sed 's!/!_!g')/$RUN_ID/"
echo "Validation report  output/metrics/microchip_validation_report.md"
echo
