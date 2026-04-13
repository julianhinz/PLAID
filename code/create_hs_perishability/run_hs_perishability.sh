#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_hs_perishability.sh
# -------------------------------------------------------------------
# End-to-end HS perishability pipeline for a single model:
#   1. Download HS data (if needed)
#   2. Build prompts
#   3. Call LLM via OpenRouter
#   4. Parse outputs to CSV
#
# Usage:
#   ./run_hs_perishability.sh MODEL             # HS H5 (default)
#   ./run_hs_perishability.sh MODEL H6          # specify HS version
#
# OUTPUTS
#   - LLM JSON: temp/llm_perishability/<model>/<run_id>/*.json
#   - Parsed CSV: output/indicators/perishability_hs6_*.csv
# -------------------------------------------------------------------

MODEL="${1:?Need MODEL (e.g. openai/gpt-5)}"
HS_VER="${2:-H5}"

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  exit 1
fi

STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_ID="${HS_VER}-${STAMP}"

PROMPTS_FILE="temp/prompts/prompts_hs6_perishability_${HS_VER}.csv"

echo "=== HS Perishability Pipeline ==="
echo "Model        : $MODEL"
echo "Run ID       : $RUN_ID"
echo "HS version   : $HS_VER"
echo

echo "[1/4] Download HS data (if needed)"
Rscript code/common/download_hs.R

echo "[2/4] Build prompts"
Rscript code/create_hs_perishability/02_build_prompts_perishability.R "$HS_VER"

echo "[3/4] Call LLM"
Rscript code/common/call_llm.R "$MODEL" "$RUN_ID" --indicator perishability --prompts "$PROMPTS_FILE"

echo "[4/4] Parse outputs"
Rscript code/create_hs_perishability/04_save_indicators.R "$MODEL" "$RUN_ID"

echo
echo "Done."
echo "Parsed predictions in output/indicators/"
echo "Raw responses in   temp/llm_perishability/$(echo "$MODEL" | sed 's!/!_!g')/$RUN_ID/"
echo
