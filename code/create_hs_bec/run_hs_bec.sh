#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_hs_bec.sh
# -------------------------------------------------------------------
# End-to-end HS BEC (Broad Economic Categories) pipeline for a single model:
#   1. Download HS data (if needed)
#   2. Build prompts
#   3. Call LLM via OpenRouter
#   4. Parse outputs to CSV
#   5. Validate against UN BEC benchmark (optional)
#
# Usage:
#   ./run_hs_bec.sh MODEL             # HS H5 (default)
#   ./run_hs_bec.sh MODEL H6          # specify HS version
#
# OUTPUTS
#   - LLM JSON: temp/llm_bec/<model>/<run_id>/*.json
#   - Parsed CSV: output/indicators/bec_hs6_*.csv
#   - Validation report: output/metrics/bec_validation_report.md (if benchmark exists)
# -------------------------------------------------------------------

MODEL="${1:?Need MODEL (e.g. openai/gpt-4o-mini)}"
HS_VER="${2:-H5}"

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  exit 1
fi

STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_ID="${HS_VER}-${STAMP}"

PROMPTS_FILE="temp/prompts/prompts_hs6_bec_${HS_VER}.csv"

echo "=== HS BEC Pipeline ==="
echo "Model        : $MODEL"
echo "Run ID       : $RUN_ID"
echo "HS version   : $HS_VER"
echo

echo "[1/4] Download HS data (if needed)"
Rscript code/common/download_hs.R

echo "[2/4] Build prompts"
Rscript code/create_hs_bec/02_build_prompts_bec.R "$HS_VER"

echo "[3/4] Call LLM"
Rscript code/common/call_llm.R "$MODEL" "$RUN_ID" --indicator bec --prompts "$PROMPTS_FILE"

echo "[4/4] Parse outputs"
Rscript code/create_hs_bec/04_save_indicators.R "$MODEL" "$RUN_ID"

# Step 5: validate if benchmark exists
BENCHMARK="input/benchmarks/bec_hs_concordance.csv"
if [[ -f "$BENCHMARK" ]]; then
  echo "[5/5] Validate against UN BEC benchmark"
  Rscript code/create_hs_bec/05_validate.R "$MODEL" "$RUN_ID"
else
  echo "[5/5] Skipping validation (benchmark not found at $BENCHMARK)"
  echo "      To enable validation, provide: $BENCHMARK"
  echo "      Columns: hs6_code, bec_category (capital|intermediate|consumption)"
fi

echo
echo "Done."
echo "Parsed predictions in output/indicators/"
echo "Raw responses in   temp/llm_bec/$(echo "$MODEL" | sed 's!/!_!g')/$RUN_ID/"
echo
