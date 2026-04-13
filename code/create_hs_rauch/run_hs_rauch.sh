#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_hs_rauch.sh
# -------------------------------------------------------------------
# End-to-end HS Rauch pipeline for a single model:
#   1. Download HS data (if needed)
#   2. Build prompts (classic or year-specific)
#   3. Call LLM via OpenRouter
#   4. Parse outputs to CSV
#
# Usage:
#   ./run_hs_rauch.sh MODEL             # classic wording, HS H5
#   ./run_hs_rauch.sh MODEL 1995        # year-specific wording (1995), HS H5
#   ./run_hs_rauch.sh MODEL H6          # classic wording, HS H6 (shorthand)
#   ./run_hs_rauch.sh MODEL 1995 H6     # year-specific wording, HS H6
#
# OUTPUTS
#   - LLM JSON: temp/llm/<model>/<run_id>/*.json
#   - Parsed CSV: output/indicators/rauch_hs6_*.csv
# -------------------------------------------------------------------

MODEL="${1:?Need MODEL (e.g. openai/gpt-5)}"
ARG2="${2:-}"
ARG3="${3:-}"

# Allow shorthand: if the 2nd arg looks like an HS version (H0–H6), treat it as HS_VER
if [[ -n "$ARG2" && "$ARG2" =~ ^H[0-6]$ && -z "$ARG3" ]]; then
  YEAR=""
  HS_VER="$ARG2"
else
  YEAR="$ARG2"
  HS_VER="${ARG3:-H5}"
fi

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  exit 1
fi

STAMP="$(date +%Y%m%d-%H%M%S)"
RUN_ID="${HS_VER}-${STAMP}"

SUFFIX=""
[[ -n "$YEAR" ]] && SUFFIX="_${YEAR}"
PROMPTS_FILE="temp/prompts/prompts_hs6_rauch_full_${HS_VER}${SUFFIX}.csv"

echo "=== HS Rauch Pipeline ==="
echo "Model        : $MODEL"
echo "Run ID       : $RUN_ID"
echo "Year variant : ${YEAR:-classic}"
echo "HS version   : $HS_VER"
echo

echo "[1/4] Download HS data (if needed)"
Rscript code/common/download_hs.R

echo "[2/4] Build prompts"
if [[ -n "$YEAR" ]]; then
  Rscript code/create_hs_rauch/02_build_prompts_rauch.R "$YEAR" "$HS_VER"
else
  Rscript code/create_hs_rauch/02_build_prompts_rauch.R "$HS_VER"
fi

echo "[3/4] Call LLM"
Rscript code/common/call_llm.R "$MODEL" "$RUN_ID" --indicator rauch --prompts "$PROMPTS_FILE"

echo "[4/4] Parse outputs"
Rscript code/create_hs_rauch/04_save_indicators.R "$MODEL" "$RUN_ID"

echo
echo "Done."
echo "Parsed predictions in output/indicators/"
echo "Raw responses in   temp/llm/$(echo "$MODEL" | sed 's!/!_!g')/$RUN_ID/"
echo
