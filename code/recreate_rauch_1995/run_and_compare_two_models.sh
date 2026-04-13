#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_and_compare_two_models.sh
# -------------------------------------------------------------------
# This script:
#   1. Downloads SITC Rev.2 product list (if needed)
#   2. Builds year-specific prompts (1995 context)
#   3. Calls an LLM for all SITC-4 codes (Model 1)
#   4. Parses its JSON outputs into a CSV
#   5. Repeats 3–4 for Model 2
#   6. Runs 06_compare_models.R to compare both models vs. Rauch truth
#
# IMPORTANT:
#   - Requires OpenRouter API key in the environment:
#       export OPENROUTER_API_KEY="sk-or-..."
#
# BASIC USAGE
#   ./run_and_compare_two_models.sh MODEL1 MODEL2 [TRUTH]
#
# WHERE
#   MODEL1, MODEL2 = OpenRouter model IDs, e.g.:
#     - openai/gpt-5
#     - anthropic/claude-3.5-sonnet
#   TRUTH          = "con" (conservative, default) or "lib" (liberal)
#
# EXAMPLES
#   # Compare ChatGPT (gpt-5) vs Claude Sonnet, using conservative truth:
#   ./run_and_compare_two_models.sh openai/gpt-5 anthropic/claude-3.5-sonnet
#
#   # Same, but using liberal truth labels:
#   ./run_and_compare_two_models.sh openai/gpt-5 anthropic/claude-3.5-sonnet lib
#
#   # Compare two OpenAI models:
#   ./run_and_compare_two_models.sh openai/gpt-5 openai/gpt-4.1-mini
#
# OUTPUTS
#   - Model predictions (per model) in:
#       output/indicators/rauch_sitc2_*.csv
#   - Multi-model comparison report in:
#       ~/work/data/metrics/multi_model_confusions_*.md
# -------------------------------------------------------------------

MODEL1="${1:?Need MODEL1 (e.g. openai/gpt-5)}"
MODEL2="${2:?Need MODEL2 (e.g. anthropic/claude-3.5-sonnet)}"
TRUTH="${3:-con}"

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  echo "Example: export OPENROUTER_API_KEY=\"sk-or-...\""
  exit 1
fi

STAMP="$(date +%Y%m%d_%H%M%S)"
RUN1="RUN1_${STAMP}"
RUN2="RUN2_${STAMP}"

# Make model strings filename-safe (match 04_parse_llm_sitc_rauch.R)
SAFE1="$(echo "$MODEL1" | sed 's/[^A-Za-z0-9]\+/-/g')"
SAFE2="$(echo "$MODEL2" | sed 's/[^A-Za-z0-9]\+/-/g')"

echo "=== Rauch–LLM Pipeline for Two Models ==="
echo "Model 1: $MODEL1  (run_id = $RUN1)"
echo "Model 2: $MODEL2  (run_id = $RUN2)"
echo "Truth column: $TRUTH"
echo "Reference year (prompts): 1995"
echo

echo "[1/7] Download SITC (if not already present)"
Rscript 01_download_sitc.R

echo "[2/7] Build prompts (1995 context)"
Rscript 02_build_prompt_rauch.R 1995

echo "[3/7] Call LLM for Model 1"
Rscript 03_call_llms.R "$MODEL1" "$RUN1" 1995

echo "[4/7] Parse outputs for Model 1"
Rscript 04_parse_llm_sitc_rauch.R "$MODEL1" "$RUN1"

echo "[5/7] Call LLM for Model 2"
Rscript 03_call_llms.R "$MODEL2" "$RUN2" 1995

echo "[6/7] Parse outputs for Model 2"
Rscript 04_parse_llm_sitc_rauch.R "$MODEL2" "$RUN2"

# Find the parsed CSVs produced by step 4/6
FILE1="$(ls output/indicators/rauch_sitc2_*_${SAFE1}_${RUN1}.csv | tail -n 1 || true)"
FILE2="$(ls output/indicators/rauch_sitc2_*_${SAFE2}_${RUN2}.csv | tail -n 1 || true)"

if [[ -z "$FILE1" || -z "$FILE2" ]]; then
  echo "ERROR: Could not find parsed CSVs for one or both models."
  echo "Looked for patterns:"
  echo "  output/indicators/rauch_sitc2_*_${SAFE1}_${RUN1}.csv"
  echo "  output/indicators/rauch_sitc2_*_${SAFE2}_${RUN2}.csv"
  echo "Check that 03_call_llms.R and 04_parse_llm_sitc_rauch.R ran without errors."
  exit 1
fi

echo "[7/7] Compare models with 06_compare_models.R"
echo "Using files:"
echo "  $FILE1"
echo "  $FILE2"
echo

Rscript 06_compare_models.R "$TRUTH" "$FILE1" "$FILE2"

echo
echo "Done."
echo "Parsed predictions in:"
echo "  output/indicators/"
echo "Multi-model comparison report in:"
echo "  ~/work/data/metrics/multi_model_confusions_*.md"
echo
echo "Example for Sonnet vs ChatGPT (already run):"
echo "  MODEL1=$MODEL1"
echo "  MODEL2=$MODEL2"
echo "  TRUTH=$TRUTH"
