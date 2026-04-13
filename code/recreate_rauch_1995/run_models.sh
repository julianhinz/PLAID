#!/usr/bin/env bash
set -euo pipefail

# -------------------------------------------------------------------
# run_models.sh
# -------------------------------------------------------------------
# Runs the full SITC Rauch pipeline:
#   Once:      01 (download SITC) → 02 (build prompts, 1995 context)
#   Per model: 03 (call LLM)     → 04 (save indicators)
#
# IMPORTANT:
#   - Requires OpenRouter API key in the environment:
#       export OPENROUTER_API_KEY="sk-or-..."
#
# USAGE
#   ./run_models.sh [TRUTH]
#
# WHERE
#   TRUTH = "con" (conservative, default) or "lib" (liberal)
#
# NOTE: Verify OpenRouter model IDs at https://openrouter.ai/models
#   before running — IDs below may need updating.
# -------------------------------------------------------------------

TRUTH="${1:-con}"

if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "ERROR: Please set OPENROUTER_API_KEY in your environment."
  echo "Example: export OPENROUTER_API_KEY=\"sk-or-...\""
  exit 1
fi

# -------------------------------------------------------------------
# Model IDs — verify these against https://openrouter.ai/models
# -------------------------------------------------------------------
declare -a MODELS=(
  "anthropic/claude-haiku-4-5"      # Claude Haiku 4.5
  "openai/gpt-5.4-mini"             # GPT-5.4 Mini
  "google/gemini-2.5-flash"         # Gemini 2.5 Flash
  "mistralai/mistral-small-2603"    # Mistral Small
)

STAMP="$(date +%Y%m%d_%H%M%S)"

echo "=== SITC Rauch Pipeline ==="
echo "Truth column : $TRUTH"
echo "Stamp        : $STAMP"
echo "Models       :"
for m in "${MODELS[@]}"; do echo "  $m"; done
echo

# -------------------------------------------------------------------
# Steps 01–02: run once
# -------------------------------------------------------------------
echo "[01] Downloading SITC..."
Rscript 01_download_sitc.R

echo "[02] Building prompts..."
Rscript 02_build_prompts_rauch.R

echo

# Collect output CSVs for downstream comparison
declare -a CSV_FILES=()

for MODEL in "${MODELS[@]}"; do
  SAFE="$(echo "$MODEL" | sed 's/[^A-Za-z0-9]\+/-/g')"
  RUN_ID="${SAFE}_${STAMP}"

  echo "─────────────────────────────────────────────"
  echo "Model  : $MODEL"
  echo "Run ID : $RUN_ID"
  echo

  echo "  [03] Calling LLM..."
  Rscript 03_call_llm.R --model "$MODEL" --run_id "$RUN_ID"

  echo "  [04] Saving indicators..."
  Rscript 04_save_indicators.R --model "$MODEL" --run_id "$RUN_ID"

  CSV="$(ls output/indicators/rauch_sitc2_*_${SAFE}_${RUN_ID}.csv 2>/dev/null | tail -n 1 || true)"
  if [[ -z "$CSV" ]]; then
    echo "  WARNING: Could not find output CSV for $MODEL — skipping from summary."
  else
    echo "  Output : $CSV"
    CSV_FILES+=("$CSV")
  fi
  echo
done

echo "═══════════════════════════════════════════════"
echo "Done. Indicators written to output/indicators/:"
for f in "${CSV_FILES[@]}"; do echo "  $f"; done
echo
echo "To compare any two models, run:"
echo "  Rscript 06_compare_models.R $TRUTH <file1.csv> <file2.csv>"
