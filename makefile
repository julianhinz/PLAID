# -------------------------------------------------------------------
# PLAID Makefile — orchestrates indicator pipelines
# -------------------------------------------------------------------
#
# Usage:
#   make plaid                          # all indicators, default models
#   make plaid MODELS="mistralai/mistral-small-2603 google/gemini-2.5-flash"
#   make plaid INDICATORS="rauch bec"   # subset of indicators
#   make plaid/rauch                    # single indicator
#   make consistency                    # cross-model agreement report
#   make clean                          # remove temp/ and output/
#
# Environment:
#   OPENROUTER_API_KEY must be set for LLM calls.
#   PLAID_STAMP can be set to resume an existing run.
#
# -------------------------------------------------------------------

RSCRIPT    ?= Rscript
MODELS     ?= mistralai/mistral-small-2603 google/gemini-2.5-flash anthropic/claude-haiku-4.5 openai/gpt-5.4-mini
INDICATORS ?= rauch perishability bec hazmat microchip 3tg
HS_VERSIONS := H0 H1 H2 H3 H4 H5 H6
STAMP      := $(or $(PLAID_STAMP),$(shell date +%Y%m%d-%H%M%S))

HS_XLSX    := input/product_descriptions/HSCodeandDescription.xlsx
HS_FLAG    := temp/hs/.extracted

# -------------------------------------------------------------------
# Top-level targets
# -------------------------------------------------------------------
.PHONY: plaid consistency clean download prompts

plaid: $(addprefix plaid/,$(INDICATORS))
	@echo "All indicators complete."

consistency:
	$(RSCRIPT) code/99_consistency_checks.R

clean:
	rm -rf temp/* output/*

# -------------------------------------------------------------------
# Step 0: Download / extract HS nomenclature (once)
# -------------------------------------------------------------------
$(HS_FLAG): $(HS_XLSX)
	$(RSCRIPT) code/common/download_hs.R
	@touch $@

download: $(HS_FLAG)

# -------------------------------------------------------------------
# Per-indicator targets
# -------------------------------------------------------------------
# Each indicator goes through: prompts → dedup → (model loop: llm → parse → fanout)

define INDICATOR_RULES

.PHONY: plaid/$(1)

# Prompt file pattern — rauch uses "rauch_full", others use indicator name
$(if $(filter rauch,$(1)),\
  $(eval _PROMPT_BASE := rauch_full),\
  $(eval _PROMPT_BASE := $(1)))

# Build prompts for all HS revisions
plaid/$(1)/prompts: $(HS_FLAG)
	@echo "[$(1)] Building prompts for all revisions..."
	@for h in $(HS_VERSIONS); do \
		$(RSCRIPT) code/create_hs_$(1)/02_build_prompts_$(1).R $$$$h; \
	done
	@touch temp/prompts/.prompts_$(1)_done

# Deduplicate prompts across revisions
plaid/$(1)/dedup: plaid/$(1)/prompts
	@echo "[$(1)] Deduplicating prompts..."
	$(RSCRIPT) code/common/deduplicate_prompts.R $(1)

# Full indicator pipeline (all models)
plaid/$(1): plaid/$(1)/dedup
	@for model in $(MODELS); do \
		echo "[$(1)] Calling LLM: $$$$model (stamp: $(STAMP))..."; \
		$(RSCRIPT) code/common/call_llm.R "$$$$model" "deduped-$(STAMP)" \
			--indicator $(1) \
			--prompts temp/prompts/prompts_hs6_$(_PROMPT_BASE)_deduped.csv; \
		echo "[$(1)] Parsing responses..."; \
		$(RSCRIPT) code/common/save_indicators.R --indicator $(1) "$$$$model" "deduped-$(STAMP)"; \
		echo "[$(1)] Fanning out to per-revision CSVs..."; \
		$(RSCRIPT) code/common/fan_out_results.R "$$$$model" $(1); \
	done
	@echo "[$(1)] Done."

endef

$(foreach ind,$(INDICATORS),$(eval $(call INDICATOR_RULES,$(ind))))

# -------------------------------------------------------------------
# Convenience: single-indicator shortcuts
# -------------------------------------------------------------------
.PHONY: rauch perishability bec hazmat microchip 3tg
rauch perishability bec hazmat microchip 3tg: %: plaid/%
