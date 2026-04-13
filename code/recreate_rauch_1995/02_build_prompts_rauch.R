#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompt_rauch.R
# -------------------------------------------------------------------
# Builds one FULL prompt per SITC code for a three-way w/r/n classification.
#
# Usage:
#   Rscript 02_build_prompt_rauch.R              # defaults to "classic"
#   Rscript 02_build_prompt_rauch.R classic      # generic wording (no explicit year)
#   Rscript 02_build_prompt_rauch.R 1995         # year-specific wording (e.g. 1995)
#   Rscript 02_build_prompt_rauch.R <YEAR>       # any 4-digit year: 1980, 2005, ...
#
# Output:
#   temp/prompts/prompts_sitc2_rauch_full.csv
#   temp/prompts/prompts_sitc2_rauch_full_<YEAR>.csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, jsonlite, glue, stringr, readr, rlang)

`%||%` <- get("%||%", envir = asNamespace("rlang"), inherits = FALSE)

# ────────────────────────────────────────────────────────────────────
# 0  CLI: variant selector
#     - "classic"  → no explicit year / generic wording
#     - <YYYY>     → year-specific wording (temporal context = YYYY)
#     - default    → classic
# ────────────────────────────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)

is_year <- function(x) grepl("^[0-9]{4}$", x)

if (length(args) >= 1 && args[[1]] == "classic") {
  variant  <- "classic"
  ref_year <- NA_integer_
} else if (length(args) >= 1 && is_year(args[[1]])) {
  variant  <- "year"
  ref_year <- as.integer(args[[1]])
} else {
  variant  <- "classic"
  ref_year <- NA_integer_
}

suffix <- if (variant == "classic") "" else paste0("_", ref_year)

message("Prompt variant: ", variant, if (!is.na(ref_year)) paste0(" (", ref_year, ")"))

# ────────────────────────────────────────────────────────────────────
# 1  I/O paths
# ────────────────────────────────────────────────────────────────────

temp_sitc_dir <- file.path("temp", "sitc")
temp_wiki_dir <- file.path("temp", "wiki")
prompt_dir    <- file.path("temp", "prompts")
dir.create(prompt_dir, recursive = TRUE, showWarnings = FALSE)

# ────────────────────────────────────────────────────────────────────
# 2  Load SITC nomenclature + optional Wikipedia extracts
# ────────────────────────────────────────────────────────────────────

sitc_file <- list.files(temp_sitc_dir, pattern = "sitc_2\\.csv$", full.names = TRUE)
stopifnot(length(sitc_file) == 1)

sitc_full <- fread(sitc_file)

# Find a column name ignoring case/spacing
find_col <- function(df, candidates) {
  match <- candidates[tolower(candidates) %in% tolower(names(df))]
  if (length(match) == 0) return(NULL)
  actual <- names(df)[tolower(names(df)) == tolower(match[1])]
  actual
}

code_col <- find_col(sitc_full, c("ProductCode", "Code"))
desc_col <- find_col(sitc_full, c("Product Description", "ProductDescription", "description"))

sitc_dt <- sitc_full[Tier == 4, .(
  code        = as.character(.SD[[code_col]]),
  description = as.character(.SD[[desc_col]])
)]

sitc_dt <- sitc_dt[description != "UN Special Code", ]

# Optional: short Wikipedia extract per code (if present)
get_extract <- function(code) {
  jf <- file.path(temp_wiki_dir, paste0(code, ".json"))
  if (!file.exists(jf)) return("")
  txt <- fromJSON(jf)$extract %||% ""
  substr(txt, 1, 400)
}
sitc_dt[, wiki := vapply(code, get_extract, character(1))]

# Safety trims
sitc_dt[, description := str_trim(substr(description %||% "", 1, 260))]
sitc_dt[, wiki        := str_trim(wiki)]

# ────────────────────────────────────────────────────────────────────
# 3  Shared metadata
# ────────────────────────────────────────────────────────────────────

indicator_name <- "rauch_wr_n"  # internal key for downstream scripts

# ------------------- PROMPT TEXT (classic vs year-specific) -------------------

if (variant == "classic") {
  # === Generic wording (no explicit temporal context) ===
  defs_full <- glue::glue(
    "You are a trade-economics specialist.",
    "",
    "Task:",
    "Classify each item into three categories with labels w, r, and n.",
    "",
    "Definitions:",
    "- w = organized-exchange: homogeneous goods with standardized contracts and public exchange prices.",
    "- r = reference-priced: no exchange listing, but widely published benchmark prices in trade journals or price-reporting agencies.",
    "- n = differentiated: no exchange and no common benchmark prices; value depends on design, brand, and technical specifications.",
    .sep = "\n"
  )
  
  guidance_full <- glue::glue(
    "Decision order:",
    "1) Exchange test → w",
    "2) Reference-price test → r",
    "3) Else → n",
    "",
    "Use the most specific described form or grade (not just the broad heading).",
    "Tie-breaker: when evidence is strong, prefer the more standardized category (w > r > n).",
    "If information is insufficient, make a best guess with lower confidence and add a flag.",
    "",
    "Cues:",
    "- Exchange (w): CBOT, ICE, NYMEX, or LME contracts; named exchange grades (for example, No. 2 yellow corn, LME Grade A copper cathodes).",
    "- Reference (r): standard specifications with widely published spot or benchmark assessments (polymers, common steel products, pulp, fertilizers).",
    "- Differentiation (n): brand or model identifiers, bespoke machinery or parts, apparel, finished consumer goods.",
    .sep = "\n"
  )
  
  examples_full <- glue::glue(
    "Few-shot examples (format to imitate):",
    "",
    "Example 1",
    "Input → code_system: SITC, code: 5629, description: \"Fertilizers, n.e.s.\"",
    "Output → {{",
    "  \"code_system\": \"SITC\",",
    "  \"code\": \"5629\",",
    "  \"short_description\": \"Fertilizers, n.e.s.\",",
    "  \"rauch_category\": \"r\",",
    "  \"reasoning\": \"Fertilizers are standardized bulk commodities with well-defined specs and widely reported benchmark prices, but are not exchange-listed.\",",
    "  \"evidence_type\": \"reference_prices\",",
    "  \"confidence\": 0.93,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 2",
    "Input → code_system: SITC, code: 6340, description: \"Plywood consisting of sheets of wood\"",
    "Output → {{",
    "  \"code_system\": \"SITC\",",
    "  \"code\": \"6340\",",
    "  \"short_description\": \"Plywood sheets\",",
    "  \"rauch_category\": \"w\",",
    "  \"reasoning\": \"Plywood is a standardized wood product with recognized grades and public market listings, so it fits organized-exchange goods.\",",
    "  \"evidence_type\": \"exchange_listing\",",
    "  \"confidence\": 0.91,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 3",
    "Input → code_system: SITC, code: 6351, description: \"Wooden packing cases, boxes, crates, drums etc.\"",
    "Output → {{",
    "  \"code_system\": \"SITC\",",
    "  \"code\": \"6351\",",
    "  \"short_description\": \"Wooden packing cases and crates\",",
    "  \"rauch_category\": \"n\",",
    "  \"reasoning\": \"Packing cases and crates are typically customized to order and lack exchange or benchmark prices, so they are differentiated goods.\",",
    "  \"evidence_type\": \"differentiation_markers\",",
    "  \"confidence\": 0.90,",
    "  \"flags\": []",
    "}}",
    .sep = "\n"
  )
  
} else {
  # === Year-specific wording (no mention of indices or paper names) ===
  if (is.na(ref_year)) {
    stop("Year-specific variant selected but ref_year is NA. This should not happen.")
  }
  
  defs_full <- glue::glue(
    "You are a trade-economics specialist. Temporal context: {ref_year}.",
    "",
    "Task:",
    "Classify each item into three categories with labels w, r, and n, using a {ref_year} context.",
    "",
    "Definitions:",
    "- w = organized-exchange: homogeneous commodities traded on organized exchanges with standardized contracts and public quotes (for example, LME, CBOT, or NYMEX as of {ref_year}).",
    "- r = reference-priced: not exchange-listed, but widely published benchmark prices exist in {ref_year} trade journals or price-reporting publications (for example, Chemical Marketing Reporter).",
    "- n = differentiated: no exchange listing and no widely accepted benchmark prices; valuation depends on brand, design, and technical specifications; ‘named’ or branded goods and bespoke machinery or apparel fall here.",
    "",
    "Notes:",
    "- Use {ref_year} institutions, market structure, and publications; do not rely on post-{ref_year} developments.",
    "- Think in terms of market thickness and arbitrage opportunities in {ref_year}: organized exchanges centralize price information; reference-priced goods have usable published quotes; differentiated goods require search and matching.",
    "- Do not mention or cite any index or the name of any paper; just apply the definitions above.",
    .sep = "\n"
  )
  
  guidance_full <- glue::glue(
    "Decision order:",
    "1) Exchange test ({ref_year}) → w",
    "2) Reference-price test ({ref_year}) → r",
    "3) Else → n",
    "",
    "Use the most specific described form or grade (not just the broad heading).",
    "Tie-breaker: when evidence is strong, prefer the more standardized category (w > r > n).",
    "If information is insufficient, make a best guess with lower confidence and add a flag.",
    "",
    "Cues ({ref_year}):",
    "- Exchange (w): LME, CBOT, or NYMEX contracts; recognized exchange grades (for example, LME Grade A copper cathodes, No. 2 yellow corn).",
    "- Reference (r): standardized specifications with widely published {ref_year} spot or benchmark assessments (polymers, many fertilizers, pulp, common steel forms).",
    "- Differentiation (n): brand or model identifiers, customized specifications (apparel and footwear, bespoke machinery or parts, finished consumer goods).",
    .sep = "\n"
  )
  
  examples_full <- glue::glue(
    "Few-shot examples ({ref_year} context; format to imitate):",
    "",
    "Example 1",
    "Input → code_system: SITC, code: 5629, description: \"Fertilizers, n.e.s.\"",
    "Output → {{",
    "  \"code_system\": \"SITC\",",
    "  \"code\": \"5629\",",
    "  \"short_description\": \"Fertilizers, n.e.s.\",",
    "  \"rauch_category\": \"r\",",
    "  \"reasoning\": \"Standardized bulk commodity with widely published benchmark prices in {ref_year} trade journals; not exchange-listed.\",",
    "  \"evidence_type\": \"reference_prices\",",
    "  \"confidence\": 0.93,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 2",
    "Input → code_system: SITC, code: 6851, description: \"Lead and lead alloys, unwrought\"",
    "Output → {{",
    "  \"code_system\": \"SITC\",",
    "  \"code\": \"6851\",",
    "  \"short_description\": \"Unwrought lead\",",
    "  \"rauch_category\": \"w\",",
    "  \"reasoning\": \"Unwrought lead has organized-exchange trading (for example, on the LME) with standardized grades and public quotes in {ref_year}.\",",
    "  \"evidence_type\": \"exchange_listing\",",
    "  \"confidence\": 0.92,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 3",
    "Input → code_system: SITC, code: 8510, description: \"Footwear\"",
    "Output → {{",
    "  \"code_system\": \"SITC\",",
    "  \"code\": \"8510\",",
    "  \"short_description\": \"Footwear\",",
    "  \"rauch_category\": \"n\",",
    "  \"reasoning\": \"Branded or differentiated consumer good; no exchange listing and no usable {ref_year} benchmark prices; valuation depends on design, brand, and specifications.\",",
    "  \"evidence_type\": \"differentiation_markers\",",
    "  \"confidence\": 0.90,",
    "  \"flags\": []",
    "}}",
    .sep = "\n"
  )
}

schema_full <- paste(
  "{",
  "\"code_system\": \"SITC|HS|unknown\",",
  "\"code\": \"string or null\",",
  "\"short_description\": \"string\",",
  "\"rauch_category\": \"w|r|n\",",
  "\"reasoning\": \"1–2 sentences citing exchange/reference/differentiation cues\",",
  "\"evidence_type\": \"exchange_listing|reference_prices|differentiation_markers|heuristic_guess\",",
  "\"confidence\": \"number 0.0–1.0\",",
  "\"flags\": [\"optional notes like 'ambiguous_grade','insufficient_detail'\"]",
  "}",
  sep = "\n"
)

# ────────────────────────────────────────────────────────────────────
# 4  Builder
# ────────────────────────────────────────────────────────────────────

build_prompt_full <- function(code, description, wiki) {
  glue::glue(
    "{defs_full}",
    "",
    "{guidance_full}",
    "",
    "{examples_full}",
    "",
    "---",
    "INPUT",
    "code_system: SITC",
    "code: {code}",
    "description: {description}",
    "wikipedia_hint: \"{wiki}\"",
    "",
    "OUTPUT",
    "Return JSON only (one object). Use this schema (types/allowed values shown as strings):",
    "{schema_full}",
    .sep = "\n"
  )
}

# ────────────────────────────────────────────────────────────────────
# 5  Build & save prompts
# ────────────────────────────────────────────────────────────────────

prompts_full <- sitc_dt[, .(
  code      = code,
  indicator = indicator_name,
  model     = "full",
  prompt    = build_prompt_full(code, description, wiki)
)]

out_file <- file.path(prompt_dir, paste0("prompts_sitc2_rauch_full", suffix, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
