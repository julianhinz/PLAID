#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompts_rauch.R
# -------------------------------------------------------------------
# Builds prompts for Rauch w/r/n classification on HS-6 items.
# Usage examples:
#   Rscript 02_build_prompts_rauch.R              # classic wording, H5
#   Rscript 02_build_prompts_rauch.R 1995         # year-specific wording (1995), H5
#   Rscript 02_build_prompts_rauch.R H6           # classic wording, H6
#   Rscript 02_build_prompts_rauch.R 1995 H6      # year-specific wording, H6
# Output: temp/prompts/prompts_hs6_rauch_full[_YEAR].csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, glue, stringr, readr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ────────────────────────────────────────────────────────────────────
# CLI parsing
# ────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

is_year <- function(x) grepl("^[0-9]{4}$", x)
is_hsver <- function(x) grepl("^H[0-6]$", toupper(x))

variant  <- "classic"
ref_year <- NA_integer_
hs_ver   <- "H5"

if (length(args) >= 1) {
  a1 <- args[[1]]
  if (identical(a1, "classic")) {
    variant <- "classic"
  } else if (is_year(a1)) {
    variant  <- "year"
    ref_year <- as.integer(a1)
  } else if (is_hsver(a1)) {
    hs_ver <- toupper(a1)
  }
}
if (length(args) >= 2) {
  a2 <- args[[2]]
  if (variant == "classic" && is_year(a2)) {
    variant  <- "year"
    ref_year <- as.integer(a2)
  }
  if (is_hsver(a2)) hs_ver <- toupper(a2)
}

suffix <- if (variant == "classic") "" else paste0("_", ref_year)
message("Prompt variant: ", variant, if (!is.na(ref_year)) paste0(" (", ref_year, ")"))
message("HS version: ", hs_ver)

# ────────────────────────────────────────────────────────────────────
# I/O paths
# ────────────────────────────────────────────────────────────────────
temp_hs_dir <- file.path("temp", "hs")
prompt_dir  <- file.path("temp", "prompts")
dir.create(prompt_dir, recursive = TRUE, showWarnings = FALSE)

# ────────────────────────────────────────────────────────────────────
# Load HS nomenclature (Tier 3 = HS-6)
# ────────────────────────────────────────────────────────────────────
hs_files <- list.files(temp_hs_dir, pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(hs_files) > 0)

hs_full <- rbindlist(lapply(hs_files, fread), fill = TRUE)

if (!"ProductDescription" %in% names(hs_full)) stop("Could not find ProductDescription column in HS data.")
hs_full[, description_raw := ProductDescription]

hs_dt <- hs_full[Tier == 3 & toupper(NomenclatureCode) == hs_ver]
hs_dt[, code := stringr::str_pad(as.character(ProductCode), 6, pad = "0")]
hs_dt[, description := as.character(description_raw)]
hs_dt[, chapter := substr(code, 1, 2)]
hs_dt <- hs_dt[, .(code, description, chapter)]
hs_dt[, description := str_trim(substr(description %||% "", 1, 260))]
hs_dt[is.na(description), description := ""]

chapters <- hs_full[
  Tier == 1 & toupper(NomenclatureCode) == hs_ver,
  .(chapter = stringr::str_pad(as.character(ProductCode), 2, pad = "0"),
    chapter_desc = as.character(description_raw %||% ""))
]
chapters[, chapter_desc := str_trim(substr(chapter_desc %||% "", 1, 240))]
chapters[is.na(chapter_desc), chapter_desc := ""]

hs_dt <- merge(hs_dt, chapters, by = "chapter", all.x = TRUE)
hs_dt[is.na(chapter_desc), chapter_desc := ""]

if (nrow(hs_dt) == 0) stop("No HS-6 rows found for version ", hs_ver)

# ────────────────────────────────────────────────────────────────────
# Prompt text (classic vs year-specific)
# ────────────────────────────────────────────────────────────────────
if (variant == "classic") {
  defs_full <- glue::glue(
    "You are a trade-economics specialist.",
    "",
    "Task:",
    "Classify each item into three categories with labels w, r, and n.",
    "",
    "Definitions:",
    "- w = organized-exchange: homogeneous goods with standardized contracts and public exchange prices.",
    "- r = reference-priced: not exchange-listed, but widely published benchmark prices exist in trade journals or price-reporting agencies.",
    "- n = differentiated: no exchange listing and no widely accepted benchmark prices; valuation depends on brand, design, and specifications.",
    .sep = "\n"
  )

  guidance_full <- glue::glue(
    "Decision order:",
    "1) Exchange test → w",
    "2) Reference-price test → r",
    "3) Else → n",
    "",
    "Use the most specific described grade/form (not just the broad heading).",
    "Tie-breaker: when evidence is strong, prefer the more standardized category (w > r > n).",
    "If information is insufficient, make a best guess with lower confidence and add a flag.",
    "",
    "Cues:",
    "- Exchange (w): exchange contracts such as CBOT/ICE/LME grades (for example, No. 2 yellow corn, LME Grade A copper).",
    "- Reference (r): standardized specs with widely published spot or benchmark assessments (polymers, fertilizers, common steel forms).",
    "- Differentiation (n): brand or model identifiers; consumer goods, apparel, machinery, bespoke parts.",
    .sep = "\n"
  )

  examples_full <- glue::glue(
    "Few-shot examples (format to imitate):",
    "",
    "Example 1",
    "Input → code_system: HS, code: 100590, description: \"Maize (corn)\"",
    "Output → {{",
    "  \"code_system\": \"HS\",",
    "  \"code\": \"100590\",",
    "  \"short_description\": \"Maize (corn)\",",
    "  \"rauch_category\": \"w\",",
    "  \"reasoning\": \"Maize has standardized exchange contracts (e.g., CBOT) with public prices, so it is organized-exchange.\",",
    "  \"evidence_type\": \"exchange_listing\",",
    "  \"confidence\": 0.94,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 2",
    "Input → code_system: HS, code: 390110, description: \"Polyethylene with density < 0.94\"",
    "Output → {{",
    "  \"code_system\": \"HS\",",
    "  \"code\": \"390110\",",
    "  \"short_description\": \"Polyethylene (<0.94 g/cc)\",",
    "  \"rauch_category\": \"r\",",
    "  \"reasoning\": \"Polyethylene trades on standardized specs with widely published benchmark prices but is not exchange-listed, so it is reference-priced.\",",
    "  \"evidence_type\": \"reference_prices\",",
    "  \"confidence\": 0.90,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 3",
    "Input → code_system: HS, code: 847130, description: \"Portable automatic data processing machines (laptops)\"",
    "Output → {{",
    "  \"code_system\": \"HS\",",
    "  \"code\": \"847130\",",
    "  \"short_description\": \"Laptops\",",
    "  \"rauch_category\": \"n\",",
    "  \"reasoning\": \"Laptops are branded and differentiated consumer goods with no exchange or benchmark prices, so they are differentiated.\",",
    "  \"evidence_type\": \"differentiation_markers\",",
    "  \"confidence\": 0.88,",
    "  \"flags\": []",
    "}}",
    .sep = "\n"
  )

} else {
  if (is.na(ref_year)) stop("Year-specific variant selected but ref_year is NA.")

  defs_full <- glue::glue(
    "You are a trade-economics specialist. Temporal context: {ref_year}.",
    "",
    "Task:",
    "Classify each item into three categories with labels w, r, and n, using a {ref_year} context.",
    "",
    "Definitions:",
    "- w = organized-exchange: homogeneous commodities traded on organized exchanges with standardized contracts and public quotes (for example, LME, CBOT, or NYMEX as of {ref_year}).",
    "- r = reference-priced: not exchange-listed, but widely published benchmark prices exist in {ref_year} trade journals or price-reporting publications.",
    "- n = differentiated: no exchange listing and no widely accepted benchmark prices; valuation depends on brand, design, and specifications.",
    "",
    "Notes:",
    "- Use {ref_year} institutions, market structure, and publications; do not rely on post-{ref_year} developments.",
    "- Think in terms of market thickness and arbitrage opportunities in {ref_year}: organized exchanges centralize price information; reference-priced goods have usable published quotes; differentiated goods require search and matching.",
    "- Do not cite any paper names; just apply the definitions above.",
    .sep = "\n"
  )

  guidance_full <- glue::glue(
    "Decision order:",
    "1) Exchange test ({ref_year}) → w",
    "2) Reference-price test ({ref_year}) → r",
    "3) Else → n",
    "",
    "Use the most specific described grade/form (not just the broad heading).",
    "Tie-breaker: when evidence is strong, prefer the more standardized category (w > r > n).",
    "If information is insufficient, make a best guess with lower confidence and add a flag.",
    "",
    "Cues ({ref_year}):",
    "- Exchange (w): LME/CBOT/NYMEX contracts; recognized exchange grades (for example, LME Grade A copper cathodes, No. 2 yellow corn).",
    "- Reference (r): standardized specs with widely published {ref_year} spot or benchmark assessments (polymers, many fertilizers, pulp, common steel forms).",
    "- Differentiation (n): brand or model identifiers; customized machinery, apparel, finished consumer goods.",
    .sep = "\n"
  )

  examples_full <- glue::glue(
    "Few-shot examples ({ref_year} context; format to imitate):",
    "",
    "Example 1",
    "Input → code_system: HS, code: 100590, description: \"Maize (corn)\"",
    "Output → {{",
    "  \"code_system\": \"HS\",",
    "  \"code\": \"100590\",",
    "  \"short_description\": \"Maize (corn)\",",
    "  \"rauch_category\": \"w\",",
    "  \"reasoning\": \"Corn has organized-exchange contracts (e.g., CBOT) with public quotes in {ref_year}, so it is organized-exchange.\",",
    "  \"evidence_type\": \"exchange_listing\",",
    "  \"confidence\": 0.94,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 2",
    "Input → code_system: HS, code: 390110, description: \"Polyethylene with density < 0.94\"",
    "Output → {{",
    "  \"code_system\": \"HS\",",
    "  \"code\": \"390110\",",
    "  \"short_description\": \"Polyethylene (<0.94 g/cc)\",",
    "  \"rauch_category\": \"r\",",
    "  \"reasoning\": \"Polyethylene had widely published benchmark prices in {ref_year} but was not exchange-listed, so it is reference-priced.\",",
    "  \"evidence_type\": \"reference_prices\",",
    "  \"confidence\": 0.90,",
    "  \"flags\": []",
    "}}",
    "",
    "Example 3",
    "Input → code_system: HS, code: 847130, description: \"Portable automatic data processing machines (laptops)\"",
    "Output → {{",
    "  \"code_system\": \"HS\",",
    "  \"code\": \"847130\",",
    "  \"short_description\": \"Laptops\",",
    "  \"rauch_category\": \"n\",",
    "  \"reasoning\": \"Laptops are branded differentiated goods with no usable {ref_year} benchmark prices, so they are differentiated.\",",
    "  \"evidence_type\": \"differentiation_markers\",",
    "  \"confidence\": 0.88,",
    "  \"flags\": []",
    "}}",
    .sep = "\n"
  )
}

schema_full <- paste(
  "{",
  "\"code_system\": \"HS\",",
  "\"code\": \"string (6 digits)\",",
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
# Builder
# ────────────────────────────────────────────────────────────────────
build_prompt_full <- function(code, description, chapter, chapter_desc) {
  chapter_desc <- chapter_desc %||% ""
  description  <- description %||% ""

  chapter_line <- if (nzchar(chapter_desc)) {
    glue::glue("chapter_hint: {chapter} — {chapter_desc}")
  } else {
    glue::glue("chapter_hint: {chapter}")
  }

  glue::glue(
    "{defs_full}",
    "",
    "{guidance_full}",
    "",
    "{examples_full}",
    "",
    "---",
    "INPUT",
    "code_system: HS",
    "code: {code}",
    "description: {description}",
    "{chapter_line}",
    "",
    "OUTPUT",
    "Return JSON only (one object). Use this schema (types/allowed values shown as strings):",
    "{schema_full}",
    .sep = "\n"
  )
}

# ────────────────────────────────────────────────────────────────────
# Build & save prompts
# ────────────────────────────────────────────────────────────────────
hs_dt[, prompt := mapply(
  build_prompt_full,
  code, description, chapter, chapter_desc,
  SIMPLIFY = FALSE
)]

prompts_full <- hs_dt[, .(
  code        = code,
  description = description,
  indicator   = "rauch_wr_n",
  model       = "full",
  prompt
)]

out_file <- file.path(prompt_dir, paste0("prompts_hs6_rauch_full_", hs_ver, suffix, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
