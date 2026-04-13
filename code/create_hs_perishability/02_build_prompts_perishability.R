#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompts_perishability.R
# -------------------------------------------------------------------
# Builds prompts for perishability classification on HS-6 items.
# Usage examples:
#   Rscript 02_build_prompts_perishability.R              # default H5
#   Rscript 02_build_prompts_perishability.R H6           # specify HS version
# Output: temp/prompts/prompts_hs6_perishability_<HS_VER>.csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, glue, stringr, readr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ────────────────────────────────────────────────────────────────────
# CLI parsing
# ────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

is_hsver <- function(x) grepl("^H[0-6]$", toupper(x))

hs_ver <- "H5"

if (length(args) >= 1) {
  a1 <- args[[1]]
  if (is_hsver(a1)) hs_ver <- toupper(a1)
}

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
# Prompt text (single variant)
# ────────────────────────────────────────────────────────────────────
defs_full <- glue::glue(
  "You are a trade-economics specialist.",
  "",
  "Task:",
  "For the given HS-6 item, classify its economic perishability and estimate an approximate value half-life.",
  "",
  "Key concept (economic perishability):",
  "How quickly the good loses economic value over time due purely to time passing,",
  "even under standard commercial storage and handling for internationally traded goods.",
  "",
  "This includes (when relevant):",
  "- Physical spoilage or biological decay (food, flowers).",
  "- Chemical instability or sterility loss (some pharmaceuticals/biologics).",
  "- Regulatory expiry or shelf-life constraints.",
  "- Seasonal/fashion obsolescence (apparel, seasonal items).",
  "- Technological/model obsolescence (electronics).",
  "",
  "Important:",
  "- Do NOT equate perishability with 'food' or HS chapter.",
  "- Assume the typical traded form consistent with the HS-6 description (e.g., fresh vs frozen; live vs processed).",
  "- All HS goods are storable in principle; your job is to assess how costly time is for this product.",
  "",
  "Outputs required:",
  "1) perishability_class: integer in {{1,2,3,4,5}}",
  "2) half_life_days: a single numeric estimate of the value half-life in DAYS (not hours),",
  "3) a short reason grounded in shelf-life / obsolescence / time sensitivity cues.",
  .sep = "\n"
)

guidance_full <- glue::glue(
  "Perishability classes (anchor ranges by value half-life in DAYS):",
  "- 1 = Ultra-perishable: 1–7 days (time sensitivity dominates; a few days can destroy value).",
  "- 2 = Highly perishable: 8–60 days (days–weeks matter materially; cold chain / expiry common).",
  "- 3 = Moderately perishable: 61–360 days (months; depreciation/obsolescence within a year).",
  "- 4 = Low perishability: 361–3600 days (1–10 years; slow depreciation).",
  "- 5 = Non-perishable: >3600 days (>10 years; time largely irrelevant).",
  "",
  "How to estimate half_life_days:",
  "- Provide a best-guess point estimate consistent with the class anchors.",
  "- Think 'economic value half-life': time until the typical market value is about 50% lower due to time alone.",
  "- For physical perishables: use typical commercial shelf life under proper storage (e.g., cold chain).",
  "- For fashion/tech: use typical product-cycle depreciation (model turnover, seasonal markdowns).",
  "",
  "Tie-breakers:",
  "- If the description suggests strict freshness/cold-chain or very short shelf life, move toward lower class (more perishable).",
  "- If the good is a durable input (metals, ores, basic chemicals) or a long-lived capital good, move toward higher class.",
  "- If insufficient detail (e.g., unclear fresh vs processed), choose class 3 with lower confidence and add a flag.",
  "",
  "Return JSON only. No extra text outside JSON.",
  .sep = "\n"
)

examples_full <- glue::glue(
  "Few-shot examples (format to imitate):",
  "",
  "Example 1",
  "Input → code_system: HS, code: 030289, description: \"Fish, fresh or chilled (excluding fillets)\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"030289\",",
  "  \"short_description\": \"Fresh or chilled fish\",",
  "  \"perishability_class\": 1,",
  "  \"half_life_days\": 3,",
  "  \"reasoning\": \"Fresh/chilled fish loses market value within days even under cold chain; border delays are very costly.\",",
  "  \"evidence_type\": \"physical_spoilage\",",
  "  \"confidence\": 0.90,",
  "  \"flags\": []",
  "}}",
  "",
  "Example 2",
  "Input → code_system: HS, code: 610910, description: \"T-shirts of cotton\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"610910\",",
  "  \"short_description\": \"Cotton T-shirts\",",
  "  \"perishability_class\": 3,",
  "  \"half_life_days\": 180,",
  "  \"reasoning\": \"Apparel is economically perishable via seasonality/fashion and retail markdown cycles; value decays over months.\",",
  "  \"evidence_type\": \"seasonal_fashion_obsolescence\",",
  "  \"confidence\": 0.75,",
  "  \"flags\": []",
  "}}",
  "",
  "Example 3",
  "Input → code_system: HS, code: 720711, description: \"Semi-finished products of iron or non-alloy steel\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"720711\",",
  "  \"short_description\": \"Semi-finished steel products\",",
  "  \"perishability_class\": 5,",
  "  \"half_life_days\": 20000,",
  "  \"reasoning\": \"Basic steel forms do not become obsolete quickly and can be stored for years with minimal economic decay; time is largely irrelevant.\",",
  "  \"evidence_type\": \"durable_storable_input\",",
  "  \"confidence\": 0.85,",
  "  \"flags\": []",
  "}}",
  .sep = "\n"
)

schema_full <- paste(
  "{",
  "\"code_system\": \"HS\",",
  "\"code\": \"string (6 digits)\",",
  "\"short_description\": \"string\",",
  "\"perishability_class\": \"integer 1–5\",",
  "\"half_life_days\": \"number (single point estimate in days)\",",
  "\"reasoning\": \"1–2 sentences citing spoilage/expiry/seasonality/obsolescence cues\",",
  "\"evidence_type\": \"physical_spoilage|chemical_instability|expiry_regulatory|seasonal_fashion_obsolescence|tech_obsolescence|durable_storable_input|heuristic_guess\",",
  "\"confidence\": \"number 0.0–1.0\",",
  "\"flags\": [\"optional notes like 'ambiguous_form','unclear_fresh_vs_processed','insufficient_detail'\"]",
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
  indicator   = "perishability",
  model       = "full",
  prompt
)]

out_file <- file.path(prompt_dir, paste0("prompts_hs6_perishability_", hs_ver, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
