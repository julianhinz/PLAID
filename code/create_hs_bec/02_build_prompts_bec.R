#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompts_bec.R
# -------------------------------------------------------------------
# Builds prompts for BEC (Broad Economic Categories) classification
# on HS-6 items. Classifies products as capital, intermediate, or
# consumption goods.
# Usage examples:
#   Rscript 02_build_prompts_bec.R              # default H5
#   Rscript 02_build_prompts_bec.R H6           # specify HS version
# Output: temp/prompts/prompts_hs6_bec_<HS_VER>.csv
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
# Prompt text
# ────────────────────────────────────────────────────────────────────
defs_full <- glue::glue(
  "You are a trade-economics specialist.",
  "",
  "Task:",
  "For the given HS-6 item, classify its end-use category according to the UN Broad Economic Categories (BEC).",
  "",
  "Categories:",
  "- capital: Goods used in production over multiple periods (machinery, equipment, tools, vehicles used in production).",
  "- intermediate: Goods that are transformed or consumed in the production process (raw materials, parts, components, fuels, semi-finished products).",
  "- consumption: Goods destined for final household use (food for household consumption, clothing, consumer electronics, household appliances).",
  "",
  "Important:",
  "- Focus on the PRIMARY end-use of the typical traded form of the product.",
  "- Some goods have dual uses; classify according to the predominant use in international trade.",
  "- Industrial machinery and equipment → capital.",
  "- Raw materials, ingredients, intermediate inputs → intermediate.",
  "- Finished consumer goods for households → consumption.",
  "",
  "Outputs required:",
  "1) bec: one of capital, intermediate, consumption",
  "2) a short reason grounded in end-use, production chain position, and trade patterns.",
  .sep = "\n"
)

guidance_full <- glue::glue(
  "Classification guidance:",
  "- capital: Equipment with productive life spanning multiple periods.",
  "  Examples: turbines, lathes, tractors (agricultural machinery), aircraft, ships, construction equipment.",
  "- intermediate: Inputs consumed or embodied in production, traded between firms.",
  "  Examples: crude oil, iron ore, cotton fibers, automotive parts, electronic components, chemicals for industry.",
  "- consumption: Final goods purchased by households for direct use.",
  "  Examples: packaged food, beverages, apparel, footwear, consumer electronics, household appliances, pharmaceuticals for retail.",
  "",
  "Tie-breakers:",
  "- If a good is predominantly used as an input in further production, classify as intermediate even if it can also be consumed directly.",
  "- If a good is a finished product sold primarily to households, classify as consumption even if businesses also buy it.",
  "- Capital goods that are also consumer durables (e.g. personal computers) → consumption if primarily household use, capital if primarily business/industrial.",
  "",
  "Return JSON only. No extra text outside JSON.",
  .sep = "\n"
)

examples_full <- glue::glue(
  "Few-shot examples (format to imitate):",
  "",
  "Example 1",
  "Input → code_system: HS, code: 840734, description: \"Spark-ignition reciprocating piston engines of a cylinder capacity exceeding 1,000 cc\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"840734\",",
  "  \"short_description\": \"Engine parts for vehicles\",",
  "  \"bec\": \"intermediate\",",
  "  \"reasoning\": \"Internal combustion engines are components assembled into vehicles; they enter further production rather than being consumed directly by households.\",",
  "  \"confidence\": 0.90",
  "}}",
  "",
  "Example 2",
  "Input → code_system: HS, code: 721391, description: \"Bars and rods of iron or non-alloy steel, hot-rolled, in irregularly wound coils\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"721391\",",
  "  \"short_description\": \"Hot-rolled steel in coils\",",
  "  \"bec\": \"intermediate\",",
  "  \"reasoning\": \"Hot-rolled steel is a semi-finished input used in manufacturing; it is processed further before reaching end consumers.\",",
  "  \"confidence\": 0.95",
  "}}",
  "",
  "Example 3",
  "Input → code_system: HS, code: 610910, description: \"T-shirts, singlets and other vests, of cotton\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"610910\",",
  "  \"short_description\": \"Cotton T-shirts\",",
  "  \"bec\": \"consumption\",",
  "  \"reasoning\": \"Finished cotton garments are final consumer goods purchased directly by households.\",",
  "  \"confidence\": 0.95",
  "}}",
  "",
  "Example 4",
  "Input → code_system: HS, code: 845010, description: \"Household-type washing machines, each of a dry linen capacity not exceeding 10 kg\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"845010\",",
  "  \"short_description\": \"Household washing machines\",",
  "  \"bec\": \"consumption\",",
  "  \"reasoning\": \"Household washing machines are consumer durables purchased by households for personal use, not for productive investment.\",",
  "  \"confidence\": 0.90",
  "}}",
  "",
  "Example 5",
  "Input → code_system: HS, code: 847130, description: \"Portable automatic data processing machines, weighing not more than 10 kg\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"847130\",",
  "  \"short_description\": \"Laptops and portable computers\",",
  "  \"bec\": \"capital\",",
  "  \"reasoning\": \"Portable computers are primarily used as productive capital equipment in offices and businesses, though they also serve household uses.\",",
  "  \"confidence\": 0.75",
  "}}",
  .sep = "\n"
)

schema_full <- paste(
  "{",
  "\"code_system\": \"HS\",",
  "\"code\": \"string (6 digits)\",",
  "\"short_description\": \"string\",",
  "\"bec\": \"capital|intermediate|consumption\",",
  "\"reasoning\": \"1-2 sentences citing end-use, trade chain position\",",
  "\"confidence\": \"number 0.0-1.0\"",
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
  indicator   = "bec",
  model       = "full",
  prompt
)]

out_file <- file.path(prompt_dir, paste0("prompts_hs6_bec_", hs_ver, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
