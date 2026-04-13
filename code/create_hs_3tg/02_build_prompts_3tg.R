#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompts_3tg.R
# -------------------------------------------------------------------
# Builds prompts for 3TG conflict minerals classification on HS-6 items.
# Usage examples:
#   Rscript 02_build_prompts_3tg.R              # default H5
#   Rscript 02_build_prompts_3tg.R H6           # specify HS version
# Output: temp/prompts/prompts_hs6_3tg_<HS_VER>.csv
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
  "You are an expert on mineral supply chains, conflict minerals regulation, and product composition.",
  "",
  "Task:",
  "For the given HS-6 item, determine whether the product IS or CONTAINS tin, tantalum, tungsten, or gold",
  "(collectively known as 3TG or conflict minerals), and if so, identify which specific mineral it relates to.",
  "",
  "Key definitions:",
  "- conflict_mineral: true if the product is, contains, or is primarily derived from tin, tantalum,",
  "  tungsten, or gold. This includes ores, concentrates, refined metals, alloys, and manufactured",
  "  products where these minerals are a defining material input.",
  "  Regulated under EU Regulation 2017/821 and US Dodd-Frank Act Section 1502.",
  "- specific_mineral: which of the four 3TG minerals the product primarily relates to.",
  "  Values: 'tin', 'tantalum', 'tungsten', 'gold', or 'none' if not a conflict mineral.",
  "  If a product could relate to multiple 3TG minerals, pick the primary or most prominent one.",
  "",
  "Important:",
  "- Focus on whether the HS-6 description implies the presence or use of a 3TG mineral as a",
  "  key input or primary material, not just an incidental trace amount.",
  "- Generic categories (e.g., 'electrical machinery') should be classified false unless the",
  "  description specifically identifies a 3TG mineral.",
  "- Include downstream products where a 3TG mineral is definitionally present (e.g., tin-plated",
  "  steel, tungsten carbide tools, gold jewellery).",
  "- Do NOT classify products merely because they might coincidentally use a 3TG mineral in production.",
  .sep = "\n"
)

guidance_full <- glue::glue(
  "Coverage guidance:",
  "- Tin: includes tin ores, tin metal, tin alloys (solder, pewter, bronze), tin compounds, tin-plated products.",
  "- Tantalum: includes tantalite ores, tantalum metal, tantalum capacitors, tantalum carbide.",
  "- Tungsten: includes wolframite/scheelite ores, tungsten metal, tungsten carbide, tungsten filaments, tungsten alloys.",
  "- Gold: includes gold ores, gold metal (monetary and non-monetary), gold alloys, gold jewellery, gold-plated items,",
  "  gold compounds.",
  "",
  "When to classify as true:",
  "- Ores and concentrates containing a 3TG mineral.",
  "- Pure metal forms (unwrought, wrought, powder, waste/scrap).",
  "- Alloys where a 3TG mineral is a named or definitional component.",
  "- Manufactured articles where the product description explicitly identifies a 3TG material.",
  "- Compounds and chemical preparations of a 3TG element.",
  "",
  "When to classify as false:",
  "- Generic product categories with no specific mention of a 3TG mineral.",
  "- Products from other minerals or materials (iron, copper, aluminium, etc.).",
  "- Agricultural goods, textiles, plastics, and other clearly non-mineral products.",
  "",
  "Return JSON only. No extra text outside JSON.",
  .sep = "\n"
)

examples_full <- glue::glue(
  "Few-shot examples (format to imitate):",
  "",
  "Example 1",
  "Input → code_system: HS, code: 261590, description: \"Niobium, tantalum or vanadium ores and concentrates\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"261590\",",
  "  \"short_description\": \"Tantalum ores and concentrates\",",
  "  \"conflict_mineral\": true,",
  "  \"specific_mineral\": \"tantalum\",",
  "  \"reasoning\": \"Tantalite ore is the primary source of tantalum, a regulated 3TG conflict mineral under EU 2017/821 and US Dodd-Frank Section 1502.\",",
  "  \"confidence\": 0.95",
  "}}",
  "",
  "Example 2",
  "Input → code_system: HS, code: 800110, description: \"Tin, not alloyed, unwrought\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"800110\",",
  "  \"short_description\": \"Unwrought tin, not alloyed\",",
  "  \"conflict_mineral\": true,",
  "  \"specific_mineral\": \"tin\",",
  "  \"reasoning\": \"Pure unwrought tin is a direct form of a 3TG conflict mineral regulated under EU 2017/821 and US Dodd-Frank Section 1502.\",",
  "  \"confidence\": 0.99",
  "}}",
  "",
  "Example 3",
  "Input → code_system: HS, code: 710812, description: \"Gold, non-monetary, unwrought (excl. powder)\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"710812\",",
  "  \"short_description\": \"Non-monetary unwrought gold\",",
  "  \"conflict_mineral\": true,",
  "  \"specific_mineral\": \"gold\",",
  "  \"reasoning\": \"Non-monetary unwrought gold is a direct form of the 3TG conflict mineral gold, regulated under EU 2017/821 and US Dodd-Frank Section 1502.\",",
  "  \"confidence\": 0.99",
  "}}",
  "",
  "Example 4",
  "Input → code_system: HS, code: 261100, description: \"Tungsten ores and concentrates\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"261100\",",
  "  \"short_description\": \"Tungsten ores and concentrates\",",
  "  \"conflict_mineral\": true,",
  "  \"specific_mineral\": \"tungsten\",",
  "  \"reasoning\": \"Tungsten ores (wolframite, scheelite) are the primary source of tungsten, a regulated 3TG conflict mineral under EU 2017/821 and US Dodd-Frank Section 1502.\",",
  "  \"confidence\": 0.99",
  "}}",
  "",
  "Example 5",
  "Input → code_system: HS, code: 520100, description: \"Cotton, not carded or combed\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"520100\",",
  "  \"short_description\": \"Raw cotton, not carded or combed\",",
  "  \"conflict_mineral\": false,",
  "  \"specific_mineral\": \"none\",",
  "  \"reasoning\": \"Raw cotton is an agricultural fibre product with no connection to tin, tantalum, tungsten, or gold.\",",
  "  \"confidence\": 0.99",
  "}}",
  .sep = "\n"
)

schema_full <- paste(
  "{",
  "\"code_system\": \"HS\",",
  "\"code\": \"string (6 digits)\",",
  "\"short_description\": \"string\",",
  "\"conflict_mineral\": \"true or false (boolean)\",",
  "\"specific_mineral\": \"tin | tantalum | tungsten | gold | none\",",
  "\"reasoning\": \"1-2 sentences explaining the classification\",",
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
  indicator   = "3tg",
  model       = "full",
  prompt
)]

out_file <- file.path(prompt_dir, paste0("prompts_hs6_3tg_", hs_ver, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
