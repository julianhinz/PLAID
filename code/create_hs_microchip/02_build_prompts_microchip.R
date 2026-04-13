#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompts_microchip.R
# -------------------------------------------------------------------
# Builds prompts for microchip content classification on HS-6 items.
# Usage examples:
#   Rscript 02_build_prompts_microchip.R              # default H5
#   Rscript 02_build_prompts_microchip.R H6           # specify HS version
# Output: temp/prompts/prompts_hs6_microchip_<HS_VER>.csv
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
  "You are an expert on electronics, semiconductor technology, and product composition.",
  "",
  "Task:",
  "For the given HS-6 item, determine whether the product contains, embeds, or IS a",
  "semiconductor, microprocessor, integrated circuit, or microcontroller as a functional component.",
  "",
  "Key concept (microchip_content):",
  "A product has microchip_content = true if:",
  "- It IS a semiconductor component (integrated circuits, processors, microcontrollers, memory chips).",
  "- It is a finished electronic device that functions via embedded semiconductors (laptops, smartphones, TVs, cameras).",
  "- It is a product with embedded control electronics as a core functional part (motor vehicles with ECUs,",
  "  medical devices with microcontrollers, industrial machinery with PLCs, smart appliances).",
  "",
  "A product has microchip_content = false if:",
  "- It contains no semiconductor components as a functional element (raw materials, basic commodities).",
  "- It is purely mechanical, chemical, biological, or textile with no embedded electronics.",
  "- Any electronic connection is incidental rather than functional (e.g., basic electric motors without control circuits).",
  "",
  "Important:",
  "- Focus on the typical traded form described in the HS-6 code.",
  "- When a product category spans both chip-containing and non-chip variants, assess the dominant form.",
  "- 'Contains a wire' or 'uses electricity' is NOT sufficient; the product must contain a semiconductor device.",
  .sep = "\n"
)

guidance_full <- glue::glue(
  "Classification guidance:",
  "- TRUE examples: computers, phones, televisions, cameras, vehicles (post-1980s with ECUs),",
  "  aircraft avionics, medical imaging devices, industrial robots, circuit boards, ICs, memory.",
  "- FALSE examples: raw lumber, steel, cotton textiles, fresh food, furniture, basic hand tools,",
  "  simple electric motors without control circuits, basic light bulbs, wire and cable.",
  "- BORDERLINE: For products where some variants contain chips and others do not, lean toward",
  "  true if the majority of internationally traded units at this HS-6 level contain semiconductors.",
  "",
  "Return JSON only. No extra text outside JSON.",
  .sep = "\n"
)

examples_full <- glue::glue(
  "Few-shot examples (format to imitate):",
  "",
  "Example 1",
  "Input → code_system: HS, code: 847130, description: \"Portable automatic data-processing machines\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"847130\",",
  "  \"short_description\": \"Portable computers (laptops)\",",
  "  \"microchip_content\": true,",
  "  \"reasoning\": \"Laptops are built around processors, memory chips, and other integrated circuits that are central to their function.\",",
  "  \"confidence\": 0.99",
  "}}",
  "",
  "Example 2",
  "Input → code_system: HS, code: 870323, description: \"Motor vehicles, spark-ignition, 1500-3000cc\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"870323\",",
  "  \"short_description\": \"Passenger cars 1500-3000cc\",",
  "  \"microchip_content\": true,",
  "  \"reasoning\": \"Modern passenger vehicles contain multiple ECUs and microcontrollers for engine management, safety systems, and infotainment.\",",
  "  \"confidence\": 0.97",
  "}}",
  "",
  "Example 3",
  "Input → code_system: HS, code: 440710, description: \"Lumber, coniferous, sawn or chipped lengthwise\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"440710\",",
  "  \"short_description\": \"Coniferous sawn lumber\",",
  "  \"microchip_content\": false,",
  "  \"reasoning\": \"Sawn lumber is a raw wood product with no electronic components whatsoever.\",",
  "  \"confidence\": 0.99",
  "}}",
  "",
  "Example 4",
  "Input → code_system: HS, code: 854231, description: \"Electronic integrated circuits: processors and controllers\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"854231\",",
  "  \"short_description\": \"Processors and microcontrollers (ICs)\",",
  "  \"microchip_content\": true,",
  "  \"reasoning\": \"This HS code IS the semiconductor component itself — processors and microcontrollers are integrated circuits by definition.\",",
  "  \"confidence\": 1.00",
  "}}",
  "",
  "Example 5",
  "Input → code_system: HS, code: 610910, description: \"T-shirts of cotton\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"610910\",",
  "  \"short_description\": \"Cotton T-shirts\",",
  "  \"microchip_content\": false,",
  "  \"reasoning\": \"Cotton T-shirts are textile products with no semiconductor or electronic components.\",",
  "  \"confidence\": 0.99",
  "}}",
  .sep = "\n"
)

schema_full <- paste(
  "{",
  "\"code_system\": \"HS\",",
  "\"code\": \"string (6 digits)\",",
  "\"short_description\": \"string\",",
  "\"microchip_content\": \"true or false (boolean)\",",
  "\"reasoning\": \"1-2 sentences explaining whether a semiconductor is a functional component\",",
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
  indicator   = "microchip",
  model       = "full",
  prompt
)]

out_file <- file.path(prompt_dir, paste0("prompts_hs6_microchip_", hs_ver, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
