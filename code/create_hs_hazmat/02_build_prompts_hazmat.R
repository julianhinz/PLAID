#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 02_build_prompts_hazmat.R
# -------------------------------------------------------------------
# Builds prompts for hazardous materials and dual-use classification
# on HS-6 items.
# Usage examples:
#   Rscript 02_build_prompts_hazmat.R              # default H5
#   Rscript 02_build_prompts_hazmat.R H6           # specify HS version
# Output: temp/prompts/prompts_hs6_hazmat_<HS_VER>.csv
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
  "You are an expert on hazardous materials classification and export control regulations.",
  "",
  "Task:",
  "For the given HS-6 item, classify whether it is (a) hazardous and (b) dual-use.",
  "",
  "Key concepts:",
  "",
  "hazardous: The product is classified under the Globally Harmonized System (GHS) of Classification",
  "and Labelling of Chemicals, or is subject to dangerous goods transport regulations (IMDG, IATA-DGR).",
  "This includes explosives, flammable materials, toxic substances, corrosives, radioactive materials,",
  "and oxidizers. Focus on the inherent physical and chemical hazard of the product itself.",
  "",
  "dual_use: The product has legitimate civilian applications but also has potential military,",
  "weapons, or surveillance applications per the Wassenaar Arrangement on Export Controls for",
  "Conventional Arms and Dual-Use Goods and Technologies. This includes certain chemicals,",
  "electronics, precision instruments, encryption technology, nuclear materials, and aerospace",
  "components. A product is dual-use if its specifications, performance characteristics, or",
  "materials could provide meaningful military or weapons development capability.",
  "",
  "Important:",
  "- A product can be both hazardous AND dual-use (e.g., radioactive materials).",
  "- A product can be hazardous but NOT dual-use (e.g., household bleach).",
  "- A product can be dual-use but NOT hazardous (e.g., certain precision optics).",
  "- A product can be neither (e.g., cotton fabric).",
  "- Base the classification on the TYPICAL product fitting the HS-6 description,",
  "  not on rare or exotic uses.",
  .sep = "\n"
)

guidance_full <- glue::glue(
  "Classification guidance:",
  "",
  "For hazardous:",
  "- YES if: the product is a chemical that is toxic, corrosive, explosive, or flammable at",
  "  standard conditions; radioactive material; oxidizing agent; compressed/liquefied gas.",
  "- NO if: the product is an inert solid, fabric, food, wood, or other material without",
  "  inherent chemical/physical hazard properties.",
  "",
  "For dual_use:",
  "- YES if: the product appears on or closely matches items in Wassenaar Arrangement lists,",
  "  Chemical Weapons Convention schedules, Nuclear Suppliers Group lists, or similar multilateral",
  "  export control regimes. Key categories: advanced materials, electronics above certain thresholds,",
  "  computers, telecommunications, sensors/lasers, navigation, marine, aerospace, propulsion.",
  "- NO if: the product is a common consumer good, basic commodity, food, textile, or simple",
  "  manufactured good with no meaningful military application.",
  "",
  "Return JSON only. No extra text outside JSON.",
  .sep = "\n"
)

examples_full <- glue::glue(
  "Few-shot examples (format to imitate):",
  "",
  "Example 1",
  "Input → code_system: HS, code: 281511, description: \"Sodium hydroxide (caustic soda), solid\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"281511\",",
  "  \"short_description\": \"Sodium hydroxide (caustic soda), solid\",",
  "  \"hazardous\": true,",
  "  \"dual_use\": false,",
  "  \"reasoning\": \"Sodium hydroxide is a corrosive substance classified under GHS and subject to IMDG dangerous goods regulations; it has no meaningful weapons or military application.\",",
  "  \"confidence\": 0.95",
  "}}",
  "",
  "Example 2",
  "Input → code_system: HS, code: 901380, description: \"Liquid crystal devices; lasers; other optical appliances\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"901380\",",
  "  \"short_description\": \"Liquid crystal devices; lasers\",",
  "  \"hazardous\": false,",
  "  \"dual_use\": true,",
  "  \"reasoning\": \"Lasers with certain power thresholds and wavelengths are controlled under the Wassenaar Arrangement for potential weapons-targeting, rangefinding, and directed-energy applications; liquid crystal devices are generally not inherently hazardous.\",",
  "  \"confidence\": 0.80",
  "}}",
  "",
  "Example 3",
  "Input → code_system: HS, code: 520100, description: \"Cotton, not carded or combed\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"520100\",",
  "  \"short_description\": \"Cotton, not carded or combed\",",
  "  \"hazardous\": false,",
  "  \"dual_use\": false,",
  "  \"reasoning\": \"Raw cotton is an agricultural commodity with no inherent chemical hazard and no weapons or military application.\",",
  "  \"confidence\": 0.98",
  "}}",
  "",
  "Example 4",
  "Input → code_system: HS, code: 284440, description: \"Radioactive chemical elements and isotopes\"",
  "Output → {{",
  "  \"code_system\": \"HS\",",
  "  \"code\": \"284440\",",
  "  \"short_description\": \"Radioactive chemical elements and isotopes\",",
  "  \"hazardous\": true,",
  "  \"dual_use\": true,",
  "  \"reasoning\": \"Radioactive elements pose radiation hazards classified under GHS and IMDG; they are also controlled under the Nuclear Suppliers Group guidelines for potential nuclear weapons applications.\",",
  "  \"confidence\": 0.97",
  "}}",
  .sep = "\n"
)

schema_full <- paste(
  "{",
  "\"code_system\": \"HS\",",
  "\"code\": \"string (6 digits)\",",
  "\"short_description\": \"string\",",
  "\"hazardous\": true|false,",
  "\"dual_use\": true|false,",
  "\"reasoning\": \"1-2 sentences grounded in specific regulatory frameworks or hazard properties\",",
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
  indicator   = "hazmat",
  model       = "full",
  prompt
)]

out_file <- file.path(prompt_dir, paste0("prompts_hs6_hazmat_", hs_ver, ".csv"))
fwrite(prompts_full, out_file)

cat("Saved:", out_file, "\n")
