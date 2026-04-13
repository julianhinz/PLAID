#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# build_database.R
# -------------------------------------------------------------------
# Assembles the PLAID database from per-model, per-revision indicator
# CSVs. Produces 42 files (6 indicators × 7 HS revisions), each with
# consensus classifications via majority vote and model agreement stats.
#
# Usage:
#   Rscript code/build_database.R
#
# Output:
#   output/database/PLAID_v0.1_{indicator}_{revision}.csv
#   output/database/README.txt
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)

# ────────────────────────────────────────────────────────────────────
# Configuration
# ────────────────────────────────────────────────────────────────────
VERSION <- "v0.1"
DATE_PATTERN <- "20260330"  # default date stamp for the beta-release CSVs

MODELS <- c(
  "mistralai-mistral-small-2603",
  "openai-gpt-5-4-mini",
  "anthropic-claude-haiku-4-5",
  "google-gemini-2-5-flash"
)

HS_REVISIONS <- paste0("H", 0:6)

indicator_dir <- file.path("output", "indicators")
database_dir  <- file.path("output", "database")
dir.create(database_dir, recursive = TRUE, showWarnings = FALSE)

#' Return the most-recent run date (YYYYMMDD) matching indicator files in the
#' given directory for the named indicator. Used for hazmat after the
#' 2026-04-10 reparse that dropped the dual_use column.
pick_latest_run <- function(indicator, dir = indicator_dir) {
  files <- list.files(dir, pattern = sprintf("^%s_hs6_[0-9]{8}_", indicator))
  dates <- unique(str_extract(files, "(?<=_hs6_)[0-9]{8}"))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) return(NA_character_)
  max(dates)
}

# Per-indicator date overrides. Empty string means "fall back to DATE_PATTERN".
INDICATOR_DATES <- c(
  hazmat = pick_latest_run("hazmat")   # expects 20260410 after reparse
)

# ────────────────────────────────────────────────────────────────────
# Helpers
# ────────────────────────────────────────────────────────────────────

#' Compute the mode; break ties by highest mean confidence
mode_with_tiebreak <- function(values, confidences = NULL) {
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA)
  tab <- table(values)
  max_count <- max(tab)
  candidates <- names(tab[tab == max_count])
  if (length(candidates) == 1) return(candidates[1])
  # Tie — pick candidate with higher mean confidence
  if (!is.null(confidences) && length(confidences) == length(values)) {
    conf_by_val <- tapply(confidences[!is.na(values)], values[!is.na(values)], mean, na.rm = TRUE)
    candidates <- candidates[order(-conf_by_val[candidates])]
  }
  candidates[1]
}

#' Compute share of each level across models
shares <- function(values, levels) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n == 0) return(setNames(rep(NA_real_, length(levels)), levels))
  setNames(vapply(levels, function(l) sum(values == l) / n, numeric(1)), levels)
}

#' Load per-revision CSVs for all models, return list of data.tables
load_model_csvs <- function(indicator_prefix, revision) {
  date_pat <- if (indicator_prefix %in% names(INDICATOR_DATES)) {
    INDICATOR_DATES[[indicator_prefix]]
  } else {
    DATE_PATTERN
  }
  if (is.null(date_pat) || is.na(date_pat) || !nzchar(date_pat)) {
    date_pat <- DATE_PATTERN
  }
  dts <- list()
  for (mod in MODELS) {
    pattern <- sprintf("^%s_hs6_%s_%s_%s\\.csv$",
                       indicator_prefix, date_pat, mod, revision)
    files <- list.files(indicator_dir, pattern = pattern, full.names = TRUE)
    if (length(files) == 0) {
      message(sprintf("  [skip] No file for %s / %s / %s", indicator_prefix, mod, revision))
      next
    }
    dt <- fread(files[1], colClasses = c(code = "character"))
    dt[, code := str_pad(code, 6, pad = "0")]
    dts[[mod]] <- dt
  }
  dts
}

#' Load HS nomenclature descriptions for a revision
load_descriptions <- function(revision) {
  f <- file.path("temp", "hs", paste0(revision, "_Nomenclature.csv"))
  if (!file.exists(f)) { message("  [warn] No nomenclature: ", f); return(NULL) }
  dt <- fread(f, colClasses = c(ProductCode = "character"))
  dt <- dt[Tier == 3, .(hs6_code = str_pad(ProductCode, 6, pad = "0"),
                         description = ProductDescription)]
  unique(dt, by = "hs6_code")
}

# ────────────────────────────────────────────────────────────────────
# Indicator builders
# ────────────────────────────────────────────────────────────────────

build_rauch <- function(revision) {
  dts <- load_model_csvs("rauch", revision)
  if (length(dts) == 0) return(NULL)
  # Get all codes across models
  all_codes <- unique(unlist(lapply(dts, function(d) d$code)))
  result <- data.table(hs6_code = all_codes)
  # Collect values per code
  for (mod in names(dts)) {
    dt <- dts[[mod]][, .(code, val = rauch_category, conf = as.numeric(confidence))]
    setnames(dt, "val", paste0("v_", which(names(dts) == mod)))
    setnames(dt, "conf", paste0("c_", which(names(dts) == mod)))
    result <- merge(result, dt, by.x = "hs6_code", by.y = "code", all.x = TRUE)
  }
  n_models <- length(dts)
  vcols <- paste0("v_", seq_len(n_models))
  ccols <- paste0("c_", seq_len(n_models))
  result[, rauch := apply(.SD, 1, function(r) {
    mode_with_tiebreak(r[vcols], as.numeric(r[ccols]))
  }), .SDcols = c(vcols, ccols)]
  for (level in c("w", "r", "n")) {
    result[, paste0("rauch_share_", level) := apply(.SD, 1, function(r) {
      vals <- as.character(r[!is.na(r)])
      if (length(vals) == 0) NA_real_ else sum(vals == level) / length(vals)
    }), .SDcols = vcols]
  }
  result[, c(vcols, ccols) := NULL]
  result
}

build_perishability <- function(revision) {
  dts <- load_model_csvs("perishability", revision)
  if (length(dts) == 0) return(NULL)
  all_codes <- unique(unlist(lapply(dts, function(d) d$code)))
  result <- data.table(hs6_code = all_codes)
  for (i in seq_along(dts)) {
    dt <- dts[[i]][, .(code,
      pc = as.numeric(perishability_class),
      hl = as.numeric(half_life_days),
      conf = as.numeric(confidence))]
    setnames(dt, c("code", paste0(c("pc_","hl_","c_"), i)))
    result <- merge(result, dt, by.x = "hs6_code", by.y = "code", all.x = TRUE)
  }
  n <- length(dts)
  pc_cols <- paste0("pc_", seq_len(n))
  hl_cols <- paste0("hl_", seq_len(n))
  c_cols  <- paste0("c_", seq_len(n))
  result[, perishability_class := apply(.SD, 1, function(r) {
    vals <- as.character(r[pc_cols])
    confs <- as.numeric(r[c_cols])
    as.integer(round(as.numeric(mode_with_tiebreak(vals[!is.na(vals)], confs[!is.na(vals)]))))
  }), .SDcols = c(pc_cols, c_cols)]
  result[, perishability_class_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = pc_cols]
  result[, perishability_class_sd   := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = pc_cols]
  result[, half_life_days_mean      := rowMeans(.SD, na.rm = TRUE), .SDcols = hl_cols]
  result[, half_life_days_sd        := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = hl_cols]
  result[, c(pc_cols, hl_cols, c_cols) := NULL]
  result
}

build_bec <- function(revision) {
  dts <- load_model_csvs("bec", revision)
  if (length(dts) == 0) return(NULL)
  all_codes <- unique(unlist(lapply(dts, function(d) d$code)))
  result <- data.table(hs6_code = all_codes)
  for (i in seq_along(dts)) {
    dt <- dts[[i]][, .(code, val = bec, conf = as.numeric(confidence))]
    setnames(dt, c("code", paste0("v_", i), paste0("c_", i)))
    result <- merge(result, dt, by.x = "hs6_code", by.y = "code", all.x = TRUE)
  }
  n <- length(dts)
  vcols <- paste0("v_", seq_len(n))
  ccols <- paste0("c_", seq_len(n))
  result[, bec := apply(.SD, 1, function(r) {
    mode_with_tiebreak(r[vcols], as.numeric(r[ccols]))
  }), .SDcols = c(vcols, ccols)]
  for (level in c("capital", "intermediate", "consumption")) {
    result[, paste0("bec_share_", level) := apply(.SD, 1, function(r) {
      vals <- as.character(r[!is.na(r)])
      if (length(vals) == 0) NA_real_ else sum(vals == level) / length(vals)
    }), .SDcols = vcols]
  }
  result[, c(vcols, ccols) := NULL]
  result
}

build_boolean <- function(revision, indicator_prefix, field_name) {
  dts <- load_model_csvs(indicator_prefix, revision)
  if (length(dts) == 0) return(NULL)
  all_codes <- unique(unlist(lapply(dts, function(d) d$code)))
  result <- data.table(hs6_code = all_codes)
  for (i in seq_along(dts)) {
    dt <- dts[[i]][, .(code)]
    val <- dts[[i]][[field_name]]
    # Normalize to numeric 0/1
    val <- fifelse(tolower(as.character(val)) %in% c("true", "1"), 1L, 0L)
    dt[, v := val]
    setnames(dt, "v", paste0("v_", i))
    result <- merge(result, dt, by.x = "hs6_code", by.y = "code", all.x = TRUE)
  }
  n <- length(dts)
  vcols <- paste0("v_", seq_len(n))
  result[, paste0(field_name)        := rowMeans(.SD, na.rm = TRUE) >= 0.5, .SDcols = vcols]
  result[, paste0(field_name, "_mean") := rowMeans(.SD, na.rm = TRUE), .SDcols = vcols]
  result[, paste0(field_name, "_sd")   := apply(.SD, 1, sd, na.rm = TRUE), .SDcols = vcols]
  result[, c(vcols) := NULL]
  result
}

build_hazmat <- function(revision) {
  # Hazmat is a single-field indicator (hazardous) as of the 2026-04-10 reparse.
  # The dual_use field was retired from v0.1; see dev-notes/2026-04-10_*.md.
  build_boolean(revision, "hazmat", "hazardous")
}

build_microchip <- function(revision) {
  build_boolean(revision, "microchip", "microchip_content")
}

build_3tg <- function(revision) {
  dt_cm <- build_boolean(revision, "3tg", "conflict_mineral")
  if (is.null(dt_cm)) return(NULL)
  # Also get specific_mineral mode
  dts <- load_model_csvs("3tg", revision)
  all_codes <- unique(unlist(lapply(dts, function(d) d$code)))
  sm_result <- data.table(hs6_code = all_codes)
  for (i in seq_along(dts)) {
    dt <- dts[[i]][, .(code, sm = specific_mineral)]
    setnames(dt, "sm", paste0("sm_", i))
    sm_result <- merge(sm_result, dt, by.x = "hs6_code", by.y = "code", all.x = TRUE)
  }
  sm_cols <- paste0("sm_", seq_along(dts))
  sm_result[, specific_mineral := apply(.SD, 1, function(r) {
    mode_with_tiebreak(as.character(r))
  }), .SDcols = sm_cols]
  sm_result[, c(sm_cols) := NULL]
  merge(dt_cm, sm_result, by = "hs6_code", all.x = TRUE)
}

# ────────────────────────────────────────────────────────────────────
# Main loop
# ────────────────────────────────────────────────────────────────────
builders <- list(
  rauch         = build_rauch,
  perishability = build_perishability,
  bec           = build_bec,
  hazmat        = build_hazmat,
  microchip     = build_microchip,
  `3tg`         = build_3tg
)

total_files <- 0

for (ind_name in names(builders)) {
  build_fn <- builders[[ind_name]]
  message(sprintf("\n=== %s ===", toupper(ind_name)))

  for (rev in HS_REVISIONS) {
    message(sprintf("  %s:", rev))
    dt <- build_fn(rev)
    if (is.null(dt) || nrow(dt) == 0) {
      message("    [skip] No data.")
      next
    }

    # Join with HS nomenclature for description
    desc_dt <- load_descriptions(rev)
    if (!is.null(desc_dt)) {
      dt <- merge(dt, desc_dt, by = "hs6_code", all.x = TRUE)
      # Move description to second column
      cols <- names(dt)
      setcolorder(dt, c("hs6_code", "description", setdiff(cols, c("hs6_code", "description"))))
    }

    # Sort by code
    setorder(dt, hs6_code)

    # Round numeric columns
    num_cols <- names(dt)[sapply(dt, is.numeric)]
    for (nc in num_cols) dt[, (nc) := round(get(nc), 4)]

    # Write
    outfile <- file.path(database_dir,
      sprintf("PLAID_%s_%s_%s.csv.gz", VERSION, ind_name, rev))
    fwrite(dt, outfile)
    message(sprintf("    -> %s (%d codes, %d cols)", basename(outfile), nrow(dt), ncol(dt)))
    total_files <- total_files + 1
  }
}

# ────────────────────────────────────────────────────────────────────
# README
# ────────────────────────────────────────────────────────────────────
readme_text <- sprintf(
'PLAID: Product-Level AI-Derived Indicators Database
====================================================

Version : %s
Date    : %s
Authors : Carsten Brockhaus, Julian Hinz, Irene Iodice

DESCRIPTION
-----------
This database provides LLM-generated product-level trade indicators
for HS6 (Harmonized System, 6-digit) codes across all 7 HS revisions
(H0/HS92 through H6/HS22).

Each indicator is classified independently by 4 LLMs via OpenRouter.
The released values are consensus classifications: the mode across
models, with per-category shares and standard deviations to quantify
model agreement.

Models used:
  - mistralai/mistral-small-2603
  - openai/gpt-5.4-mini
  - anthropic/claude-haiku-4.5
  - google/gemini-2.5-flash

HS product descriptions sourced from UN STATS:
  https://unstats.un.org/unsd/classifications/Econ/download/In%%20Text/HSCodeandDescription.xlsx

FILES
-----
42 CSV files: PLAID_%s_{indicator}_{revision}.csv

Indicators: rauch, perishability, bec, hazmat, microchip, 3tg
Revisions:  H0 (HS92), H1 (HS96), H2 (HS02), H3 (HS07),
            H4 (HS12), H5 (HS17), H6 (HS22)

COLUMN DEFINITIONS
------------------

All tables share:
  hs6_code       : HS6 product code, zero-padded to 6 digits (string)
  description    : Product description from HS nomenclature

rauch:
  rauch          : Rauch (1999) classification (mode across models)
                     w = organized exchange / homogeneous
                     r = reference priced
                     n = differentiated
  rauch_share_w  : Share of models classifying as w (0-1)
  rauch_share_r  : Share of models classifying as r (0-1)
  rauch_share_n  : Share of models classifying as n (0-1)

perishability:
  perishability_class      : Economic perishability class (mode, 1-5)
                               1 = ultra-perishable (1-7 days)
                               2 = highly perishable (8-60 days)
                               3 = moderately perishable (61-360 days)
                               4 = low perishability (1-10 years)
                               5 = non-perishable (>10 years)
  perishability_class_mean : Mean class across models
  perishability_class_sd   : SD across models
  half_life_days_mean      : Mean estimated half-life in days
  half_life_days_sd        : SD of half-life estimates

bec:
  bec                      : BEC end-use category (mode)
                               capital / intermediate / consumption
  bec_share_capital        : Share classifying as capital (0-1)
  bec_share_intermediate   : Share classifying as intermediate (0-1)
  bec_share_consumption    : Share classifying as consumption (0-1)

hazmat:
  hazardous                : Hazardous material (mode, TRUE/FALSE)
  hazardous_mean           : Share of models saying TRUE (0-1)
  hazardous_sd             : SD across models

microchip:
  microchip_content        : Contains microchip/semiconductor (mode, TRUE/FALSE)
  microchip_content_mean   : Share saying TRUE (0-1)
  microchip_content_sd     : SD across models

3tg:
  conflict_mineral         : Contains 3TG conflict mineral (mode, TRUE/FALSE)
  conflict_mineral_mean    : Share saying TRUE (0-1)
  conflict_mineral_sd      : SD across models
  specific_mineral         : Which mineral (mode): tin/tantalum/tungsten/gold/none

CITATION
--------
If you use this database in your research, please cite:

  Brockhaus, C., Hinz, J., & Iodice, I. (2026).
  PLAID: Product-Level AI-Derived Indicators Database for
  International Trade. [Working paper / dataset].

LICENSE
-------
Creative Commons Attribution 4.0 International (CC BY 4.0)
https://creativecommons.org/licenses/by/4.0/
',
  VERSION, format(Sys.Date(), "%%Y-%%m-%%d"), VERSION
)

writeLines(readme_text, file.path(database_dir, "README.txt"))

message(sprintf("\n============================"))
message(sprintf("PLAID %s database assembled.", VERSION))
message(sprintf("  %d files written to %s", total_files, database_dir))
message(sprintf("  README written."))
message("Done.")
