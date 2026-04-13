#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# fan_out_results.R
# -------------------------------------------------------------------
# Distributes results from a deduplicated LLM run back to per-revision
# indicator CSVs.
#
# Usage:
#   Rscript fan_out_results.R MODEL INDICATOR
#   Rscript fan_out_results.R openai/gpt-5.4-mini rauch
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, readr, digest)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript fan_out_results.R MODEL INDICATOR")
model     <- args[[1]]
indicator <- args[[2]]

safe_model <- gsub("[^A-Za-z0-9]+", "-", model)
today <- format(Sys.Date(), "%Y%m%d")

# Indicator CSV naming convention per pipeline
indicator_prefix <- switch(indicator,
  rauch        = "rauch",
  perishability = "perishability",
  bec          = "bec",
  hazmat       = "hazmat",
  microchip    = "microchip",
  `3tg`        = "3tg",
  stop("Unknown indicator: ", indicator)
)

# Find the deduped indicator CSV (latest by mtime for this model)
out_dir <- file.path("output", "indicators")
pattern <- sprintf("^%s_hs6_.*%s.*deduped.*\\.csv$", indicator_prefix, safe_model)
candidates <- list.files(out_dir, pattern = pattern, full.names = TRUE)
if (length(candidates) == 0) stop("No deduped indicator CSV found matching: ", pattern)
deduped_csv <- candidates[which.max(file.info(candidates)$mtime)]
message("Reading deduped indicators: ", deduped_csv)

# Read deduped indicator results (ensure code is character for joining)
dt <- fread(deduped_csv, colClasses = c(code = "character"))
message(sprintf("Deduped indicator rows: %d", nrow(dt)))

# Read dedup mapping and compute desc_hash for joining
map_file <- file.path("temp", "prompts", sprintf("dedup_map_%s.csv", indicator))
if (!file.exists(map_file)) stop("Dedup mapping not found: ", map_file)
dedup_map <- fread(map_file, colClasses = c(code = "character"))
dedup_map[, desc_hash := substr(sapply(description, digest::digest, algo = "md5"), 1, 8)]
message(sprintf("Dedup map entries: %d", nrow(dedup_map)))

# Join indicators with mapping on (code, desc_hash) to avoid many-to-many on duplicate codes
dt_mapped <- merge(dt, dedup_map[, .(code, desc_hash, source_revisions)],
                   by = c("code", "desc_hash"), all.x = TRUE)

if (any(is.na(dt_mapped$source_revisions))) {
  n_missing <- sum(is.na(dt_mapped$source_revisions))
  warning(sprintf("%d codes in indicator CSV not found in dedup map — skipping", n_missing))
  dt_mapped <- dt_mapped[!is.na(source_revisions)]
}

# Fan out: for each revision, write a separate CSV
revisions <- sort(unique(unlist(strsplit(dt_mapped$source_revisions, ","))))
message(sprintf("Fanning out to %d revisions: %s", length(revisions), paste(revisions, collapse = ", ")))

for (rev in revisions) {
  # Filter to codes that belong to this revision
  rev_dt <- dt_mapped[grepl(rev, source_revisions)]
  rev_dt[, source_revisions := NULL]

  outfile <- file.path(out_dir,
    sprintf("%s_hs6_%s_%s_%s.csv", indicator_prefix, today, safe_model, rev))
  fwrite(rev_dt, outfile)
  message(sprintf("  %s: %d codes -> %s", rev, nrow(rev_dt), outfile))
}

message("Fan-out complete.")
