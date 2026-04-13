#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# deduplicate_prompts.R
# -------------------------------------------------------------------
# Deduplicates prompts across HS revisions for a given indicator.
# Reads all per-revision prompt CSVs, keeps unique (code, description)
# pairs, writes a deduped prompts CSV and a mapping file.
#
# Usage:
#   Rscript deduplicate_prompts.R INDICATOR
#   Rscript deduplicate_prompts.R rauch
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, readr)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript deduplicate_prompts.R INDICATOR")
indicator <- args[[1]]

prompt_dir <- file.path("temp", "prompts")

# Map indicator name to file pattern
# Rauch uses "rauch_full", others use indicator name directly
pattern <- if (indicator == "rauch") {
  "^prompts_hs6_rauch_full_H[0-6]\\.csv$"
} else {
  sprintf("^prompts_hs6_%s_H[0-6]\\.csv$", indicator)
}

files <- list.files(prompt_dir, pattern = pattern, full.names = TRUE)
if (length(files) == 0) stop("No prompts files found matching pattern: ", pattern)

message(sprintf("Found %d revision files for indicator '%s'", length(files), indicator))

# Read all revision files, tag with revision
all_prompts <- rbindlist(lapply(files, function(f) {
  dt <- fread(f, colClasses = "character")
  # Extract HS version from filename (e.g., prompts_hs6_rauch_full_H5.csv -> H5)
  hs_ver <- regmatches(basename(f), regexpr("H[0-6]", basename(f)))
  dt[, hs_ver := hs_ver]
  dt
}))

message(sprintf("Total prompts across all revisions: %d", nrow(all_prompts)))

# Build dedup mapping: for each unique (code, description), list source revisions
dedup_map <- all_prompts[, .(
  source_revisions = paste(sort(unique(hs_ver)), collapse = ",")
), by = .(code, description)]

message(sprintf("Unique (code, description) pairs: %d", nrow(dedup_map)))
message(sprintf("Deduplication savings: %d prompts (%.1f%%)",
                nrow(all_prompts) - nrow(dedup_map),
                (1 - nrow(dedup_map) / nrow(all_prompts)) * 100))

# Keep only first occurrence of each (code, description) pair for the prompts
deduped <- all_prompts[!duplicated(all_prompts, by = c("code", "description"))]
deduped[, hs_ver := NULL]  # Remove revision tag — not needed in prompts

# Write outputs
deduped_file <- file.path(prompt_dir, sprintf("prompts_hs6_%s_deduped.csv",
                          if (indicator == "rauch") "rauch_full" else indicator))
map_file <- file.path(prompt_dir, sprintf("dedup_map_%s.csv", indicator))

fwrite(deduped, deduped_file)
fwrite(dedup_map, map_file)

message(sprintf("Deduped prompts: %s", deduped_file))
message(sprintf("Dedup mapping:   %s", map_file))
