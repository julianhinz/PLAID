#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 08_aggregate_models.R
# -------------------------------------------------------------------
# Aggregates Rauch indicator predictions across multiple models:
#   - Plurality label (most frequent category per product)
#   - Uncertainty measures: agreement rate, entropy, n unique labels
#   - Matches against Rauch (1999) ground truth (con & lib)
#
# Usage:
#   Rscript 08_aggregate_models.R [con|lib] [file1.csv file2.csv ...]
#   Rscript 08_aggregate_models.R             # auto-detects all rauch_sitc2_*.csv
#
# Output:
#   output/indicators/rauch_sitc2_aggregated_<stamp>.csv
#   output/metrics/aggregation_report_<stamp>.md
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("stringr",    quietly = TRUE)) install.packages("stringr")
  library(data.table)
  library(stringr)
})

pad4    <- function(x) str_pad(as.character(x), 4, pad = "0")
rnd     <- function(x, k = 3) ifelse(is.finite(x), round(x, k), NA_real_)
esc_md  <- function(x) { x <- gsub("\\|", "\\\\|", x); gsub("\r?\n", "<br>", x) }
md_table <- function(df, digits = 3) {
  df <- as.data.frame(df)
  if (!nrow(df)) return("_(none)_\n")
  num <- vapply(df, is.numeric, logical(1))
  if (any(num)) df[num] <- lapply(df[num], function(z) ifelse(is.finite(z), round(z, digits), z))
  for (j in seq_along(df)) if (is.character(df[[j]]) || is.factor(df[[j]])) df[[j]] <- esc_md(as.character(df[[j]]))
  hdr  <- paste(names(df), collapse = " | ")
  sep  <- paste(rep("---", ncol(df)), collapse = " | ")
  rows <- apply(df, 1, function(r) paste(ifelse(is.na(r), "", r), collapse = " | "))
  paste(c(hdr, sep, rows), collapse = "\n")
}

# Shannon entropy (base 2) over a character vector of labels
label_entropy <- function(labels) {
  labels <- labels[!is.na(labels)]
  if (!length(labels)) return(NA_real_)
  p <- table(labels) / length(labels)
  p <- p[p > 0]
  -sum(p * log2(p))
}

# ── CLI ──────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1 && args[1] %in% c("con", "lib")) {
  truth_col <- args[1]
  files     <- args[-1]
} else {
  truth_col <- "con"
  files     <- args
}

# Find indicator directory: works when run via Rscript from the script dir,
# or sourced from RStudio at project root.
.find_ind_dir <- function() {
  wd <- getwd()
  candidates <- c(
    file.path(wd, "output", "indicators"),
    file.path(wd, "code", "recreate_rauch_1995", "output", "indicators")
  )
  hit <- Filter(dir.exists, candidates)
  if (length(hit)) hit[1] else candidates[1]
}
ind_dir <- .find_ind_dir()

if (!length(files)) {
  all_files <- list.files(ind_dir, pattern = "^rauch_sitc2_.*\\.csv$", full.names = TRUE)
  files <- all_files[!grepl("aggregated", basename(all_files))]
}
if (!length(files)) stop("No model indicator files found in: ", ind_dir)
for (f in files) if (!file.exists(f)) stop("File not found: ", f)

cat(sprintf("Aggregating %d model file(s):\n", length(files)))
for (f in files) cat("  ", f, "\n")
cat(sprintf("Ground truth column: %s\n\n", truth_col))

# ── Extract a short model label from filename ────────────────────────
model_label <- function(path) {
  b <- basename(path)
  m <- str_match(b, "^rauch_sitc2_\\d{8}_([^_]+)_.+\\.csv$")
  if (!is.na(m[1, 2])) m[1, 2] else tools::file_path_sans_ext(b)
}

# ── Read each model file ─────────────────────────────────────────────
read_model <- function(path, tag) {
  DT <- fread(path, showProgress = FALSE)

  # normalise code column name
  code_col <- intersect(c("code4", "code", "sitc4"), names(DT))
  if (!length(code_col)) stop("No code/code4/sitc4 column in: ", path)
  DT[, code4 := pad4(get(code_col[1]))]

  if (!"rauch_category" %in% names(DT)) stop("No 'rauch_category' column in: ", path)
  DT[, pred := tolower(trimws(rauch_category))]

  # optional: confidence
  conf_col <- intersect(c("confidence", "conf", "prob"), names(DT))
  DT[, conf := if (length(conf_col)) suppressWarnings(as.numeric(get(conf_col[1]))) else NA_real_]

  # optional: description (take first model's, used for labelling)
  desc_col <- intersect(c("short_description", "description", "desc",
                           "label", "item", "title", "sitc_label"), names(DT))
  DT[, desc := if (length(desc_col)) as.character(get(desc_col[1])) else NA_character_]

  # one row per code (plurality by confidence, then count)
  D1 <- DT[!is.na(pred) & nzchar(pred) & pred %in% c("w", "r", "n")]
  if (!nrow(D1)) {
    warning("No valid predictions in: ", path)
    return(data.table(code4 = character(), pred = character(),
                      conf = numeric(), desc = character()))
  }
  best <- if (all(is.na(D1$conf))) {
    D1[, .N, by = .(code4, pred, desc)][order(code4, -N)][
      , .SD[1], by = code4][, .(code4, pred, conf = NA_real_, desc)]
  } else {
    D1[order(-ifelse(is.na(conf), -Inf, conf))][
      , .SD[1], by = code4][, .(code4, pred, conf, desc)]
  }

  setnames(best, c("pred", "conf", "desc"),
           c(paste0("pred_",  tag),
             paste0("conf_",  tag),
             paste0("desc_",  tag)))
  best[]
}

tags   <- paste0("M", seq_along(files))
labels <- vapply(files, model_label, character(1))
names(labels) <- tags

model_dts <- Map(read_model, files, tags)
D <- Reduce(function(x, y) merge(x, y, by = "code4", all = TRUE), model_dts)

# take description from first non-NA desc column
desc_cols <- paste0("desc_", tags)
D[, description := {
  vals <- .SD
  apply(vals, 1, function(r) {
    v <- r[!is.na(r) & nzchar(r)]
    if (length(v)) v[1] else NA_character_
  })
}, .SDcols = desc_cols]

# ── Aggregate across models ──────────────────────────────────────────
pred_cols <- paste0("pred_", tags)
conf_cols <- paste0("conf_", tags)

D[, c("plurality_label",
       "n_agree",
       "n_models_present",
       "n_unique_labels",
       "agreement_rate",
       "entropy",
       "unanimous") := {
  preds <- as.matrix(.SD[, pred_cols, with = FALSE])

  plurality  <- character(.N)
  n_agree    <- integer(.N)
  n_present  <- integer(.N)
  n_unique   <- integer(.N)
  agree_rate <- numeric(.N)
  ent        <- numeric(.N)
  unani      <- logical(.N)

  for (i in seq_len(.N)) {
    row_preds <- preds[i, ]
    valid     <- row_preds[!is.na(row_preds) & row_preds %in% c("w", "r", "n")]
    np        <- length(valid)
    n_present[i] <- np
    if (np == 0L) {
      plurality[i]  <- NA_character_
      n_agree[i]    <- 0L
      n_unique[i]   <- 0L
      agree_rate[i] <- NA_real_
      ent[i]        <- NA_real_
      unani[i]      <- FALSE
      next
    }
    tbl           <- sort(table(valid), decreasing = TRUE)
    plurality[i]  <- names(tbl)[1]
    n_agree[i]    <- as.integer(tbl[1])
    n_unique[i]   <- length(tbl)
    agree_rate[i] <- n_agree[i] / np
    p             <- tbl / np
    ent[i]        <- -sum(p * log2(p))
    unani[i]      <- (n_unique[i] == 1L)
  }

  list(plurality, n_agree, n_present, n_unique, agree_rate, ent, unani)
}, .SDcols = c(pred_cols, conf_cols)]

# mean confidence across models
D[, mean_confidence := rowMeans(.SD, na.rm = TRUE), .SDcols = conf_cols]

# ── Rauch ground truth ───────────────────────────────────────────────
base_dir  <- dirname(ind_dir)
rauch_dir <- file.path(base_dir, "temp", "rauch")
dir.create(rauch_dir, showWarnings = FALSE, recursive = TRUE)
rauch_path <- file.path(rauch_dir, "Rauch_classification_revised.csv")
if (!file.exists(rauch_path)) {
  message("Downloading Rauch classification...")
  download.file(
    "https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv",
    destfile = rauch_path, mode = "wb", quiet = TRUE
  )
}
Rauch <- fread(rauch_path, showProgress = FALSE)[,
  .(code4 = pad4(sitc4), con = tolower(con), lib = tolower(lib))
]

D <- merge(D, Rauch, by = "code4", all.x = TRUE)
D[, rauch_truth := ifelse(truth_col == "con", con, lib)]

D[, matches_rauch_con := ifelse(
  !is.na(con) & !is.na(plurality_label), plurality_label == con, NA)]
D[, matches_rauch_lib := ifelse(
  !is.na(lib) & !is.na(plurality_label), plurality_label == lib, NA)]
D[, matches_rauch := ifelse(
  !is.na(rauch_truth) & !is.na(plurality_label),
  plurality_label == rauch_truth, NA)]

# ── Build output table ───────────────────────────────────────────────
out_cols <- c("code4", "description",
              pred_cols,
              "plurality_label", "n_agree", "n_models_present",
              "n_unique_labels", "agreement_rate", "entropy", "unanimous",
              "mean_confidence",
              "con", "lib",
              "matches_rauch_con", "matches_rauch_lib")

out <- D[, intersect(out_cols, names(D)), with = FALSE]
setorder(out, code4)

met_dir <- file.path(base_dir, "output", "metrics")
dir.create(ind_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(met_dir, showWarnings = FALSE, recursive = TRUE)

stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_csv <- file.path(ind_dir, sprintf("rauch_sitc2_aggregated_%s.csv", stamp))
out_md  <- file.path(met_dir, sprintf("aggregation_report_%s.md",     stamp))

fwrite(out, out_csv)
cat("Aggregated CSV written to:", out_csv, "\n")

# ── Summary stats for report ─────────────────────────────────────────
total_N      <- nrow(out)
has_rauch    <- out[!is.na(con)]
N_rauch      <- nrow(has_rauch)
acc_con      <- if (N_rauch) mean(has_rauch$matches_rauch_con, na.rm = TRUE) else NA_real_
acc_lib      <- if (N_rauch) mean(has_rauch$matches_rauch_lib, na.rm = TRUE) else NA_real_
pct_unani    <- mean(out$unanimous,      na.rm = TRUE)
mean_agree   <- mean(out$agreement_rate, na.rm = TRUE)
mean_entropy <- mean(out$entropy,        na.rm = TRUE)

label_dist <- out[!is.na(plurality_label), .N, by = plurality_label][order(-N)]
label_dist[, pct := rnd(N / sum(N) * 100)]

agree_dist <- out[!is.na(agreement_rate),
                  .(count = .N),
                  by = .(agreement_rate = rnd(agreement_rate, 2))
                 ][order(-agreement_rate)]

# ── Markdown report ──────────────────────────────────────────────────
model_meta <- data.table(Tag = tags, Model = labels, File = files)

sink(out_md)
cat("# Aggregated Rauch Indicators — Multi-Model Report\n\n")
cat(sprintf("**Timestamp:** %s  \n", stamp))
cat(sprintf("**Models aggregated:** %d  \n", length(files)))
cat(sprintf("**Ground truth column:** `%s`  \n\n", truth_col))

cat("## Models\n\n")
cat(md_table(model_meta), "\n\n")

cat("---\n\n")
cat("## 1. Coverage\n\n")
cat(md_table(data.table(
  Metric = c("Total products in aggregated file",
             "Products matched with Rauch (1999)"),
  N      = c(total_N, N_rauch)
)), "\n\n")

cat("## 2. Plurality Label Distribution\n\n")
cat(md_table(label_dist), "\n\n")

cat("## 3. Uncertainty Measures\n\n")
cat(md_table(data.table(
  Metric = c("% unanimous (all models agree)",
             "Mean agreement rate",
             "Mean Shannon entropy (bits)"),
  Value  = c(rnd(pct_unani * 100),
             rnd(mean_agree),
             rnd(mean_entropy))
)), "\n\n")

cat("### Agreement rate distribution\n\n")
cat(md_table(agree_dist), "\n\n")

cat("## 4. Accuracy vs Rauch (1999) — Plurality Label\n\n")
cat(md_table(data.table(
  Truth_column = c("con (conservative)", "lib (liberal)"),
  Accuracy     = c(rnd(acc_con), rnd(acc_lib)),
  N            = c(N_rauch, N_rauch)
)), "\n\n")

if (N_rauch > 0) {
  cat("### Accuracy by Rauch category (`con`)\n\n")
  cat(md_table(
    has_rauch[!is.na(matches_rauch_con),
              .(N = .N,
                Correct  = sum(matches_rauch_con),
                Accuracy = rnd(mean(matches_rauch_con))),
              by = .(Rauch_con = con)][order(Rauch_con)]
  ), "\n\n")
}

cat("## 5. High-Uncertainty Products (entropy > 1 bit)\n\n")
high_unc <- out[!is.na(entropy) & entropy > 1][order(-entropy)][
  1:min(.N, 30),
  .(code4,
    description     = substr(description, 1, 80),
    .SD,
    plurality_label,
    entropy         = rnd(entropy),
    agreement_rate  = rnd(agreement_rate),
    con,
    matches_rauch_con),
  .SDcols = pred_cols
]
if (nrow(high_unc)) cat(md_table(high_unc), "\n\n") else cat("_(none)_\n\n")

cat(sprintf("\n**Output CSV:** `%s`\n", basename(out_csv)))
cat("_Report generated by 08_aggregate_models.R_\n")
sink()

cat("Report written to:", out_md, "\n")
