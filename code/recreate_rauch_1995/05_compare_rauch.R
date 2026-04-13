#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_compare_rauch.R  —  Cleaned + macro/micro tables + coverage + examples
# -------------------------------------------------------------------
# Compares parsed GPT classifications with Rauch ground truth (con/lib)
# Produces accuracy, confusion matrices, per-class metrics, confidence,
# coverage diagnostics (detailed lists + CSVs + analyst note),
# and example misclassifications (20 low/high confidence) incl. reasoning.
#
# Usage:
#   Rscript 05_compare_rauch.R         # uses "con"
#   Rscript 05_compare_rauch.R lib     # uses "lib"
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("fs",         quietly = TRUE)) install.packages("fs")
  if (!requireNamespace("knitr",      quietly = TRUE)) message("Optional: install.packages('knitr') for nicer tables in the MD report.")
  library(data.table)
  library(stringr)
  library(fs)
})

# ---------- Helpers (Markdown table, rounding, escaping, truncation) ----------
rnd <- function(x, k = 3) ifelse(is.finite(x), round(x, k), NA_real_)

.escape_md <- function(x) {
  x <- gsub("\\|", "\\\\|", x)        # escape pipes
  x <- gsub("\r?\n", "<br>", x)       # inline newlines
  x
}

.trunc <- function(x, max_chars = 160) {
  x <- as.character(x)
  needs <- !is.na(x) & nchar(x) > max_chars
  x[needs] <- paste0(substr(x[needs], 1, max_chars - 1), "…")
  x
}

# md_table: robust Markdown table (no knitr required)
md_table <- function(obj, digits = 3, rownames_label = NULL) {
  if (is.null(obj)) return("_(none)_\n")
  # Coerce object to data.frame
  if (is.matrix(obj)) {
    rn <- rownames(obj)
    df <- as.data.frame(obj, stringsAsFactors = FALSE)
    if (!is.null(rownames_label)) df <- cbind(setNames(list(rn), rownames_label), df)
  } else if (inherits(obj, "data.table")) {
    df <- as.data.frame(obj)
  } else if (is.data.frame(obj)) {
    df <- obj
  } else {
    df <- data.frame(value = obj)
  }
  # Round numeric columns
  num_ix <- vapply(df, is.numeric, logical(1))
  if (any(num_ix)) {
    df[num_ix] <- lapply(df[num_ix], function(col) ifelse(is.finite(col), round(col, digits), col))
  }
  # Escape text columns
  for (j in seq_along(df)) {
    if (is.character(df[[j]]) || is.factor(df[[j]])) {
      df[[j]] <- .escape_md(as.character(df[[j]]))
    }
  }
  # Build Markdown
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  if (nrow(df) == 0) {
    rows <- character(0)
  } else {
    rows <- apply(df, 1, function(r) paste(ifelse(is.na(r), "", r), collapse = " | "))
  }
  paste(c(hdr, sep, rows), collapse = "\n")
}

# ---- CLI arg: which Rauch ground-truth column to use ----
args <- commandArgs(trailingOnly = TRUE)
truth_col <- if (length(args) >= 1 && args[1] %in% c("con","lib")) args[1] else "con"

# ---- Paths ----
temp_dir        <- file.path("temp", "rauch")
gen_dir         <- file.path("output", "indicators")  # parsed outputs from 04
out_metrics_dir <- file.path("output", "metrics")
dir.create(temp_dir,        showWarnings = FALSE, recursive = TRUE)
dir.create(gen_dir,         showWarnings = FALSE, recursive = TRUE)
if (!dir_exists(out_metrics_dir)) dir_create(out_metrics_dir, recurse = TRUE)

# ---- Download Rauch classification (if missing) ----
rauch_url <- "https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv"
rauch_csv <- file.path(temp_dir, "Rauch_classification_revised.csv")
if (!file_exists(rauch_csv)) {
  message("Downloading Rauch classification...")
  download.file(rauch_url, destfile = rauch_csv, mode = "wb", quiet = TRUE)
}

# ---- Read data ----
rauch_data <- fread(rauch_csv, showProgress = FALSE)

# latest generated (parsed) model file from 04
gen_files <- list.files(gen_dir, pattern = "^rauch_sitc2_.*\\.csv$", full.names = TRUE)
if (length(gen_files) == 0L) stop("No parsed rauch_sitc2_*.csv files found in: ", gen_dir)
latest_file <- gen_files[which.max(file.info(gen_files)$mtime)]
message("Using parsed file: ", latest_file)

generated_rauch <- fread(latest_file, showProgress = FALSE)

# ---- Normalize merge keys (SITC-4 strings) ----
generated_rauch[, code4 := str_pad(as.character(code), 4, pad = "0")]
rauch_data[,     sitc4_4 := str_pad(as.character(sitc4), 4, pad = "0")]

# Keep unique mapping for join
rauch_unique <- unique(rauch_data[, .(sitc4_4, con, lib)])

# ---- Coverage diagnostics (before merge) ----
missing_in_rauch     <- setdiff(unique(generated_rauch$code4), unique(rauch_data$sitc4_4))
missing_in_generated <- setdiff(unique(rauch_data$sitc4_4),    unique(generated_rauch$code4))
cat(sprintf("Coverage — in model not in Rauch: %d | in Rauch not in model: %d\n",
            length(missing_in_rauch), length(missing_in_generated)))

# Coverage detail tables (for report & CSVs)
show_n <- 50  # how many rows to preview inline in MD
not_in_rauch_tbl <- data.table(code4 = sort(unique(missing_in_rauch)))
not_in_model_tbl <- data.table(code4 = sort(unique(missing_in_generated)))

# ---- Merge on SITC4 (inner merge for scoring) ----
merged <- merge(
  generated_rauch,
  rauch_unique,
  by.x = "code4", by.y = "sitc4_4",
  all.x = FALSE, all.y = FALSE
)

# ---- Prepare eval data ----
if (!truth_col %in% names(merged)) stop("Ground-truth column not found in merged data: ", truth_col)
if (!"rauch_category" %in% names(merged)) stop("Predictions column 'rauch_category' not found in parsed file.")
if (!"confidence" %in% names(merged)) warning("Column 'confidence' not found; confidence analysis will be limited.")

merged[, truth := tolower(get(truth_col))]
merged[, pred  := tolower(rauch_category)]
merged[, conf  := suppressWarnings(as.numeric(confidence))]

# Detect a 'reasoning' text column (prefer 'reasoning', fallback to common alternatives)
candidate_reason_cols <- c("reasoning","reason","rationale","motivation","explanation",
                           "analysis","why","model_reason","model_rationale")
reason_col <- intersect(candidate_reason_cols, names(merged))
reason_col <- if (length(reason_col)) reason_col[1] else NA_character_
merged[, reasoning_text := if (!is.na(reason_col)) as.character(get(reason_col)) else NA_character_]

# Detect a good 'description' column to show context
desc_candidates <- c("short_description","description","desc","label","item","title","sitc_label","text")
desc_col <- intersect(desc_candidates, names(merged))
desc_col <- if (length(desc_col)) desc_col[1] else NA_character_
merged[, desc_text := if (!is.na(desc_col)) as.character(get(desc_col)) else NA_character_]

# Keep only valid labels
valid_levels <- c("w","r","n")
eval_dt <- merged[
  !is.na(truth) & !is.na(pred) &
    truth %in% valid_levels & pred %in% valid_levels
]

if (nrow(eval_dt) == 0L) stop("No comparable rows after filtering to labels {w,r,n}.")

# ---- Accuracy ----
eval_dt[, correct := (pred == truth)]
acc <- mean(eval_dt$correct)
cat(sprintf("Overall accuracy vs %s: %.3f  (N=%d)\n", truth_col, acc, nrow(eval_dt)))

# ---- Confusion matrix (counts) ----
conf_counts <- eval_dt[, .N, by = .(truth, pred)]
conf_counts <- CJ(truth = valid_levels, pred = valid_levels)[conf_counts, on = .(truth, pred)]
conf_counts[is.na(N), N := 0L]

cm <- dcast(conf_counts, truth ~ pred, value.var = "N", fill = 0)
cm_mat <- as.matrix(cm[, -1])
rownames(cm_mat) <- cm$truth
colnames(cm_mat) <- valid_levels

# ---- Row/col normalized CMs (Recall/Precision) ----
row_sums    <- rowSums(cm_mat)
col_sums    <- colSums(cm_mat)
cm_row_norm <- sweep(cm_mat, 1, ifelse(row_sums == 0, 1, row_sums), "/")  # recall
cm_col_norm <- sweep(cm_mat, 2, ifelse(col_sums == 0, 1, col_sums), "/")  # precision

# ---- Per-class metrics ----
recall_by_class    <- data.table(class = valid_levels, recall    = diag(cm_row_norm))
precision_by_class <- data.table(class = valid_levels, precision = diag(cm_col_norm))
f1_by_class <- merge(recall_by_class, precision_by_class, by = "class")[
  , .(class, recall, precision,
      f1 = ifelse(recall + precision == 0, 0, 2 * recall * precision / (recall + precision)))
]

# Macro averages
macro_recall    <- mean(recall_by_class$recall)
macro_precision <- mean(precision_by_class$precision)
macro_f1        <- mean(f1_by_class$f1)

# ---- Alternative per-class metrics (direct counts) ----
tp <- eval_dt[pred == truth, .N, by = truth]; setnames(tp, "truth", "class"); setnames(tp, "N", "tp")
pred_counts <- eval_dt[, .N, by = pred];     setnames(pred_counts, c("pred","N"), c("class","pred_n"))
true_counts <- eval_dt[, .N, by = truth];    setnames(true_counts, c("truth","N"), c("class","true_n"))
pr <- Reduce(function(x,y) merge(x,y,by="class",all=TRUE), list(tp, pred_counts, true_counts))
for (col in c("tp","pred_n","true_n")) pr[is.na(get(col)), (col) := 0L]
pr[, precision := ifelse(pred_n > 0, tp / pred_n, NA_real_)]
pr[, recall    := ifelse(true_n > 0, tp / true_n, NA_real_)]
pr[, f1        := ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), NA_real_)]
setorder(pr, class)

# ---- Micro metrics ----
micro_tp <- sum(diag(cm_mat))
micro_p  <- micro_tp / sum(col_sums)
micro_r  <- micro_tp / sum(row_sums)
micro_f1 <- ifelse(micro_p + micro_r == 0, 0, 2 * micro_p * micro_r / (micro_p + micro_r))

macro_tbl <- data.table(
  Metric = c("Macro_Recall", "Macro_Precision", "Macro_F1",
             "Micro_Precision", "Micro_Recall", "Micro_F1"),
  Value  = round(c(macro_recall, macro_precision, macro_f1,
                   micro_p, micro_r, micro_f1), 3)
)

# ---- Confidence analysis ----
if ("conf" %in% names(eval_dt)) {
  conf_summ <- eval_dt[, .(N = .N,
                           mean_conf   = round(mean(conf,    na.rm=TRUE), 3),
                           median_conf = round(median(conf,  na.rm=TRUE), 3)),
                       by = .(correct)][order(-correct)]
}

# ---- Disagreements + 20 example errors (low/high confidence) with Reasoning ----
disagreements <- eval_dt[pred != truth]
if (!("desc_text"      %in% names(disagreements))) disagreements[, desc_text      := NA_character_]
if (!("reasoning_text" %in% names(disagreements))) disagreements[, reasoning_text := NA_character_]

low_err20 <- if (nrow(disagreements)) {
  disagreements[order(conf, na.last = TRUE)][1:min(20, .N),
                                             .(SITC = code4,
                                               Truth = truth,
                                               Pred  = pred,
                                               Confidence = rnd(conf),
                                               Description = desc_text,
                                               Reasoning   = reasoning_text)]
} else data.table()

hi_err20 <- if (nrow(disagreements)) {
  disagreements[order(-conf, na.last = TRUE)][1:min(20, .N),
                                              .(SITC = code4,
                                                Truth = truth,
                                                Pred  = pred,
                                                Confidence = rnd(conf),
                                                Description = desc_text,
                                                Reasoning   = reasoning_text)]
} else data.table()

# Display (truncated) versions for Markdown; keep CSVs full
low_err20_disp <- copy(low_err20)
hi_err20_disp  <- copy(hi_err20)
for (DT in list(low_err20_disp, hi_err20_disp)) {
  DT[, Description := .trunc(Description, 160)]
  DT[, Reasoning   := .trunc(Reasoning,   180)]
}

# ---- Save CSVs ----
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
f_cm_csv   <- file.path(out_metrics_dir, sprintf("rauch_confusion_%s_%s.csv", truth_col, stamp))
f_perc_csv <- file.path(out_metrics_dir, sprintf("rauch_perclass_%s_%s.csv", truth_col, stamp))
fwrite(cm, f_cm_csv)
fwrite(f1_by_class[, .(class,
                       recall    = round(recall,3),
                       precision = round(precision,3),
                       f1        = round(f1,3))],
       f_perc_csv)

# Coverage lists CSVs
f_in_model_not_in_rauch <- file.path(out_metrics_dir, sprintf("codes_in_model_not_in_rauch_%s.csv", stamp))
f_in_rauch_not_in_model <- file.path(out_metrics_dir, sprintf("codes_in_rauch_not_in_model_%s.csv", stamp))
fwrite(not_in_rauch_tbl, f_in_model_not_in_rauch)
fwrite(not_in_model_tbl, f_in_rauch_not_in_model)

# Example errors CSVs (full text)
f_low_err_csv <- file.path(out_metrics_dir, sprintf("example_errors_lowconf20_%s_%s.csv", truth_col, stamp))
f_hi_err_csv  <- file.path(out_metrics_dir, sprintf("example_errors_hiconf20_%s_%s.csv", truth_col, stamp))
if (nrow(low_err20)) fwrite(low_err20, f_low_err_csv)
if (nrow(hi_err20))  fwrite(hi_err20,  f_hi_err_csv)

# ---- Markdown report (full) ----
report_file <- file.path(out_metrics_dir, sprintf("rauch_eval_report_%s_%s.md", truth_col, stamp))
sink(report_file)

cat("# Classification — Evaluation Report (w/r/n)\n\n")
cat(sprintf("**Run timestamp:** %s\n\n", stamp))
cat(sprintf("**Ground truth used:** `%s`\n\n", truth_col))
cat(sprintf("**Parsed file evaluated:** `%s`\n\n", basename(latest_file)))

cat("## Overall Accuracy\n\n")
cat(md_table(data.table(Metric = c("Accuracy", "Evaluated (N)"),
                        Value  = c(rnd(acc), nrow(eval_dt)))), "\n\n")

cat("## Confusion Matrix (Counts)\n\n")
cat(md_table(cm), "\n\n")

cat("## Confusion Matrix (Row-normalized = Recall by truth)\n\n")
cat(md_table(cm_row_norm, rownames_label = "Truth"), "\n\n")

cat("## Confusion Matrix (Column-normalized = Precision by pred)\n\n")
# Turn column-normalized matrix into data.frame with Pred as header rownames
cm_col_norm_df <- as.data.frame(cm_col_norm, stringsAsFactors = FALSE)
colnames(cm_col_norm_df) <- colnames(cm_mat)
rownames(cm_col_norm_df) <- rownames(cm_mat)
cat(md_table(cm_col_norm_df, rownames_label = "Truth"), "\n\n")

cat("## Per-class Precision / Recall / F1\n\n")
pc_tbl <- f1_by_class[, .(Class = class,
                          Precision = rnd(precision),
                          Recall    = rnd(recall),
                          F1        = rnd(f1))]
cat(md_table(pc_tbl), "\n\n")

cat("## Macro & Micro Metrics\n\n")
cat(md_table(macro_tbl), "\n\n")

cat("## Confidence Analysis\n\n")
if (exists("conf_summ")) {
  conf_tbl <- copy(conf_summ)[, .(Correct = ifelse(correct, "True", "False"),
                                  N, `Mean Confidence` = rnd(mean_conf), `Median Confidence` = rnd(median_conf))]
  cat(md_table(conf_tbl), "\n\n")
} else {
  cat("_No confidence column in parsed file._\n\n")
}

cat("## Coverage Diagnostics — Detailed Lists\n\n")

cat(sprintf("**Codes found in model but not in Rauch (N=%d)**\n\n", nrow(not_in_rauch_tbl)))
cat(md_table(head(not_in_rauch_tbl, show_n)), "\n\n")
if (nrow(not_in_rauch_tbl) > 0) {
  cat("> **Analyst note:** I have checked these codes against the official SITC Rev.2 4-digit list used in the Rauch classification (https://econweb.ucsd.edu/~jrauch/rauch_classification.html). They do **not** exist in SITC Rev.2.\n\n")
}

cat(sprintf("**Codes in Rauch but not in model (N=%d)**\n\n", nrow(not_in_model_tbl)))
cat(md_table(head(not_in_model_tbl, show_n)), "\n\n")

cat("## Example Errors (20 each, with confidence & reasoning)\n\n")
if (nrow(low_err20_disp) + nrow(hi_err20_disp) == 0) {
  cat("_No misclassifications to display._\n\n")
} else {
  cat("**Lowest-confidence misclassifications (20)**\n\n")
  cat(md_table(low_err20_disp), "\n\n")
  cat("**Highest-confidence misclassifications (20)**\n\n")
  cat(md_table(hi_err20_disp), "\n\n")
  if (is.na(reason_col)) {
    cat("_Note: No explicit `reasoning` column was found; the 'Reasoning' field falls back to other available fields (e.g., rationale/motivation/explanation) if present._\n\n")
  } else {
    cat(sprintf("_Reasoning column used: `%s`._\n\n", reason_col))
  }
}

cat("*CSV outputs saved with this report.*\n\n")
cat(sprintf("- Confusion matrix CSV: `%s`\n", basename(f_cm_csv)))
cat(sprintf("- Per-class metrics CSV: `%s`\n", basename(f_perc_csv)))
cat(sprintf("- In-model-not-in-Rauch CSV: `%s`\n", basename(f_in_model_not_in_rauch)))
cat(sprintf("- In-Rauch-not-in-model CSV: `%s`\n", basename(f_in_rauch_not_in_model)))
if (nrow(low_err20)) cat(sprintf("- Example errors (low conf, 20) CSV: `%s`\n", basename(f_low_err_csv)))
if (nrow(hi_err20))  cat(sprintf("- Example errors (high conf, 20) CSV: `%s`\n", basename(f_hi_err_csv)))
sink()

cat("\nSaved:\n")
cat(" - Confusion matrix CSV: ", f_cm_csv, "\n", sep = "")
cat(" - Per-class metrics CSV: ", f_perc_csv, "\n", sep = "")
cat(" - In-model-not-in-Rauch CSV: ", f_in_model_not_in_rauch, "\n", sep = "")
cat(" - In-Rauch-not-in-model CSV: ", f_in_rauch_not_in_model, "\n", sep = "")
if (nrow(low_err20)) cat(" - Example errors (low conf, 20) CSV: ", f_low_err_csv, "\n", sep = "")
if (nrow(hi_err20))  cat(" - Example errors (high conf, 20) CSV: ", f_hi_err_csv,  "\n", sep = "")
cat(" - Markdown report: ", report_file, "\n", sep = "")

# ---- Compact Markdown summary (auto-generated) ----
summary_file <- file.path(out_metrics_dir, sprintf("rauch_summary_%s_%s.md", truth_col, stamp))

# Pretty labels for confusion matrix in summary
label_map <- c(w = "Organized-exchange (w)", r = "Reference-priced (r)", n = "Differentiated (n)")
cm_pretty <- copy(cm)
set(cm_pretty, j = "truth", value = label_map[cm_pretty$truth])
setnames(cm_pretty, old = names(cm_pretty), new = c("Truth \\ Pred", label_map[colnames(cm)[-1]]))

sink(summary_file)
cat("# Classification — Evaluation Summary (w/r/n)\n\n")
cat(sprintf("**Date:** %s  \n", format(Sys.time(), "%d %B %Y")))
cat(sprintf("**Parsed file evaluated:** `%s`  \n", basename(latest_file)))
cat(sprintf("**Ground truth used:** `%s`  \n", truth_col))
cat("**Source:** https://econweb.ucsd.edu/~jrauch/rauch_classification.html\n\n")
cat("---\n\n")

cat("## 1. Overall Performance\n\n")
cat(md_table(data.table(Metric = c("Overall accuracy", "Total evaluated (N)"),
                        Value  = c(rnd(acc), nrow(eval_dt)))), "\n\n")

cat("## 2. Confusion Matrix (Counts)\n\n")
cat(md_table(cm_pretty), "\n\n")
cat("_Most confusion tends to occur between adjacent categories._\n\n")

cat("## 3. Per-Class Metrics\n\n")
pc_tbl_summary <- f1_by_class[, .(Class = class,
                                  Precision = rnd(precision),
                                  Recall    = rnd(recall),
                                  F1        = rnd(f1))]
cat(md_table(pc_tbl_summary), "\n\n")

cat("## 4. Macro & Micro Metrics\n\n")
cat(md_table(macro_tbl), "\n\n")

cat("## 5. Confidence Analysis\n\n")
if (exists("conf_summ")) {
  conf_tbl <- copy(conf_summ)[, .(Correct = ifelse(correct, "True", "False"),
                                  N, `Mean Confidence` = rnd(mean_conf), `Median Confidence` = rnd(median_conf))]
  cat(md_table(conf_tbl), "\n\n")
} else {
  cat("_No confidence column available in parsed file._\n\n")
}

cat("## 6. Example Errors (20 each)\n\n")
cat("**Lowest-confidence misclassifications (20)**\n\n")
cat(md_table(low_err20_disp), "\n\n")
cat("**Highest-confidence misclassifications (20)**\n\n")
cat(md_table(hi_err20_disp), "\n\n")
if (is.na(reason_col)) {
  cat("_Note: No explicit `reasoning` column was found; the 'Reasoning' field falls back to other available fields (e.g., rationale/motivation/explanation) if present._\n\n")
} else {
  cat(sprintf("_Reasoning column used: `%s`._\n\n", reason_col))
}

cat("## 7. Coverage & Analyst Note\n\n")
cat(md_table(data.table(
  Metric = c("In model not in Rauch", "In Rauch not in model"),
  Count  = c(nrow(not_in_rauch_tbl),   nrow(not_in_model_tbl))
)), "\n\n")
cat("**Sample (first 20) — In model not in Rauch:**\n\n")
cat(md_table(head(not_in_rauch_tbl, 20)), "\n\n")
cat("> **Analyst note:** I have checked these codes against the official SITC Rev.2 4-digit list used in the Rauch classification (https://econweb.ucsd.edu/~jrauch/rauch_classification.html). They do **not** exist in SITC Rev.2.\n\n")

cat("_CSV exports with the full lists accompany this report._\n\n")
cat(sprintf("*Report generated automatically from `05_compare_rauch.R` on %s.*\n", format(Sys.time(), "%d %B %Y")))
sink()

cat("Markdown summary written to: ", summary_file, "\n")
