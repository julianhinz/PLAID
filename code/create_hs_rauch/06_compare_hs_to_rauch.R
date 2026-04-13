#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 06_compare_hs_to_rauch.R
# -------------------------------------------------------------------
# Maps HS-based Rauch classifications (w/r/n) to SITC Rev.2 via a
# concordance, then compares against the original Rauch labels.
#
# Usage:
#   Rscript 06_compare_hs_to_rauch.R                     # latest HS file, con truth, default concordance (HS version inferred, SITC Rev.2)
#   Rscript 06_compare_hs_to_rauch.R path/to/hs.csv      # specify HS prediction file
#   Rscript 06_compare_hs_to_rauch.R path/to/hs.csv lib  # use liberal truth labels
#   Rscript 06_compare_hs_to_rauch.R path/to/hs.csv con path/to/concordance.csv [S2|S4] [truth_file.csv]
#
# Inputs:
#   - HS prediction CSV with columns: code/product_code (HS6), rauch_category, optional confidence/short_description.
#   - HS6→SITC concordance CSV (SITC Rev.2 or Rev.4) with HS and SITC code columns.
# Outputs:
#   - Markdown report: output/metrics/hs_vs_rauch_from_hs_<timestamp>.md
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("stringr",   quietly = TRUE)) install.packages("stringr")
  library(data.table); library(stringr)
})

rnd <- function(x, k = 3) ifelse(is.finite(x), round(x, k), NA_real_)
md_table <- function(df, digits = 3, rownames_label = NULL) {
  df <- as.data.frame(df)
  if (!nrow(df)) return("_(none)_\n")
  num <- vapply(df, is.numeric, logical(1))
  if (any(num)) df[num] <- lapply(df[num], function(z) ifelse(is.finite(z), round(z, digits), z))
  esc <- function(x) { x <- gsub("\\|","\\\\|",x); gsub("\r?\n","<br>",x) }
  for (j in seq_along(df)) if (is.character(df[[j]])||is.factor(df[[j]])) df[[j]] <- esc(as.character(df[[j]]))
  if (!is.null(rownames_label) && is.null(df[[rownames_label]])) {
    df <- cbind(setNames(list(rownames(df)), rownames_label), df)
  }
  hdr <- paste(names(df), collapse=" | "); sep <- paste(rep("---", ncol(df)), collapse=" | ")
  rows <- apply(df,1,function(r) paste(ifelse(is.na(r),"",r), collapse=" | "))
  paste(c(hdr,sep,rows), collapse="\n")
}

pad6 <- function(x) stringr::str_pad(as.character(x), 6, pad="0")
pad4 <- function(x) stringr::str_pad(as.character(x), 4, pad="0")

# ---------- CLI ----------
args <- commandArgs(trailingOnly = TRUE)
hs_file <- if (length(args) >= 1) args[1] else NA_character_
truth_col <- if (length(args) >= 2 && args[2] %in% c("con","lib")) args[2] else "con"
concord_file <- if (length(args) >= 3) args[3] else NA_character_
sitc_rev <- if (length(args) >= 4 && toupper(args[4]) %in% c("S2","S4")) toupper(args[4]) else "S2"
truth_file <- if (length(args) >= 5) args[5] else NA_character_

if (is.na(hs_file)) {
  cand <- list.files(file.path("output", "indicators"), pattern = "^rauch_hs6_.*\\.csv$", full.names = TRUE)
  if (length(cand) == 0) stop("No HS prediction files found in output/indicators.")
  hs_file <- cand[which.max(file.info(cand)$mtime)]
}
if (!file.exists(hs_file)) stop("HS file not found: ", hs_file)

if (is.na(concord_file)) {
  # try to infer HS version from filename (e.g., ..._H5-...)
  m <- regmatches(basename(hs_file), regexpr("_H([0-6])", basename(hs_file)))
  hs_ver <- if (length(m) && nchar(m) > 0) paste0("H", substr(m, 3, 3)) else "H5"
  guess <- file.path("temp", "concordance", sprintf("Concordance_%s_to_%s.csv", hs_ver, sitc_rev))
  concord_file <- guess
  if (!file.exists(concord_file)) {
    stop("Could not find concordance file: ", concord_file, "\nProvide path explicitly as 3rd arg.")
  }
}

message("HS file: ", hs_file)
message("Truth column: ", truth_col)
message("SITC revision: ", sitc_rev)
message("Concordance: ", concord_file)
if (!is.na(truth_file)) message("Truth file override: ", truth_file)

# ---------- Load HS predictions ----------
HS <- fread(hs_file, showProgress = FALSE)
grab <- function(DT, cand){ c <- intersect(cand, names(DT)); if (length(c)) DT[[c[1]]] else NULL }

code_col <- if ("code" %in% names(HS)) "code" else if ("product_code" %in% names(HS)) "product_code" else NULL
if (is.null(code_col)) stop("HS file must have a code/product_code column.")
HS[, code6 := pad6(get(code_col))]

pred_col <- if ("rauch_category" %in% names(HS)) "rauch_category" else NULL
if (is.null(pred_col)) stop("HS file missing column 'rauch_category'.")
HS[, pred := tolower(get(pred_col))]

conf <- suppressWarnings(as.numeric(grab(HS, c("confidence","conf","prob"))))
if (is.null(conf)) conf <- NA_real_
HS[, conf := conf]

desc <- grab(HS, c("short_description","description","desc","label","item","title","text"))
if (is.null(desc)) desc <- NA_character_
HS[, desc := as.character(desc)]

# keep one row per HS6 (highest confidence if available)
HS_best <- if (all(is.na(HS$conf))) {
  HS[!is.na(pred) & nzchar(pred)][, .N, by=.(code6, pred)][order(code6, -N)][, .SD[1], by=code6][, .(code6, pred)]
} else {
  HS[!is.na(pred) & nzchar(pred)][order(-ifelse(is.na(conf), -Inf, conf))][, .SD[1], by=code6][, .(code6, pred, conf, desc)]
}
if (!"conf" %in% names(HS_best)) HS_best[, conf := NA_real_]
if (!"desc" %in% names(HS_best)) HS_best[, desc := NA_character_]

# ---------- Load concordance HS6 -> SITC4 ----------
Con <- fread(concord_file, showProgress = FALSE)
find_col <- function(df, patterns) {
  hits <- NULL
  for (p in patterns) {
    hits <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits)) break
  }
  if (!length(hits)) return(NULL)
  hits[1]
}
hs_col   <- find_col(Con, c("^hs\\b", "HS .*Product Code"))
sitc_col <- find_col(Con, c("^sitc\\b", "SITC .*Product Code"))
if (is.null(hs_col) || is.null(sitc_col)) {
  stop("Concordance needs HS and SITC code columns; could not find them in ", concord_file)
}
Con[, hs6 := pad6(get(hs_col))]
# Concordance often carries 5-digit SITC codes; collapse to SITC Rev.2 4-digit by truncation
Con[, sitc_raw := stringr::str_pad(as.character(get(sitc_col)), width = 5, pad = "0")]
Con[, sitc4 := substr(sitc_raw, 1, 4)]
Con <- unique(Con[!is.na(hs6) & !is.na(sitc4)])

# Map HS predictions to SITC4
HS_conc <- merge(Con, HS_best, by.x = "hs6", by.y = "code6", all.x = TRUE)
HS_conc <- HS_conc[!is.na(pred)]
if (nrow(HS_conc) == 0) stop("No HS predictions matched the concordance.")

# Aggregate to SITC4 by confidence-weighted vote
HS_conc[, weight := ifelse(is.na(conf), 1, conf)]
agg <- HS_conc[, .(
  w = sum(weight[pred == "w"], na.rm = TRUE),
  r = sum(weight[pred == "r"], na.rm = TRUE),
  n = sum(weight[pred == "n"], na.rm = TRUE)
), by = sitc4]
agg[, total := w + r + n]
agg[, pred_from_hs := c("w","r","n")[max.col(.SD, ties.method = "first")], .SDcols = c("w","r","n")]
agg[, prob_w := ifelse(total > 0, w / total, NA_real_)]
agg[, prob_r := ifelse(total > 0, r / total, NA_real_)]
agg[, prob_n := ifelse(total > 0, n / total, NA_real_)]
major <- agg[, .(sitc4, pred_from_hs, prob_w, prob_r, prob_n)]

# ---------- Load Rauch truth ----------
dir.create(file.path("temp","rauch"), showWarnings=FALSE, recursive=TRUE)
if (is.na(truth_file)) {
  truth_file <- file.path("temp","rauch","Rauch_classification_revised.csv")
  if (!file.exists(truth_file)) {
    download.file("https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv",
                  destfile = truth_file, mode = "wb", quiet = TRUE)
  }
}
Rauch_raw <- fread(truth_file, showProgress = FALSE)
# detect sitc column name
sitc_truth_col <- if ("sitc4" %in% names(Rauch_raw)) "sitc4" else if ("V1" %in% names(Rauch_raw)) "V1" else names(Rauch_raw)[1]
con_col <- if ("con" %in% names(Rauch_raw)) "con" else if ("V2" %in% names(Rauch_raw)) "V2" else stop("Cannot find con column in truth file.")
lib_col <- if ("lib" %in% names(Rauch_raw)) "lib" else if ("V3" %in% names(Rauch_raw)) "V3" else con_col
Rauch <- Rauch_raw[, .(sitc4 = pad4(get(sitc_truth_col)),
                       con  = tolower(get(con_col)),
                       lib  = tolower(get(lib_col)))]
Rauch[, truth := if (truth_col=="con") con else lib]

# ---------- Evaluation ----------
Eval <- merge(major, Rauch[, .(sitc4, truth)], by = "sitc4", all.x = FALSE, all.y = FALSE)
Eval <- Eval[truth %in% c("w","r","n") & pred_from_hs %in% c("w","r","n")]
if (!nrow(Eval)) stop("No overlapping SITC codes between mapped HS predictions and Rauch truth.")

Eval[, correct := (truth == pred_from_hs)]
acc <- mean(Eval$correct)

conf_counts <- Eval[, .N, by = .(truth, pred_from_hs)]
cats <- c("w","r","n")
conf_counts <- CJ(truth = cats, pred_from_hs = cats)[conf_counts, on = .(truth, pred_from_hs)]
conf_counts[is.na(N), N := 0L]
cm <- dcast(conf_counts, truth ~ pred_from_hs, value.var = "N", fill = 0)
cm_mat <- as.matrix(cm[, -1, with=FALSE]); rownames(cm_mat) <- cm$truth
row_sums <- rowSums(cm_mat); col_sums <- colSums(cm_mat)
cm_row <- sweep(cm_mat, 1, ifelse(row_sums == 0, 1, row_sums), "/")
cm_col <- sweep(cm_mat, 2, ifelse(col_sums == 0, 1, col_sums), "/")

# NOTE: diag(cm_row)/diag(cm_col) returns values in the order of the matrix's
# row/column names, which dcast sorts alphabetically (n, r, w) — NOT the order
# of `cats` (w, r, n). Previously we labelled the diagonals with `cats`, which
# swapped the per-class precision/recall values (n got w's numbers and vice
# versa). Use the actual matrix row/column names instead.
recall_by_class    <- data.table(class = rownames(cm_row),    recall    = diag(cm_row))
precision_by_class <- data.table(class = colnames(cm_col),    precision = diag(cm_col))
f1_by_class <- merge(recall_by_class, precision_by_class, by = "class")[
  , .(class, recall, precision,
      f1 = ifelse(recall + precision == 0, 0, 2 * recall * precision / (recall + precision)))]
# Order rows as (w, r, n) for report readability
f1_by_class <- f1_by_class[match(cats, class)]

macro_tbl <- data.table(
  Metric = c("Macro_Recall", "Macro_Precision", "Macro_F1"),
  Value  = round(c(mean(f1_by_class$recall),
                   mean(f1_by_class$precision),
                   mean(f1_by_class$f1)), 3)
)

# ---------- Output report ----------
out_dir <- file.path("output", "metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
report <- file.path(out_dir, sprintf("hs_vs_rauch_from_hs_%s.md", stamp))
sink(report)

cat("# HS Rauch vs Original Rauch (via HS→SITC mapping)\n\n")
cat(sprintf("**Run timestamp:** %s  \n", stamp))
cat(sprintf("**HS predictions file:** %s  \n", basename(hs_file)))
cat(sprintf("**Concordance file:** %s  \n", basename(concord_file)))
cat(sprintf("**Truth column:** `%s`  \n\n", truth_col))
cat("Aggregation: confidence-weighted votes from HS6 → SITC4 (w=0,r=1,n=2 labels not averaged, just weighted counts).\n\n")

cat("## Overall Accuracy\n\n")
cat(md_table(data.table(Metric = c("Accuracy", "Evaluated (N)"),
                        Value  = c(rnd(acc), sprintf("%d", nrow(Eval))))), "\n\n")

cat("## Confusion Matrix (Counts)\n\n")
cat(md_table(cm), "\n\n")

cat("## Confusion Matrix (Row-normalized = Recall by truth)\n\n")
cat(md_table(as.data.frame(round(cm_row, 3), stringsAsFactors = FALSE), rownames_label = "Truth"), "\n\n")

cat("## Confusion Matrix (Column-normalized = Precision by pred)\n\n")
cm_col_df <- as.data.frame(round(cm_col, 3), stringsAsFactors = FALSE)
rownames(cm_col_df) <- rownames(cm_mat)
cat(md_table(cm_col_df, rownames_label = "Truth"), "\n\n")

cat("## Per-class Precision / Recall / F1\n\n")
pc_tbl <- f1_by_class[, .(Class = class,
                          Precision = rnd(precision),
                          Recall    = rnd(recall),
                          F1        = rnd(f1))]
cat(md_table(pc_tbl), "\n\n")

cat("## Macro Metrics\n\n")
cat(md_table(macro_tbl), "\n\n")

sink()
cat("Wrote report: ", report, "\n", sep = "")

# -------------------------------------------------------------------
# Per-revision cross-revision extension
# -------------------------------------------------------------------
# Re-runs the HS6 → SITC Rev.2 → Rauch comparison for every HS revision
# (H0–H6) using the majority-vote consensus database CSVs, and writes a
# LaTeX table summarising overall agreement by revision. Supports the
# paper's "cross-revision extension" narrative: agreement with Rauch via
# crosswalk should decline modestly in later revisions as the HS
# classification drifts away from Rauch's SITC Rev.2 world.
# -------------------------------------------------------------------

compute_rev_agreement <- function(hs_ver, rauch_truth, db_dir = file.path("output", "database"),
                                  conc_dir = file.path("temp", "concordance")) {
  db_file   <- file.path(db_dir,   sprintf("PLAID_v0.1_rauch_%s.csv.gz", hs_ver))
  conc_file <- file.path(conc_dir, sprintf("Concordance_%s_to_S2.csv", hs_ver))
  if (!file.exists(db_file))   { message("Missing consensus file: ", db_file);   return(NULL) }
  if (!file.exists(conc_file)) { message("Missing concordance: ",    conc_file); return(NULL) }

  DB <- fread(db_file, showProgress = FALSE)
  db_code_col <- if ("hs6_code" %in% names(DB)) "hs6_code" else if ("code" %in% names(DB)) "code" else names(DB)[1]
  db_pred_col <- if ("rauch" %in% names(DB)) "rauch" else if ("rauch_category" %in% names(DB)) "rauch_category" else stop("no rauch col in ", db_file)
  DB_best <- DB[, .(code6 = pad6(get(db_code_col)),
                    pred  = tolower(get(db_pred_col)))]
  DB_best <- DB_best[!is.na(pred) & pred %in% c("w","r","n")]

  Cn <- fread(conc_file, showProgress = FALSE)
  cn_hs_col   <- find_col(Cn, c("^hs\\b", "HS .*Product Code"))
  cn_sitc_col <- find_col(Cn, c("^sitc\\b", "SITC .*Product Code"))
  if (is.null(cn_hs_col) || is.null(cn_sitc_col)) {
    stop("Concordance needs HS/SITC columns: ", conc_file)
  }
  Cn[, hs6 := pad6(get(cn_hs_col))]
  Cn[, sitc_raw := stringr::str_pad(as.character(get(cn_sitc_col)), width = 5, pad = "0")]
  Cn[, sitc4 := substr(sitc_raw, 1, 4)]
  Cn <- unique(Cn[!is.na(hs6) & !is.na(sitc4), .(hs6, sitc4)])

  M <- merge(Cn, DB_best, by.x = "hs6", by.y = "code6", all.x = TRUE)
  M <- M[!is.na(pred)]
  if (!nrow(M)) return(NULL)

  Agg <- M[, .(w = sum(pred == "w"),
               r = sum(pred == "r"),
               n = sum(pred == "n")), by = sitc4]
  Agg[, total := w + r + n]
  Agg[, pred_from_hs := c("w","r","n")[max.col(.SD, ties.method = "first")],
      .SDcols = c("w","r","n")]
  Agg <- Agg[, .(sitc4, pred_from_hs)]

  E <- merge(Agg, rauch_truth, by = "sitc4")
  E <- E[truth %in% c("w","r","n") & pred_from_hs %in% c("w","r","n")]
  if (!nrow(E)) return(NULL)

  list(revision = hs_ver,
       n_matched = nrow(E),
       agreement = mean(E$pred_from_hs == E$truth))
}

rauch_truth_dt <- Rauch[, .(sitc4, truth)]

rev_rows <- rbindlist(lapply(paste0("H", 0:6), function(v) {
  res <- compute_rev_agreement(v, rauch_truth_dt)
  if (is.null(res)) return(NULL)
  data.table(revision = res$revision,
             n_matched = res$n_matched,
             agreement = res$agreement)
}))

if (nrow(rev_rows)) {
  cat("\n\n## Per-revision HS \u2192 SITC Rev.2 agreement vs. Rauch\n\n")
  print(rev_rows[, .(Revision = revision,
                     `N matched` = n_matched,
                     Agreement = rnd(agreement, 3))])

  tbl_dir <- file.path("output", "analysis", "tables")
  dir.create(tbl_dir, recursive = TRUE, showWarnings = FALSE)
  tex_file <- file.path(tbl_dir, "rauch_hs_to_sitc_by_revision.tex")

  tex_lines <- c(
    "% Generated by code/create_hs_rauch/06_compare_hs_to_rauch.R",
    "% HS6 consensus (4-model majority vote) crosswalked to SITC Rev.2",
    "% and compared to Rauch (1999) conservative classification.",
    "\\begin{tabular}{lrr}",
    "\\toprule",
    "Revision & N matched & Agreement \\\\",
    "\\midrule"
  )
  body <- rev_rows[, sprintf("%s & %s & %.3f \\\\",
                             revision,
                             formatC(n_matched, big.mark = ",", format = "d"),
                             agreement)]
  tex_lines <- c(tex_lines, body, "\\bottomrule", "\\end{tabular}")
  writeLines(tex_lines, tex_file)
  cat("Wrote per-revision table: ", tex_file, "\n", sep = "")
} else {
  message("No per-revision rows computed; skipping per-revision table.")
}
