#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_compare_models.R
# -------------------------------------------------------------------
# Compares multiple HS perishability prediction files (no ground truth).
# Computes pairwise agreement rates, weighted kappa, and correlation
# on half_life_days. Perishability classes: 1-5.
# Usage:
#   Rscript 05_compare_models.R <MODEL_1.csv> <MODEL_2.csv> [MODEL_3.csv ...]
# Or without arguments: auto-discovers all perishability_hs6_*.csv files.
# Outputs a Markdown report in output/metrics/.
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("stringr",   quietly = TRUE)) install.packages("stringr")
  library(data.table); library(stringr)
})

pad6 <- function(x) stringr::str_pad(as.character(x), 6, pad = "0")
rnd  <- function(x, k = 3) ifelse(is.finite(x), round(x, k), NA_real_)
esc  <- function(x) { x <- gsub("\\|", "\\\\|", x); gsub("\r?\n", "<br>", x) }

md_table <- function(df, digits = 3) {
  df <- as.data.frame(df)
  if (!nrow(df)) return("_(none)_\n")
  num <- vapply(df, is.numeric, logical(1))
  if (any(num)) df[num] <- lapply(df[num], function(z) ifelse(is.finite(z), round(z, digits), z))
  for (j in seq_along(df)) if (is.character(df[[j]]) || is.factor(df[[j]])) df[[j]] <- esc(as.character(df[[j]]))
  hdr <- paste(names(df), collapse = " | ")
  sep <- paste(rep("---", ncol(df)), collapse = " | ")
  rows <- apply(df, 1, function(r) paste(ifelse(is.na(r), "", r), collapse = " | "))
  paste(c(hdr, sep, rows), collapse = "\n")
}

# Weighted kappa for ordinal scale (1-5)
weighted_kappa <- function(a, b, levels = 1:5) {
  a <- factor(a, levels = levels)
  b <- factor(b, levels = levels)
  tab <- table(a, b)
  N <- sum(tab)
  if (N == 0) return(NA_real_)

  # Quadratic weights
  k <- length(levels)
  w <- matrix(0, k, k)
  for (i in 1:k) for (j in 1:k) w[i, j] <- 1 - ((i - j)^2) / ((k - 1)^2)

  po <- sum(w * tab) / N
  pa <- rowSums(tab) / N
  pb <- colSums(tab) / N
  pe <- sum(outer(pa, pb) * w)

  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  (po - pe) / (1 - pe)
}

# Simple (unweighted) kappa
simple_kappa <- function(a, b, levels = 1:5) {
  a <- factor(a, levels = levels)
  b <- factor(b, levels = levels)
  tab <- table(a, b)
  N <- sum(tab)
  if (N == 0) return(NA_real_)
  po <- sum(diag(tab)) / N
  pa <- rowSums(tab) / N
  pb <- colSums(tab) / N
  pe <- sum(pa * pb)
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  (po - pe) / (1 - pe)
}

extract_model_label <- function(path) {
  base <- basename(path)
  m <- str_match(base, "^perishability_hs6_\\d{8}_([^_]+)_.+\\.csv$")
  if (!is.na(m[1, 2])) return(m[1, 2])
  base
}

grab <- function(DT, cand) {
  c <- intersect(cand, names(DT))
  if (length(c)) DT[[c[1]]] else NULL
}

read_best <- function(path, tag) {
  DT <- fread(path, showProgress = FALSE)

  # normalize code
  if ("code" %in% names(DT)) DT[, code6 := pad6(code)]
  else if ("product_code" %in% names(DT)) DT[, code6 := pad6(product_code)]
  else stop("Missing code/product_code in ", path)

  if (!"perishability_class" %in% names(DT)) stop("Missing 'perishability_class' in ", path)
  DT[, pred := as.integer(perishability_class)]

  # half_life_days
  hl <- grab(DT, c("half_life_days", "half_life"))
  if (is.null(hl)) hl <- NA_real_
  DT[, half_life := as.numeric(hl)]

  conf <- suppressWarnings(as.numeric(grab(DT, c("confidence", "conf", "prob"))))
  if (is.null(conf)) conf <- NA_real_
  DT[, conf := conf]

  desc <- grab(DT, c("short_description", "description", "desc", "label", "item", "title", "text"))
  if (is.null(desc)) desc <- NA_character_
  DT[, desc := as.character(desc)]

  reason <- grab(DT, c("reasoning", "reason", "rationale", "explanation", "analysis", "why"))
  if (is.null(reason)) reason <- NA_character_
  DT[, reason := as.character(reason)]

  evidence <- grab(DT, c("evidence_type", "evidence"))
  if (is.null(evidence)) evidence <- NA_character_
  DT[, evidence := as.character(evidence)]

  D1 <- DT[!is.na(pred) & pred >= 1 & pred <= 5]

  # Take best prediction per code (highest confidence or first)
  best <- if (all(is.na(D1$conf))) {
    M <- D1[, .N, by = .(code6, pred)][order(code6, -N)]
    M[, .SD[1], by = code6][, .(code6, pred)]
  } else {
    D1[order(-ifelse(is.na(conf), -Inf, conf))][, .SD[1], by = code6][, .(code6, pred, half_life, conf, desc, reason, evidence)]
  }

  if (!"conf" %in% names(best)) best[, conf := NA_real_]
  if (!"half_life" %in% names(best)) best[, half_life := NA_real_]
  if (!"desc" %in% names(best)) best[, desc := NA_character_]
  if (!"reason" %in% names(best)) best[, reason := NA_character_]
  if (!"evidence" %in% names(best)) best[, evidence := NA_character_]

  setnames(best, c("pred", "half_life", "conf", "desc", "reason", "evidence"),
           c(paste0("pred_", tag), paste0("hl_", tag), paste0("conf_", tag),
             paste0("desc_", tag), paste0("reason_", tag), paste0("evidence_", tag)))
  best[]
}

# ---------- CLI ----------
args <- commandArgs(trailingOnly = TRUE)

# Auto-discover if no args
if (length(args) == 0) {
  args <- list.files("output/indicators", pattern = "^perishability_hs6_.*\\.csv$", full.names = TRUE)
  args <- args[!str_detect(args, "sample")]
  if (length(args) == 0) {
    stop("No perishability CSV files found in output/indicators/")
  }
  message("Auto-discovered ", length(args), " perishability files")
}

if (length(args) < 1) {
  stop("Usage: Rscript 05_compare_models.R <MODEL_1.csv> [MODEL_2.csv ...]")
}
for (f in args) if (!file.exists(f)) stop("File not found: ", f)

tags <- paste0("M", seq_along(args))

# ---------- Read all models ----------
model_tables <- vector("list", length(args))
names(model_tables) <- tags
for (i in seq_along(args)) {
  model_tables[[i]] <- read_best(args[i], tags[i])
}

D_pred <- Reduce(function(x, y) merge(x, y, by = "code6", all = TRUE), model_tables)

model_labels <- data.table(
  Tag   = tags,
  Model = vapply(args, extract_model_label, character(1)),
  File  = basename(args)
)

valid_classes <- 1:5
class_labels <- c("1" = "Ultra-perishable (1-7d)",
                  "2" = "Highly perishable (8-60d)",
                  "3" = "Moderately perishable (61-360d)",
                  "4" = "Low perishability (1-10y)",
                  "5" = "Non-perishable (>10y)")

# ---------- Single model summary ----------
single_summaries <- list()
for (i in seq_along(tags)) {
  tag <- tags[i]
  pred_col <- paste0("pred_", tag)
  hl_col <- paste0("hl_", tag)
  conf_col <- paste0("conf_", tag)

  dt <- D_pred[!is.na(get(pred_col))]

  class_dist <- dt[, .N, by = .(class = get(pred_col))][order(class)]
  class_dist[, pct := round(100 * N / sum(N), 1)]

  hl_stats <- dt[, .(
    mean_hl = mean(get(hl_col), na.rm = TRUE),
    median_hl = median(get(hl_col), na.rm = TRUE),
    sd_hl = sd(get(hl_col), na.rm = TRUE),
    mean_conf = mean(get(conf_col), na.rm = TRUE)
  )]

  single_summaries[[tag]] <- list(
    model = model_labels[Tag == tag, Model],
    n_codes = nrow(dt),
    class_dist = class_dist,
    hl_stats = hl_stats
  )
}

# ---------- Pairwise agreement ----------
pair_stats <- list()
conf_mats <- list()
disagree_sections <- list()

if (length(tags) >= 2) {
  combs <- t(combn(tags, 2))
  for (k in seq_len(nrow(combs))) {
    t1 <- combs[k, 1]; t2 <- combs[k, 2]
    p1 <- paste0("pred_", t1); p2 <- paste0("pred_", t2)
    hl1 <- paste0("hl_", t1); hl2 <- paste0("hl_", t2)
    c1 <- paste0("conf_", t1); c2 <- paste0("conf_", t2)
    d1 <- paste0("desc_", t1); d2 <- paste0("desc_", t2)
    r1 <- paste0("reason_", t1); r2 <- paste0("reason_", t2)

    Both <- D_pred[!is.na(get(p1)) & get(p1) %in% valid_classes &
                    !is.na(get(p2)) & get(p2) %in% valid_classes,
                  .(code6,
                    A = get(p1), B = get(p2),
                    hl_A = get(hl1), hl_B = get(hl2),
                    conf_A = get(c1), conf_B = get(c2),
                    desc_A = get(d1), desc_B = get(d2),
                    reason_A = get(r1), reason_B = get(r2))]

    if (!nrow(Both)) next

    agree_rate <- mean(Both$A == Both$B)
    agree_within_1 <- mean(abs(Both$A - Both$B) <= 1)
    kap <- simple_kappa(Both$A, Both$B, valid_classes)
    wkap <- weighted_kappa(Both$A, Both$B, valid_classes)

    # Correlation on half-life
    hl_corr <- suppressWarnings(cor(Both$hl_A, Both$hl_B, use = "pairwise.complete.obs"))
    hl_corr_log <- suppressWarnings(cor(log(Both$hl_A + 1), log(Both$hl_B + 1), use = "pairwise.complete.obs"))

    # Class correlation
    class_corr <- suppressWarnings(cor(Both$A, Both$B))

    # Confusion matrix
    conf_counts <- Both[, .N, by = .(A, B)]
    conf_counts <- CJ(A = valid_classes, B = valid_classes)[conf_counts, on = .(A, B)]
    conf_counts[is.na(N), N := 0L]
    cm <- dcast(conf_counts, A ~ B, value.var = "N", fill = 0)
    setnames(cm, "A", paste0(model_labels[Tag == t1, Model]))
    conf_mats[[k]] <- list(
      title = sprintf("%s (rows) vs %s (cols)", model_labels[Tag == t1, Model], model_labels[Tag == t2, Model]),
      table = cm
    )

    pair_stats[[k]] <- data.table(
      Model_1 = model_labels[Tag == t1, Model],
      Model_2 = model_labels[Tag == t2, Model],
      N = nrow(Both),
      Exact_agree = rnd(agree_rate),
      Within_1_class = rnd(agree_within_1),
      Cohen_kappa = rnd(kap),
      Weighted_kappa = rnd(wkap),
      Class_corr = rnd(class_corr),
      HL_corr = rnd(hl_corr),
      HL_log_corr = rnd(hl_corr_log)
    )

    # Disagreement analysis
    Disagree <- Both[A != B]
    if (nrow(Disagree)) {
      Disagree[, diff := abs(A - B)]
      Disagree[, mean_conf := rowMeans(cbind(conf_A, conf_B), na.rm = TRUE)]

      agree_conf <- Both[A == B, mean(rowMeans(cbind(conf_A, conf_B), na.rm = TRUE), na.rm = TRUE)]
      disagree_conf <- mean(Disagree$mean_conf, na.rm = TRUE)

      # Worst disagreements (largest class difference)
      top_disagree <- Disagree[order(-diff, mean_conf)][1:min(.N, 10L),
        .(code6,
          class_1 = A, class_2 = B, diff,
          hl_1 = rnd(hl_A, 0), hl_2 = rnd(hl_B, 0),
          conf_1 = rnd(conf_A), conf_2 = rnd(conf_B),
          desc = substr(ifelse(is.na(desc_A), "", desc_A), 1, 60))
      ]

      disagree_sections[[k]] <- list(
        title = sprintf("%s vs %s", model_labels[Tag == t1, Model], model_labels[Tag == t2, Model]),
        n_disagree = nrow(Disagree),
        mean_diff = rnd(mean(Disagree$diff)),
        agree_conf = rnd(agree_conf),
        disagree_conf = rnd(disagree_conf),
        table = top_disagree
      )
    }
  }
}

pair_table <- if (length(pair_stats)) rbindlist(pair_stats, fill = TRUE) else data.table()

# ---------- Write Markdown ----------
out_dir <- file.path("output", "metrics")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
report <- file.path(out_dir, sprintf("perishability_model_comparison_%s.md", stamp))
sink(report)

cat("# HS Perishability — Model Comparison Report\n\n")
cat(sprintf("**Run timestamp:** %s  \n\n", stamp))

cat("## Models Compared\n\n")
cat(md_table(model_labels[, .(Tag, Model, File)]), "\n\n")
cat("---\n\n")

cat("## Class Definitions\n\n")
cat("| Class | Label | Half-life range |\n")
cat("|-------|-------|----------------|\n")
cat("| 1 | Ultra-perishable | 1-7 days |\n")
cat("| 2 | Highly perishable | 8-60 days |\n")
cat("| 3 | Moderately perishable | 61-360 days |\n")
cat("| 4 | Low perishability | 1-10 years |\n")
cat("| 5 | Non-perishable | >10 years |\n\n")
cat("---\n\n")

cat("## Single Model Summaries\n\n")
for (tag in names(single_summaries)) {
  s <- single_summaries[[tag]]
  cat(sprintf("### %s\n\n", s$model))
  cat(sprintf("- **N codes:** %d\n", s$n_codes))
  cat(sprintf("- **Mean half-life:** %.0f days\n", s$hl_stats$mean_hl))
  cat(sprintf("- **Median half-life:** %.0f days\n", s$hl_stats$median_hl))
  cat(sprintf("- **Mean confidence:** %.3f\n\n", s$hl_stats$mean_conf))
  cat("**Class distribution:**\n\n")
  cat(md_table(s$class_dist), "\n\n")
}
cat("---\n\n")

if (nrow(pair_table)) {
  cat("## Pairwise Agreement\n\n")
  cat(md_table(pair_table), "\n\n")
  cat("---\n\n")
}

if (length(conf_mats)) {
  cat("## Confusion Matrices\n\n")
  for (cm in conf_mats) {
    cat("**", cm$title, "**\n\n", sep = "")
    cat(md_table(cm$table), "\n\n")
  }
  cat("---\n\n")
}

if (length(disagree_sections)) {
  cat("## Disagreement Analysis\n\n")
  for (ds in disagree_sections) {
    cat("### ", ds$title, "\n\n", sep = "")
    cat(sprintf("- **N disagreements:** %d\n", ds$n_disagree))
    cat(sprintf("- **Mean class difference:** %.2f\n", ds$mean_diff))
    cat(sprintf("- **Mean confidence (agreements):** %.3f\n", ds$agree_conf))
    cat(sprintf("- **Mean confidence (disagreements):** %.3f\n\n", ds$disagree_conf))
    cat("**Largest disagreements (up to 10):**\n\n")
    if (nrow(ds$table)) cat(md_table(ds$table), "\n\n") else cat("_(none)_\n\n")
  }
  cat("---\n\n")
}

cat("_Report generated by 05_compare_models.R_\n")
sink()

cat("Wrote report: ", report, "\n", sep = "")
