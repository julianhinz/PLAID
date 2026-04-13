#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_compare_models.R
# -------------------------------------------------------------------
# Compares multiple HS Rauch prediction files (no ground truth).
# Computes pairwise agreement rates and Cohen's kappa on intersecting codes.
# Usage:
#   Rscript 05_compare_models.R <MODEL_1.csv> <MODEL_2.csv> [MODEL_3.csv ...]
# Outputs a Markdown report in output/metrics/.
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  if (!requireNamespace("stringr",   quietly = TRUE)) install.packages("stringr")
  library(data.table); library(stringr)
})

pad6 <- function(x) stringr::str_pad(as.character(x), 6, pad = "0")
rnd  <- function(x,k=3) ifelse(is.finite(x), round(x,k), NA_real_)
esc  <- function(x){x <- gsub("\\|","\\\\|",x); gsub("\r?\n","<br>",x)}
md_table <- function(df, digits=3){
  df <- as.data.frame(df)
  if (!nrow(df)) return("_(none)_\n")
  num <- vapply(df, is.numeric, logical(1))
  if (any(num)) df[num] <- lapply(df[num], function(z) ifelse(is.finite(z), round(z, digits), z))
  for (j in seq_along(df)) if (is.character(df[[j]])||is.factor(df[[j]])) df[[j]] <- esc(as.character(df[[j]]))
  hdr <- paste(names(df), collapse=" | "); sep <- paste(rep("---", ncol(df)), collapse=" | ")
  rows <- apply(df,1,function(r) paste(ifelse(is.na(r),"",r), collapse=" | "))
  paste(c(hdr,sep,rows), collapse="\n")
}

kappa <- function(a,b,levels=c("w","r","n")){
  a <- factor(a, levels=levels); b <- factor(b, levels=levels)
  tab <- table(a,b); N <- sum(tab); if (N==0) return(NA_real_)
  po <- sum(diag(tab))/N; pa <- rowSums(tab)/N; pb <- colSums(tab)/N; pe <- sum(pa*pb)
  if (isTRUE(all.equal(pe,1))) return(NA_real_)
  (po-pe)/(1-pe)
}

extract_model_label <- function(path){
  base <- basename(path)
  m <- str_match(base, "^rauch_hs6_\\d{8}_([^_]+)_.+\\.csv$")
  if (!is.na(m[1,2])) return(m[1,2])
  base
}

grab <- function(DT, cand){ c <- intersect(cand, names(DT)); if (length(c)) DT[[c[1]]] else NULL }

read_best <- function(path, tag){
  DT <- fread(path, showProgress=FALSE)
  # normalize code
  if ("code" %in% names(DT))      DT[, code6 := pad6(code)]
  else if ("product_code" %in% names(DT)) DT[, code6 := pad6(product_code)]
  else stop("Missing code/product_code in ", path)

  if (!"rauch_category" %in% names(DT)) stop("Missing 'rauch_category' in ", path)
  DT[, pred := tolower(rauch_category)]

  conf <- suppressWarnings(as.numeric(grab(DT, c("confidence","conf","prob"))))
  if (is.null(conf)) conf <- NA_real_
  DT[, conf := conf]

  desc <- grab(DT, c("short_description","description","desc","label","item","title","text"))
  if (is.null(desc)) desc <- NA_character_
  DT[, desc := as.character(desc)]

  # optional reasoning capture
  reason <- grab(DT, c("reasoning","reason","rationale","explanation","analysis","why"))
  if (is.null(reason)) reason <- NA_character_
  DT[, reason := as.character(reason)]

  D1 <- DT[!is.na(pred) & nzchar(pred)]
  best <- if (all(is.na(D1$conf))) {
    M <- D1[, .N, by=.(code6, pred)][order(code6, -N)]
    M[, .SD[1], by=code6][, .(code6, pred)]
  } else {
    D1[order(-ifelse(is.na(conf), -Inf, conf))][, .SD[1], by=code6][, .(code6, pred, conf, desc, reason)]
  }
  if (!"conf" %in% names(best)) best[, conf := NA_real_]
  if (!"desc" %in% names(best)) best[, desc := NA_character_]
  if (!"reason" %in% names(best)) best[, reason := NA_character_]
  setnames(best, c("pred","conf","desc","reason"),
           c(paste0("pred_",tag), paste0("conf_",tag), paste0("desc_",tag), paste0("reason_",tag)))
  best[]
}

# ---------- CLI ----------
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript 05_compare_models.R <MODEL_1.csv> <MODEL_2.csv> [MODEL_3.csv ...]")
}
for (f in args) if (!file.exists(f)) stop("File not found: ", f)

tags <- paste0("M", seq_along(args))

# ---------- Read all models ----------
model_tables <- vector("list", length(args))
names(model_tables) <- tags
for (i in seq_along(args)) {
  model_tables[[i]] <- read_best(args[i], tags[i])
}

D_pred <- Reduce(function(x,y) merge(x,y, by="code6", all=TRUE), model_tables)

model_labels <- data.table(
  Tag   = tags,
  Model = vapply(args, extract_model_label, character(1)),
  File  = args
)

# helper to map labels to numeric for correlation
map_to_num <- function(x) {
  f <- factor(tolower(x), levels = c("w","r","n"))
  as.numeric(f) - 1  # w=0, r=1, n=2
}

# ---------- Pairwise agreement ----------
pair_stats <- list()
valid <- c("w","r","n")
conf_mats <- list()
disagree_sections <- list()
if (length(tags) >= 2) {
  combs <- t(combn(tags, 2))
  for (k in seq_len(nrow(combs))) {
    t1 <- combs[k,1]; t2 <- combs[k,2]
    p1 <- paste0("pred_", t1); p2 <- paste0("pred_", t2)
    c1 <- paste0("conf_", t1); c2 <- paste0("conf_", t2)
    d1 <- paste0("desc_", t1); d2 <- paste0("desc_", t2)
    r1 <- paste0("reason_", t1); r2 <- paste0("reason_", t2)

    Both <- D_pred[!is.na(get(p1)) & get(p1) %in% valid &
                    !is.na(get(p2)) & get(p2) %in% valid,
                  .(code6,
                    A = get(p1), B = get(p2),
                    conf_A = get(c1), conf_B = get(c2),
                    desc_A = get(d1), desc_B = get(d2),
                    reason_A = get(r1), reason_B = get(r2))]

    if (!nrow(Both)) next

    agree_rate <- mean(Both$A == Both$B)
    kap <- kappa(Both$A, Both$B, valid)
    # Pearson correlation on numeric labels (w=0,r=1,n=2)
    label_corr <- suppressWarnings(cor(map_to_num(Both$A), map_to_num(Both$B)))

    # Confusion matrix counts (Model1 rows, Model2 cols)
    conf_counts <- Both[, .N, by = .(A, B)]
    conf_counts <- CJ(A = valid, B = valid)[conf_counts, on = .(A, B)]
    conf_counts[is.na(N), N := 0L]
    cm <- dcast(conf_counts, A ~ B, value.var = "N", fill = 0)
    setnames(cm, "A", paste0("Model_", t1, "_label"))
    conf_mats[[k]] <- list(
      title = sprintf("Confusion: %s (rows) vs %s (cols)", model_labels[Tag==t1, Model], model_labels[Tag==t2, Model]),
      table = cm
    )

    pair_stats[[k]] <- data.table(
      Model_1 = t1,
      Model_2 = t2,
      Label_1 = model_labels[Tag==t1, Model],
      Label_2 = model_labels[Tag==t2, Model],
      Intersection_N = nrow(Both),
      Agreement_rate = rnd(agree_rate),
      Cohen_kappa    = rnd(kap),
      Label_corr     = rnd(label_corr)
    )

    if (k == 1) {
      AgreeWrong <- Both[A==B]
      aw_by_label <- AgreeWrong[, .N, by=.(shared_label = A)][order(-N)]
      Top_examples <- AgreeWrong[order(-pmax(ifelse(is.na(conf_A),-Inf,conf_A),
                                             ifelse(is.na(conf_B),-Inf,conf_B)))][
                                               1:min(.N, 10),
                                               .(code6,
                                                 shared_label = A,
                                                 conf_1 = rnd(conf_A),
                                                 conf_2 = rnd(conf_B),
                                                 desc_1 = substr(ifelse(is.na(desc_A),"",desc_A), 1, 160),
                                                 desc_2 = substr(ifelse(is.na(desc_B),"",desc_B), 1, 160))]
      first_pair_ids   <- list(t1=t1, t2=t2)
      first_aw_by_label <- aw_by_label
      first_examples   <- Top_examples
    }

    # disagreement analysis with confidence and reasoning snippets
    Disagree <- Both[A != B]
    if (nrow(Disagree)) {
      Disagree[, min_conf := pmin(conf_A, conf_B, na.rm = TRUE)]
      Disagree[, mean_conf := rowMeans(cbind(conf_A, conf_B), na.rm = TRUE)]
      agree_conf <- Both[A == B, mean(rowMeans(cbind(conf_A, conf_B), na.rm = TRUE), na.rm = TRUE)]
      disagree_conf <- mean(Disagree$mean_conf, na.rm = TRUE)

      top_disagree <- Disagree[order(min_conf)][1:min(.N, 10L),
        .(code6,
          A, B,
          conf_A = rnd(conf_A), conf_B = rnd(conf_B),
          desc_A = substr(ifelse(is.na(desc_A),"",desc_A), 1, 140),
          desc_B = substr(ifelse(is.na(desc_B),"",desc_B), 1, 140),
          reason_A = substr(ifelse(is.na(reason_A),"",reason_A), 1, 160),
          reason_B = substr(ifelse(is.na(reason_B),"",reason_B), 1, 160))
      ]

      disagree_sections[[k]] <- list(
        title = sprintf("Disagreements: %s vs %s", model_labels[Tag==t1, Model], model_labels[Tag==t2, Model]),
        agree_conf = rnd(agree_conf),
        disagree_conf = rnd(disagree_conf),
        table = top_disagree
      )
    }
  }
}
pair_table <- if (length(pair_stats)) rbindlist(pair_stats, fill=TRUE) else data.table()

# ---------- Three-model summary (if 3 files) ----------
triple_summary <- NULL
triple_dist <- NULL
if (length(tags) == 3) {
  pcols <- paste0("pred_", tags)
  Trip <- D_pred[!is.na(get(pcols[1])) & get(pcols[1]) %in% valid &
                 !is.na(get(pcols[2])) & get(pcols[2]) %in% valid &
                 !is.na(get(pcols[3])) & get(pcols[3]) %in% valid,
                 .(code6,
                   p1 = get(pcols[1]),
                   p2 = get(pcols[2]),
                   p3 = get(pcols[3]))]
  trip_N <- nrow(Trip)
  if (trip_N > 0) {
    Trip[, all_same := (p1 == p2 & p2 == p3)]
    N_all_same <- sum(Trip$all_same)
    if (N_all_same > 0) {
      Trip_same <- Trip[all_same==TRUE]
      triple_summary <- data.frame(
        Metric = c("Intersection N (3 models)",
                   "All three give same label (count)"),
        Value  = c(trip_N, N_all_same)
      )
      triple_dist <- Trip_same[, .N, by=.(shared_label = p1)][order(-N)]
    } else {
      triple_summary <- data.frame(
        Metric = c("Intersection N (3 models)",
                   "All three give same label (count)"),
        Value  = c(trip_N, 0L)
      )
    }
  }
}

# ---------- Write Markdown ----------
out_dir <- file.path("output", "metrics")
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
report <- file.path(out_dir, sprintf("hs_multi_model_confusions_%s.md", stamp))
sink(report)

cat("# HS Rauch — Model Agreement Report\n\n")
cat(sprintf("**Run timestamp:** %s  \n\n", stamp))

cat("## Models Compared\n\n")
cat(md_table(model_labels), "\n\n")
cat("---\n\n")

cat("## Pairwise Agreement (on intersection)\n\n")
if (nrow(pair_table)) {
  cat(md_table(pair_table), "\n\n")
} else {
  cat("_(no pairwise stats available)_\n\n")
}

# Confusion matrices per pair
if (length(conf_mats)) {
  cat("### Confusion Matrices (rows = Model 1, cols = Model 2)\n\n")
  for (cm in conf_mats) {
    cat("**", cm$title, "**\n\n", sep = "")
    cat(md_table(cm$table), "\n\n")
  }
}

# Disagreements, confidence, and reasoning
if (length(disagree_sections)) {
  cat("### Disagreements: Confidence and Reasoning\n\n")
  for (ds in disagree_sections) {
    cat("**", ds$title, "**\n\n", sep = "")
    if (!is.na(ds$agree_conf) || !is.na(ds$disagree_conf)) {
      cat(md_table(data.frame(
        Metric = c("Mean confidence (agreements)", "Mean confidence (disagreements)"),
        Value  = c(ds$agree_conf, ds$disagree_conf)
      )), "\n\n")
    }
    if (nrow(ds$table)) {
      cat("Lowest-confidence disagreements (up to 10):\n\n")
      cat(md_table(ds$table), "\n\n")
    } else {
      cat("_(no disagreements found)_\n\n")
    }
  }
}

if (exists("first_pair_ids")) {
  t1 <- first_pair_ids$t1; t2 <- first_pair_ids$t2
  lab1 <- model_labels[Tag==t1, Model]
  lab2 <- model_labels[Tag==t2, Model]

  cat(sprintf("### Detailed: First Pair (%s / %s)\n\n", lab1, lab2))
  cat("Shared-label counts\n\n")
  cat(md_table(first_aw_by_label), "\n\n")
  cat("Top examples where both models give the same label (up to 10)\n\n")
  if (nrow(first_examples)) cat(md_table(first_examples), "\n\n") else cat("_(none)_\n\n")
}
cat("---\n\n")

if (!is.null(triple_summary)) {
  cat("## Three-Model Summary\n\n")
  cat(md_table(triple_summary), "\n\n")
  if (!is.null(triple_dist)) {
    cat("### Distribution where all three models give the same label\n\n")
    cat(md_table(triple_dist), "\n\n")
  }
  cat("---\n\n")
}

cat("_Report generated by 05_compare_models.R_\n")
sink()

cat("Wrote report: ", report, "\n", sep="")
