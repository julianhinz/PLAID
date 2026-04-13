#!/usr/bin/env Rscript
# 06_compare_models.R — Confusion matrices for multiple models vs Rauch + model↔model comparison
# Usage:
#   Rscript 06_compare_models.R [con|lib] <MODEL_1.csv> <MODEL_2.csv> [MODEL_3.csv ...]
# Notes:
#   - Accepts 2 or more model output files.
#   - Model labels are inferred from filenames like:
#       rauch_sitc2_<YYYYMMDD>_<MODEL>_<RUN_ID>.csv
#   - Duplicates per code4 are collapsed by highest confidence (or by majority label if no confidence).
#   - Outputs a Markdown report with confusion matrices, agreement stats, and
#     (if 3 models) a three-way agreement summary.

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly=TRUE)) install.packages("data.table")
  if (!requireNamespace("stringr",   quietly=TRUE)) install.packages("stringr")
  library(data.table); library(stringr)
})

# ---------- Helpers ----------
pad4 <- function(x) stringr::str_pad(as.character(x), 4, pad="0")
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

grab <- function(DT, cand){ c <- intersect(cand, names(DT)); if (length(c)) DT[[c[1]]] else NULL }

# extract model label from filename; expects:
#   rauch_sitc2_<YYYYMMDD>_<MODEL>_<RUN>.csv
extract_model_label <- function(path){
  base <- basename(path)
  m <- str_match(base, "^rauch_sitc2_\\d{8}_([^_]+)_.+\\.csv$")
  if (!is.na(m[1,2])) return(m[1,2])
  # fallback if pattern doesn't match: just use basename
  base
}

read_best <- function(path, tag){
  DT <- fread(path, showProgress=FALSE)
  # normalize code4
  if ("code4" %in% names(DT)) DT[, code4 := pad4(code4)]
  else if ("code" %in% names(DT)) DT[, code4 := pad4(code)]
  else if ("sitc4" %in% names(DT)) DT[, code4 := pad4(sitc4)]
  else stop("Missing code/code4/sitc4 in ", path)
  
  if (!"rauch_category" %in% names(DT)) stop("Missing 'rauch_category' in ", path)
  DT[, pred := tolower(rauch_category)]
  
  # optional fields
  conf <- suppressWarnings(as.numeric(grab(DT, c("confidence","conf","prob"))))
  if (is.null(conf)) conf <- NA_real_
  DT[, conf := conf]
  
  desc <- grab(DT, c("short_description","description","desc","label","item","title","sitc_label","text"))
  if (is.null(desc)) desc <- NA_character_
  DT[, desc := as.character(desc)]
  
  # one row per code: highest conf; if all NA, majority label
  D1 <- DT[!is.na(pred) & nzchar(pred)]
  best <- if (all(is.na(D1$conf))) {
    M <- D1[, .N, by=.(code4, pred)][order(code4, -N)]
    M[, .SD[1], by=code4][, .(code4, pred)]
  } else {
    D1[order(-ifelse(is.na(conf), -Inf, conf))][, .SD[1], by=code4][, .(code4, pred, conf, desc)]
  }
  if (!"conf" %in% names(best)) best[, conf := NA_real_]
  if (!"desc" %in% names(best)) best[, desc := NA_character_]
  setnames(best, c("pred","conf","desc"),
           c(paste0("pred_",tag), paste0("conf_",tag), paste0("desc_",tag)))
  best[]
}

# --- ROBUST CONFUSION (fixed) ---
compute_confusion <- function(eval_dt, cats = c("w","r","n")) {
  # keep only rows within allowed categories
  X <- eval_dt[truth %in% cats & pred %in% cats]
  
  # empty case: return full grid of zeros
  if (!nrow(X)) {
    zmat <- matrix(0L,
                   nrow = length(cats),
                   ncol = length(cats),
                   dimnames = list(NULL, cats))
    cm <- data.frame(truth = cats, zmat, check.names = FALSE)
    return(list(
      N      = 0L,
      acc    = NA_real_,
      cm     = cm,
      cm_row = as.data.frame(zmat, check.names = FALSE),
      cm_col = as.data.frame(zmat, check.names = FALSE)
    ))
  }
  
  # counts
  conf_counts <- X[, .N, by = .(truth, pred)]
  
  # full grid truth x pred
  conf_counts <- CJ(truth = cats, pred = cats)[conf_counts, on = .(truth, pred)]
  conf_counts[is.na(N), N := 0L]
  
  # wide table
  cm <- dcast(conf_counts, truth ~ pred, value.var = "N", fill = 0)
  
  # make sure all cats exist as columns (in case dcast dropped some)
  missing_cols <- setdiff(cats, names(cm))
  if (length(missing_cols)) {
    for (mc in missing_cols) {
      cm[, (mc) := 0L]
    }
  }
  
  # enforce column order: truth + cats
  cm <- cm[, c("truth", cats), with = FALSE]
  
  # numeric matrix for normalization
  cm_mat <- as.matrix(cm[, -1, with = FALSE])
  rownames(cm_mat) <- cm$truth
  
  rs <- rowSums(cm_mat)
  cs <- colSums(cm_mat)
  
  cm_row <- sweep(cm_mat, 1, ifelse(rs == 0, 1, rs), "/")  # recall
  cm_col <- sweep(cm_mat, 2, ifelse(cs == 0, 1, cs), "/")  # precision
  
  list(
    N      = nrow(X),
    acc    = mean(X$pred == X$truth),
    cm     = cm,
    cm_row = as.data.frame(round(cm_row, 3), check.names = FALSE),
    cm_col = as.data.frame(round(cm_col, 3), check.names = FALSE)
  )
}


kappa <- function(a,b,levels=c("w","r","n")){
  a <- factor(a, levels=levels); b <- factor(b, levels=levels)
  tab <- table(a,b); N <- sum(tab); if (N==0) return(NA_real_)
  po <- sum(diag(tab))/N; pa <- rowSums(tab)/N; pb <- colSums(tab)/N; pe <- sum(pa*pb)
  if (isTRUE(all.equal(pe,1))) return(NA_real_)
  (po-pe)/(1-pe)
}

# ---------- CLI ----------
args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript 06_compare_models.R [con|lib] <MODEL_1.csv> <MODEL_2.csv> [MODEL_3.csv ...]")
}

if (args[1] %in% c("con","lib")) {
  truth_col <- args[1]
  files <- args[-1]
} else {
  truth_col <- "con"
  files <- args
}
if (length(files) < 2) {
  stop("Need at least two model output files.")
}

for (f in files) {
  if (!file.exists(f)) stop("File not found: ", f)
}

# tags M1, M2, M3, ...
tags <- paste0("M", seq_along(files))

# ---------- Truth (Rauch) ----------
dir.create(file.path("temp","rauch"), showWarnings=FALSE, recursive=TRUE)
truth_path <- file.path("temp","rauch","Rauch_classification_revised.csv")
if (!file.exists(truth_path)) {
  download.file("https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv",
                destfile=truth_path, mode="wb", quiet=TRUE)
}
Rauch <- fread(truth_path, showProgress=FALSE)[, .(code4 = pad4(sitc4),
                                                   con  = tolower(con),
                                                   lib  = tolower(lib))]
Rauch[, truth := if (truth_col=="con") con else lib]

valid <- c("w","r","n")

# ---------- Read & collapse all models ----------
model_tables <- vector("list", length(files))
names(model_tables) <- tags
for (i in seq_along(files)) {
  model_tables[[i]] <- read_best(files[i], tags[i])
}

# merge all predictions on code4
D_pred <- Reduce(function(x,y) merge(x,y, by="code4", all=TRUE), model_tables)

# merge truth
D <- merge(D_pred, Rauch[, .(code4, truth)], by="code4", all.x=TRUE)

# ---------- Confusion per model ----------
conf_list <- list()
for (i in seq_along(tags)) {
  tag <- tags[i]
  pred_col <- paste0("pred_", tag)
  if (!pred_col %in% names(D)) next
  Eval <- D[!is.na(truth) & truth %in% valid &
              !is.na(get(pred_col)) & get(pred_col) %in% valid,
            .(truth, pred = get(pred_col))]
  conf_list[[tag]] <- compute_confusion(Eval, valid)
}

# model labels from filenames
model_labels <- data.table(
  Tag   = tags,
  Model = vapply(files, extract_model_label, character(1)),
  File  = files
)

# ---------- Pairwise model↔model agreement ----------
pair_stats <- list()
if (length(tags) >= 2) {
  combs <- t(combn(tags, 2))
  for (k in seq_len(nrow(combs))) {
    t1 <- combs[k,1]; t2 <- combs[k,2]
    p1 <- paste0("pred_", t1); p2 <- paste0("pred_", t2)
    c1 <- paste0("conf_", t1); c2 <- paste0("conf_", t2)
    d1 <- paste0("desc_", t1); d2 <- paste0("desc_", t2)
    
    Both <- D[!is.na(truth) & truth %in% valid &
                !is.na(get(p1)) & get(p1) %in% valid &
                !is.na(get(p2)) & get(p2) %in% valid,
              .(code4, truth,
                A = get(p1), B = get(p2),
                conf_A = get(c1), conf_B = get(c2),
                desc_A = get(d1), desc_B = get(d2))]
    
    if (!nrow(Both)) next
    
    agree_rate <- mean(Both$A == Both$B)
    kap <- kappa(Both$A, Both$B, valid)
    
    both_correct       <- sum(Both$A==Both$truth & Both$B==Both$truth)
    A_only_correct     <- sum(Both$A==Both$truth & Both$B!=Both$truth)
    B_only_correct     <- sum(Both$A!=Both$truth & Both$B==Both$truth)
    both_wrong         <- sum(Both$A!=Both$truth & Both$B!=Both$truth)
    agree_but_wrong    <- sum(Both$A==Both$B & Both$A!=Both$truth)
    disagree_and_wrong <- sum(Both$A!=Both$B & Both$A!=Both$truth & Both$B!=Both$truth)
    intersect_N        <- nrow(Both)
    
    pair_stats[[k]] <- data.table(
      Model_1 = t1,
      Model_2 = t2,
      Label_1 = model_labels[Tag==t1, Model],
      Label_2 = model_labels[Tag==t2, Model],
      Intersection_N = intersect_N,
      Agreement_rate = rnd(agree_rate),
      Cohen_kappa    = rnd(kap),
      Both_correct       = both_correct,
      Only_1_correct     = A_only_correct,
      Only_2_correct     = B_only_correct,
      Both_wrong         = both_wrong,
      Agree_but_wrong    = agree_but_wrong,
      Disagree_both_wrong= disagree_and_wrong
    )
    
    # Detailed "agree but wrong" examples for the first pair
    if (k == 1) {
      AgreeWrong <- Both[A==B & A!=truth]
      aw_by_truth <- AgreeWrong[, .N, by=.(truth, wrong_label=A)][order(-N)]
      topN <- 10L
      EX_aw <- AgreeWrong[order(-pmax(ifelse(is.na(conf_A),-Inf,conf_A),
                                      ifelse(is.na(conf_B),-Inf,conf_B)))][
                                        1:min(.N, topN),
                                        .(code4, truth, wrong=A,
                                          conf_1=rnd(conf_A), conf_2=rnd(conf_B),
                                          desc_1=substr(ifelse(is.na(desc_A),"",desc_A), 1, 160),
                                          desc_2=substr(ifelse(is.na(desc_B),"",desc_B), 1, 160))]
      first_pair_ids <<- list(t1=t1, t2=t2)
      first_aw_by_truth <<- aw_by_truth
      first_EX_aw <<- EX_aw
    }
  }
}
pair_table <- if (length(pair_stats)) rbindlist(pair_stats, fill=TRUE) else data.table()

# ---------- Three-model agreement (if exactly 3 models) ----------
triple_summary <- NULL
triple_dist <- NULL
if (length(tags) == 3) {
  pcols <- paste0("pred_", tags)
  Trip <- D[!is.na(truth) & truth %in% valid &
              !is.na(get(pcols[1])) & get(pcols[1]) %in% valid &
              !is.na(get(pcols[2])) & get(pcols[2]) %in% valid &
              !is.na(get(pcols[3])) & get(pcols[3]) %in% valid,
            .(code4, truth,
              p1 = get(pcols[1]),
              p2 = get(pcols[2]),
              p3 = get(pcols[3]))]
  trip_N <- nrow(Trip)
  if (trip_N > 0) {
    Trip[, all_same := (p1 == p2 & p2 == p3)]
    N_all_same <- sum(Trip$all_same)
    if (N_all_same > 0) {
      Trip_same <- Trip[all_same==TRUE]
      N_all_same_correct <- sum(Trip_same$p1 == Trip_same$truth)
      N_all_same_wrong   <- N_all_same - N_all_same_correct
      triple_summary <- data.frame(
        Metric = c("Intersection N (3 models)",
                   "All three give same label (count)",
                   "All three same AND correct vs Rauch",
                   "All three same BUT wrong vs Rauch"),
        Value  = c(trip_N, N_all_same, N_all_same_correct, N_all_same_wrong)
      )
      triple_dist <- Trip_same[, .N, by=.(truth, shared_label = p1)][order(-N)]
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
report <- file.path(out_dir, sprintf("multi_model_confusions_%s.md", stamp))
sink(report)

cat("# Multi-Model Report — Confusion vs Rauch & Model Agreement\n\n")
cat(sprintf("**Run timestamp:** %s  \n", stamp))
cat(sprintf("**Ground truth column:** `%s`  \n\n", truth_col))

cat("## Models Compared\n\n")
cat(md_table(model_labels), "\n\n")
cat("---\n\n")

cat("## 1) Confusion — Each Model vs Rauch\n\n")
for (i in seq_along(tags)) {
  tag <- tags[i]
  label <- model_labels[Tag==tag, Model]
  cf <- conf_list[[tag]]
  if (is.null(cf)) next
  cat(sprintf("### Model %s — %s\n\n", tag, label))
  cat(sprintf("- N used: **%d**  \n- Accuracy: **%.3f**\n\n", cf$N, rnd(cf$acc)))
  cat("#### Counts\n\n");          cat(md_table(cf$cm), "\n\n")
  cat("#### Row-normalized (Recall)\n\n");  cat(md_table(cf$cm_row), "\n\n")
  cat("#### Column-normalized (Precision)\n\n"); cat(md_table(cf$cm_col), "\n\n")
}
cat("---\n\n")

cat("## 2) Pairwise Model ↔ Model Agreement (on intersection)\n\n")
if (nrow(pair_table)) {
  cat(md_table(pair_table), "\n\n")
} else {
  cat("_(no pairwise stats available)_\n\n")
}

# Detailed examples only for the first pair (if present)
if (exists("first_pair_ids")) {
  t1 <- first_pair_ids$t1; t2 <- first_pair_ids$t2
  lab1 <- model_labels[Tag==t1, Model]
  lab2 <- model_labels[Tag==t2, Model]
  
  cat(sprintf("### Detailed: First Pair (%s / %s)\n\n", lab1, lab2))
  cat("#### Where both models agree but are wrong (counts)\n\n")
  cat(md_table(first_aw_by_truth), "\n\n")
  cat("#### Top examples — agree but wrong (up to 10)\n\n")
  if (nrow(first_EX_aw)) cat(md_table(first_EX_aw), "\n\n") else cat("_(none)_\n\n")
}
cat("---\n\n")

if (!is.null(triple_summary)) {
  cat("## 3) Three-Model Agreement (all three files)\n\n")
  cat("These stats are only computed because **three** model files were provided.\n\n")
  cat("### Summary — how many are the same across the three models\n\n")
  cat(md_table(triple_summary), "\n\n")
  if (!is.null(triple_dist)) {
    cat("### Distribution where all three models give the same (but not necessarily correct) label\n\n")
    cat(md_table(triple_dist), "\n\n")
  }
  cat("---\n\n")
}

cat("_Report generated by 06_compare_models.R_\n")
sink()

cat("Wrote report: ", report, "\n", sep="")
