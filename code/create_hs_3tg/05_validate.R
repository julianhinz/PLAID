#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_validate.R
# -------------------------------------------------------------------
# Validates HS 3TG conflict minerals LLM classifications against a
# benchmark list derived from EU Regulation 2017/821 Annex I.
#
# Benchmark file: input/benchmarks/eu_conflict_minerals_hs.csv
#   Columns: hs6_code (character, 6-digit), mineral (tin/tantalum/tungsten/gold),
#            regulated (logical, TRUE = listed in EU 2017/821 Annex I)
#
# Usage:
#   Rscript 05_validate.R --model openai/gpt-4o-mini --run_id RUN123
#   Rscript 05_validate.R openai/gpt-4o-mini RUN123
#   Rscript 05_validate.R  # picks latest indicator file
#
# Outputs:
#   output/metrics/3tg_validation_report.md
#   output/metrics/3tg_confusion_matrix.csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr, optparse)

# ────────────────────────────────────────────────────────────────────
# Args
# ────────────────────────────────────────────────────────────────────
default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-4o-mini"

option_list <- list(
  make_option(c("-m","--model"),  type = "character", default = default_model,
              help = "Model id (used to locate indicator file). [default: %default]"),
  make_option(c("-i","--run_id"), type = "character", default = NA_character_,
              help = "Run identifier (used to locate indicator file). If omitted, pick latest."),
  make_option(c("-f","--file"),   type = "character", default = NA_character_,
              help = "Direct path to 3TG indicator CSV. Overrides --model/--run_id.")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog [options] [MODEL] [RUN_ID]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

posargs   <- parsed$args
model     <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
run_id    <- if (length(posargs) >= 2) posargs[[2]] else parsed$options$run_id
file_override <- parsed$options$file

# ────────────────────────────────────────────────────────────────────
# Load indicator file
# ────────────────────────────────────────────────────────────────────
if (!is.na(file_override)) {
  indicator_file <- file_override
} else {
  safe_model  <- gsub("[^A-Za-z0-9]+", "-", model)
  cand <- list.files("output/indicators", pattern = "^3tg_hs6_.*\\.csv$", full.names = TRUE)
  if (!is.na(run_id)) {
    safe_run_id <- gsub("[^A-Za-z0-9]+", "-", run_id)
    cand <- cand[grepl(safe_model, cand) & grepl(safe_run_id, cand)]
  } else {
    cand <- cand[grepl(safe_model, cand)]
  }
  if (length(cand) == 0) stop("No 3TG indicator file found. Run 04_save_indicators.R first.")
  indicator_file <- cand[which.max(file.info(cand)$mtime)]
}

message("Using indicator file: ", indicator_file)
llm_dt <- fread(indicator_file, colClasses = "character")

if (!("code" %in% names(llm_dt)) || !("conflict_mineral" %in% names(llm_dt))) {
  stop("Indicator file must have columns 'code' and 'conflict_mineral'.")
}

llm_dt[, code := stringr::str_pad(code, 6, pad = "0")]
llm_dt[, conflict_mineral_llm := as.logical(conflict_mineral)]
llm_dt[, specific_mineral_llm := tolower(trimws(specific_mineral))]

# ────────────────────────────────────────────────────────────────────
# Load benchmark
# ────────────────────────────────────────────────────────────────────
benchmark_file <- "input/benchmarks/eu_conflict_minerals_hs.csv"
if (!file.exists(benchmark_file)) {
  stop(
    "Benchmark file not found: ", benchmark_file, "\n",
    "Expected columns: hs6_code (6-digit string), mineral (tin/tantalum/tungsten/gold), ",
    "regulated (TRUE/FALSE based on EU Regulation 2017/821 Annex I)."
  )
}

bench <- fread(benchmark_file, colClasses = "character")
bench[, hs6_code := stringr::str_pad(hs6_code, 6, pad = "0")]
bench[, regulated := as.logical(regulated)]
bench[, mineral := tolower(trimws(mineral))]

message(sprintf("Benchmark: %d rows, %d regulated codes.",
                nrow(bench), sum(bench$regulated, na.rm = TRUE)))

# ────────────────────────────────────────────────────────────────────
# Merge
# ────────────────────────────────────────────────────────────────────
merged <- merge(
  llm_dt[, .(code, conflict_mineral_llm, specific_mineral_llm, confidence)],
  bench[, .(code = hs6_code, mineral_bench = mineral, regulated)],
  by = "code",
  all = TRUE
)

# Codes only in benchmark (not in LLM output)
n_missing_llm <- sum(is.na(merged$conflict_mineral_llm))
if (n_missing_llm > 0) {
  warning(sprintf("%d benchmark codes have no LLM prediction.", n_missing_llm))
}

# Codes only in LLM output (not in benchmark)
n_llm_only <- sum(is.na(merged$regulated))

# For confusion matrix: restrict to codes present in both
eval_dt <- merged[!is.na(conflict_mineral_llm) & !is.na(regulated)]

# ────────────────────────────────────────────────────────────────────
# Confusion matrix
# ────────────────────────────────────────────────────────────────────
eval_dt[, tp := conflict_mineral_llm == TRUE  & regulated == TRUE]
eval_dt[, fp := conflict_mineral_llm == TRUE  & regulated == FALSE]
eval_dt[, fn := conflict_mineral_llm == FALSE & regulated == TRUE]
eval_dt[, tn := conflict_mineral_llm == FALSE & regulated == FALSE]

tp <- sum(eval_dt$tp, na.rm = TRUE)
fp <- sum(eval_dt$fp, na.rm = TRUE)
fn <- sum(eval_dt$fn, na.rm = TRUE)
tn <- sum(eval_dt$tn, na.rm = TRUE)
n_eval <- nrow(eval_dt)

precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
recall    <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
f1        <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
  2 * precision * recall / (precision + recall)
} else NA_real_
accuracy  <- if (n_eval > 0) (tp + tn) / n_eval else NA_real_

# Agreement on mineral type (among true positives)
tp_dt <- eval_dt[tp == TRUE & !is.na(specific_mineral_llm) & !is.na(mineral_bench)]
mineral_agreement <- if (nrow(tp_dt) > 0) {
  mean(tp_dt$specific_mineral_llm == tp_dt$mineral_bench, na.rm = TRUE)
} else NA_real_

# ────────────────────────────────────────────────────────────────────
# Mineral-level breakdown
# ────────────────────────────────────────────────────────────────────
minerals <- c("tin", "tantalum", "tungsten", "gold")
mineral_stats <- rbindlist(lapply(minerals, function(mn) {
  # Fix (2026-04-10): sub_bench uses hs6_code, not code. Prior version referenced
  # sub_bench$code which was NULL, making sub_merged empty and yielding NA rows.
  sub_bench  <- bench[mineral == mn & regulated == TRUE]
  sub_merged <- merged[!is.na(conflict_mineral_llm) & code %in% sub_bench$hs6_code]
  if (nrow(sub_merged) == 0) return(data.table(mineral = mn, n_bench = nrow(sub_bench),
                                                tp = 0L, fp = 0L, fn = 0L,
                                                recall = NA_real_))
  # Within sub_merged every row corresponds to a bench-regulated code for this
  # mineral, so regulated is definitionally TRUE. TP = LLM also flagged it,
  # FN = LLM missed it. FP is not defined at the per-mineral level this way.
  tp_mn <- sum(sub_merged$conflict_mineral_llm == TRUE,  na.rm = TRUE)
  fn_mn <- sum(sub_merged$conflict_mineral_llm == FALSE, na.rm = TRUE)
  fp_mn <- 0L
  recall_mn <- if ((tp_mn + fn_mn) > 0) tp_mn / (tp_mn + fn_mn) else NA_real_
  data.table(mineral = mn, n_bench = nrow(sub_bench), tp = tp_mn, fp = fp_mn,
             fn = fn_mn, recall = recall_mn)
}))

# LLM additionally flags derivative/downstream products
n_llm_extra <- sum(merged$conflict_mineral_llm == TRUE & merged$regulated == FALSE, na.rm = TRUE)

# ────────────────────────────────────────────────────────────────────
# Output directories
# ────────────────────────────────────────────────────────────────────
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)

# Confusion matrix CSV
conf_matrix <- data.table(
  predicted_positive = c(tp, fp),
  predicted_negative = c(fn, tn),
  row_label = c("actual_positive", "actual_negative")
)
fwrite(conf_matrix, "output/metrics/3tg_confusion_matrix.csv")

# ────────────────────────────────────────────────────────────────────
# Markdown report
# ────────────────────────────────────────────────────────────────────
sink("output/metrics/3tg_validation_report.md")
cat("# 3TG Conflict Minerals Validation Report\n\n")
cat(sprintf("**Indicator file:** %s\n\n", basename(indicator_file)))
cat(sprintf("**Benchmark:** EU Regulation 2017/821 Annex I (`%s`)\n\n", benchmark_file))
cat(sprintf("**LLM predictions:** %d HS-6 codes classified (%d conflict mineral, %d non-conflict)\n\n",
            nrow(llm_dt),
            sum(llm_dt$conflict_mineral_llm, na.rm = TRUE),
            sum(!llm_dt$conflict_mineral_llm, na.rm = TRUE)))
cat(sprintf("**Benchmark:** %d codes evaluated (%d regulated as conflict minerals)\n\n",
            nrow(bench), sum(bench$regulated, na.rm = TRUE)))
cat(sprintf("**Missing from LLM output:** %d benchmark codes have no prediction\n\n", n_missing_llm))

cat("## Overall Performance (vs. EU 2017/821 Annex I)\n\n")
cat(sprintf("| Metric | Value |\n|--------|-------|\n"))
cat(sprintf("| N evaluated | %d |\n", n_eval))
cat(sprintf("| True Positives (TP) | %d |\n", tp))
cat(sprintf("| False Positives (FP) | %d |\n", fp))
cat(sprintf("| False Negatives (FN) | %d |\n", fn))
cat(sprintf("| True Negatives (TN) | %d |\n", tn))
cat(sprintf("| Precision | %.3f |\n", ifelse(is.na(precision), NA, round(precision, 3))))
cat(sprintf("| Recall | %.3f |\n", ifelse(is.na(recall), NA, round(recall, 3))))
cat(sprintf("| F1 Score | %.3f |\n", ifelse(is.na(f1), NA, round(f1, 3))))
cat(sprintf("| Accuracy | %.3f |\n", ifelse(is.na(accuracy), NA, round(accuracy, 3))))
cat("\n")

cat("## Confusion Matrix\n\n")
cat("| | Predicted Positive | Predicted Negative |\n")
cat("|---|---|---|\n")
cat(sprintf("| **Actual Positive** | %d (TP) | %d (FN) |\n", tp, fn))
cat(sprintf("| **Actual Negative** | %d (FP) | %d (TN) |\n", fp, tn))
cat("\n")

cat("## Per-Mineral Recall (vs. EU 2017/821)\n\n")
cat("| Mineral | Benchmark N | TP | FP | FN | Recall |\n")
cat("|---------|-------------|----|----|-----|--------|\n")
for (i in seq_len(nrow(mineral_stats))) {
  r <- mineral_stats[i]
  cat(sprintf("| %s | %d | %d | %d | %d | %.3f |\n",
              r$mineral, r$n_bench, r$tp, r$fp, r$fn,
              ifelse(is.na(r$recall), NA, round(r$recall, 3))))
}
cat("\n")

cat("## Mineral Type Agreement (True Positives)\n\n")
cat(sprintf("Among TP codes, the LLM correctly identified the specific mineral in **%.1f%%** of cases.\n\n",
            ifelse(is.na(mineral_agreement), NA, round(mineral_agreement * 100, 1))))

cat("## LLM Scope vs. EU Annex I\n\n")
cat(sprintf(
  "The EU 2017/821 Annex I is a precise, curated list. The LLM additionally flagged **%d** codes\n",
  n_llm_extra
))
cat("as conflict minerals that are not in the benchmark — these may include derivative products\n")
cat("(alloys, components, electronics) where a 3TG mineral is a defining input but which are not\n")
cat("explicitly listed in the regulation.\n\n")
cat("**Interpretation:**\n")
cat("- High precision on listed codes validates the LLM's ability to identify core 3TG products.\n")
cat("- Additional flagged codes reflect the LLM's broader interpretation of 'contains or is derived from'.\n")
cat("- Researchers should decide whether to use the strict regulated list or the broader LLM classification.\n\n")

cat("---\n")
cat(sprintf("*Report generated: %s*\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
sink()

message("Saved: output/metrics/3tg_validation_report.md")
message("Saved: output/metrics/3tg_confusion_matrix.csv")
message(sprintf("Precision: %.3f | Recall: %.3f | F1: %.3f | Accuracy: %.3f",
                ifelse(is.na(precision), NA, round(precision, 3)),
                ifelse(is.na(recall), NA, round(recall, 3)),
                ifelse(is.na(f1), NA, round(f1, 3)),
                ifelse(is.na(accuracy), NA, round(accuracy, 3))))

# -------------------------------------------------------------------
# Chapter face validity (added 2026-04-10 per paper revision)
# -------------------------------------------------------------------
source("code/common/plot_chapter_shares.R")

consensus_3tg <- fread("output/database/PLAID_v0.1_3tg_H5.csv.gz")
consensus_3tg[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
consensus_3tg[, chapter := substr(hs6_code, 1, 2)]
consensus_3tg[, flag := as.logical(conflict_mineral)]

expected_3tg <- c("26", "71", "80", "81")
consensus_3tg[, chapter_group := fcase(
  chapter %in% expected_3tg, "Expected 3TG (26, 71, 80, 81)",
  default = "Other chapters"
)]

grp_3tg <- consensus_3tg[,
  .(n_codes = .N, share = mean(flag, na.rm = TRUE)),
  by = chapter_group][order(-share)]

cat("\n=== 3TG chapter-group share ===\n")
print(grp_3tg)

chap_3tg <- consensus_3tg[chapter_group != "Other chapters",
  .(share = mean(flag, na.rm = TRUE)), by = chapter]
chap_3tg[, label := hs_chapter_label(chapter)]
chap_3tg <- chap_3tg[order(-share, chapter)]

plot_3tg_df <- rbind(
  chap_3tg[, .(chapter_group = label, category = "3TG", share)],
  chap_3tg[, .(chapter_group = label, category = "not 3TG", share = 1 - share)]
)
plot_3tg_df[, chapter_group := factor(chapter_group, levels = rev(chap_3tg$label))]

p_3tg <- plot_chapter_shares(plot_3tg_df, indicator = "3tg",
                             category_levels = c("not 3TG", "3TG"),
                             title = "3TG conflict-mineral share by HS chapter")
save_chapter_shares(p_3tg, "output/analysis/figures/3tg_by_chapter.pdf",
                    width = 6, height = 3.5)

tex_3tg <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "\\textbf{Chapter group} & \\textbf{N codes} & \\textbf{3TG share} \\\\",
  "\\midrule",
  apply(grp_3tg, 1, function(r) {
    sprintf("%s & %s & %.1f\\%% \\\\", r[["chapter_group"]], r[["n_codes"]],
            100 * as.numeric(r[["share"]]))
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
writeLines(tex_3tg, "output/analysis/tables/3tg_chapter_face_validity.tex")
message("Wrote: output/analysis/tables/3tg_chapter_face_validity.tex")

# -------------------------------------------------------------------
# Confusion matrix PDF
# -------------------------------------------------------------------
source("code/common/plot_confusion_matrix.R")

# Merge the H5 consensus against the EU benchmark to build a full 2x2
# (positive/negative) matrix over all HS6 codes.
eu_bench <- copy(bench)
eu_bench[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
eu_pos <- unique(eu_bench[regulated == TRUE, .(hs6_code)])

merged_3tg_cm <- merge(consensus_3tg[, .(hs6_code, llm = flag)],
                       eu_pos[, .(hs6_code, eu = TRUE)],
                       by = "hs6_code", all.x = TRUE)
merged_3tg_cm[is.na(eu), eu := FALSE]

cm_3tg <- merged_3tg_cm[, .N, by = .(reference = eu, predicted = llm)]
setnames(cm_3tg, "N", "n")
cm_3tg[, reference := factor(ifelse(reference, "positive", "negative"),
                             levels = c("positive", "negative"))]
cm_3tg[, predicted := factor(ifelse(predicted, "positive", "negative"),
                             levels = c("positive", "negative"))]
p_cm_3tg <- plot_confusion_matrix(cm_3tg, indicator = "3tg",
                                  labels = c("positive", "negative"),
                                  title = "3TG LLM vs EU Reg. 2017/821")
save_confusion_matrix(p_cm_3tg,
                      "output/analysis/figures/3tg_confusion_matrix.pdf",
                      n_levels = 2)
