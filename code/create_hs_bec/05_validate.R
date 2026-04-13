#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_validate.R
# -------------------------------------------------------------------
# Compares LLM BEC classifications to a UN BEC benchmark concordance.
# Computes agreement rate, Cohen's kappa, and confusion matrix.
#
# Usage:
#   Rscript 05_validate.R --model openai/gpt-4o-mini --run_id RUN123
#   Rscript 05_validate.R   # picks latest indicator file
#
# Requires: input/benchmarks/bec_hs_concordance.csv
#   Columns: hs6_code, bec_category  (values: capital/intermediate/consumption)
#
# Outputs:
#   output/metrics/bec_validation_report.md
#   output/metrics/bec_confusion_matrix.csv
#   output/analysis/figures/bec_validation_confusion.png
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr, ggplot2, optparse)

# ────────────────────────────────────────────────────────────────────
# Args
# ────────────────────────────────────────────────────────────────────
default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-4o-mini"

option_list <- list(
  make_option(c("-m","--model"),  type = "character", default = default_model,
              help = "Model id used in the pipeline. [default: %default]"),
  make_option(c("-i","--run_id"), type = "character", default = NA_character_,
              help = "Run identifier. If omitted, use newest bec_hs6_*.csv file.")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog [options] [MODEL] [RUN_ID]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

posargs <- parsed$args
model   <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
run_id  <- if (length(posargs) >= 2) posargs[[2]] else parsed$options$run_id

# ────────────────────────────────────────────────────────────────────
# Benchmark
# ────────────────────────────────────────────────────────────────────
benchmark_file <- file.path("input", "benchmarks", "bec_hs_concordance.csv")

if (!file.exists(benchmark_file)) {
  stop(
    "Benchmark file not found: ", benchmark_file, "\n",
    "Please provide a CSV at that path with columns:\n",
    "  hs6_code     : 6-digit HS code (character)\n",
    "  bec_category : one of capital, intermediate, consumption\n",
    "This file should map HS6 codes to official UN BEC categories for validation."
  )
}

benchmark <- fread(benchmark_file, colClasses = "character")
required_cols <- c("hs6_code", "bec_category")
missing_cols <- setdiff(required_cols, names(benchmark))
if (length(missing_cols) > 0) {
  stop("Benchmark file is missing required columns: ", paste(missing_cols, collapse = ", "))
}

benchmark[, hs6_code := stringr::str_pad(hs6_code, 6, pad = "0")]
benchmark[, bec_category := tolower(trimws(bec_category))]
valid_bec <- c("capital", "intermediate", "consumption")
benchmark <- benchmark[bec_category %in% valid_bec]

message("Benchmark records: ", nrow(benchmark))

# ────────────────────────────────────────────────────────────────────
# LLM indicator file
# ────────────────────────────────────────────────────────────────────
indicator_dir <- file.path("output", "indicators")
llm_files <- list.files(indicator_dir, pattern = "^bec_hs6_.*\\.csv$", full.names = TRUE)

if (length(llm_files) == 0) {
  stop("No bec_hs6_*.csv files found in ", indicator_dir, ". Run the pipeline first.")
}

if (!is.na(run_id)) {
  safe_run <- gsub("[^A-Za-z0-9]+", "-", run_id)
  matched <- llm_files[grepl(safe_run, llm_files, fixed = TRUE)]
  if (length(matched) > 0) {
    llm_file <- matched[which.max(file.info(matched)$mtime)]
  } else {
    warning("No file matched run_id '", run_id, "'; using newest file.")
    llm_file <- llm_files[which.max(file.info(llm_files)$mtime)]
  }
} else {
  llm_file <- llm_files[which.max(file.info(llm_files)$mtime)]
}

message("Using LLM indicator file: ", basename(llm_file))
llm_dt <- fread(llm_file, colClasses = "character")

if (!all(c("code", "bec") %in% names(llm_dt))) {
  stop("LLM file must have columns 'code' and 'bec'.")
}

llm_dt[, code := stringr::str_pad(code, 6, pad = "0")]
llm_dt[, bec := tolower(trimws(bec))]
llm_dt <- llm_dt[bec %in% valid_bec]

# ────────────────────────────────────────────────────────────────────
# Merge and compare
# ────────────────────────────────────────────────────────────────────
merged <- merge(
  llm_dt[, .(code, bec_llm = bec)],
  benchmark[, .(code = hs6_code, bec_benchmark = bec_category)],
  by = "code"
)

n_total <- nrow(merged)
if (n_total == 0) {
  stop("No overlapping HS codes between LLM output and benchmark. Check code formats.")
}

message("Overlapping codes for validation: ", n_total)

merged[, match := bec_llm == bec_benchmark]
agreement_rate <- mean(merged$match)

# Cohen's kappa
kappa_cohen <- function(pred, actual, levels) {
  tab <- table(factor(pred, levels = levels), factor(actual, levels = levels))
  n <- sum(tab)
  p_o <- sum(diag(tab)) / n
  p_e <- sum(rowSums(tab) / n * colSums(tab) / n)
  if (p_e == 1) return(1.0)
  (p_o - p_e) / (1 - p_e)
}

kappa <- kappa_cohen(merged$bec_llm, merged$bec_benchmark, valid_bec)

# Per-class accuracy
per_class <- merged[, .(
  n_benchmark = .N,
  n_correct   = sum(match),
  accuracy    = mean(match)
), by = .(bec_benchmark)]

# Confusion matrix counts
conf_mat <- merged[, .N, by = .(bec_llm, bec_benchmark)]
conf_wide <- dcast(conf_mat, bec_llm ~ bec_benchmark, value.var = "N", fill = 0L)

# ────────────────────────────────────────────────────────────────────
# Outputs
# ────────────────────────────────────────────────────────────────────
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)

## Validation report
report_file <- "output/metrics/bec_validation_report.md"
sink(report_file)
cat("# BEC Validation Report\n\n")
cat(sprintf("- **LLM file**: %s\n", basename(llm_file)))
cat(sprintf("- **Benchmark**: %s\n", benchmark_file))
cat(sprintf("- **Overlapping HS codes**: %d\n", n_total))
cat(sprintf("- **Overall agreement rate**: %.1f%%\n", 100 * agreement_rate))
cat(sprintf("- **Cohen's kappa**: %.3f\n\n", kappa))

cat("## Per-class accuracy\n\n")
cat("| BEC category | N (benchmark) | N correct | Accuracy |\n")
cat("|--------------|--------------|-----------|----------|\n")
for (i in seq_len(nrow(per_class))) {
  r <- per_class[i]
  cat(sprintf("| %s | %d | %d | %.1f%% |\n",
              r$bec_benchmark, r$n_benchmark, r$n_correct, 100 * r$accuracy))
}

cat("\n## Confusion matrix (rows = LLM prediction, cols = benchmark)\n\n")
col_order <- c("bec_llm", valid_bec)
col_order <- col_order[col_order %in% names(conf_wide)]
cat("| LLM \\ Benchmark |", paste(valid_bec, collapse = " | "), "|\n")
cat("|", paste(rep("---", length(valid_bec) + 1), collapse = " | "), "|\n")
for (row_cat in valid_bec) {
  row_data <- conf_wide[bec_llm == row_cat]
  counts <- vapply(valid_bec, function(col) {
    if (nrow(row_data) == 0 || !(col %in% names(row_data))) 0L
    else as.integer(row_data[[col]])
  }, integer(1))
  cat(sprintf("| %s | %s |\n", row_cat, paste(counts, collapse = " | ")))
}
sink()
message("Report saved: ", report_file)

## Confusion matrix CSV
fwrite(conf_wide, "output/metrics/bec_confusion_matrix.csv")
message("Confusion matrix saved: output/metrics/bec_confusion_matrix.csv")

## Confusion heatmap plot
plot_conf <- conf_mat[, .(bec_llm, bec_benchmark, N)]
plot_conf[, bec_llm       := factor(bec_llm,       levels = rev(valid_bec))]
plot_conf[, bec_benchmark := factor(bec_benchmark, levels = valid_bec)]

# Normalise by benchmark column total for recall-style heatmap
col_totals <- plot_conf[, .(col_total = sum(N)), by = bec_benchmark]
plot_conf <- merge(plot_conf, col_totals, by = "bec_benchmark")
plot_conf[, pct := N / col_total]

p_conf <- ggplot(plot_conf, aes(x = bec_benchmark, y = bec_llm, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%\n(n=%d)", 100 * pct, N)),
            size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#f7f7f7", mid = "#74c476", high = "#005a32",
                       midpoint = 0.5, limits = c(0, 1),
                       labels = scales::percent_format()) +
  labs(
    x = "Benchmark (UN BEC)",
    y = "LLM Prediction",
    fill = "Recall",
    title = "BEC Validation: Confusion Matrix",
    subtitle = sprintf("Agreement = %.1f%%, Cohen's kappa = %.3f  (N = %d)",
                       100 * agreement_rate, kappa, n_total)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 11),
    panel.grid = element_blank()
  )

ggsave("output/analysis/figures/bec_validation_confusion.png",
       p_conf, width = 7, height = 5, dpi = 300)
message("Confusion plot saved: output/analysis/figures/bec_validation_confusion.png")

message("Validation complete.")
