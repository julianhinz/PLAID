#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_validate.R
# -------------------------------------------------------------------
# Validates HS microchip content classifications against OECD ICT goods list.
#
# Key asymmetry: OECD ICT is NARROWER than microchip_content.
#   - OECD ICT = primarily dedicated ICT goods (computers, phones, telecom)
#   - microchip_content = also includes vehicles with ECUs, smart appliances,
#     medical devices, industrial machinery with embedded chips
#   => LLM microchip=TRUE should be a SUPERSET of OECD ICT=TRUE.
#
# Benchmark: input/benchmarks/oecd_ict_hs.csv
#   Columns: hs6_code, ict_good
#
# Outputs:
#   output/metrics/microchip_validation_report.md
#   output/metrics/microchip_confusion_matrix.csv
#   output/metrics/microchip_confusion_matrix.png
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr, ggplot2, optparse)

# ────────────────────────────────────────────────────────────────────
# Args
# ────────────────────────────────────────────────────────────────────
default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-5"

option_list <- list(
  make_option(c("-m","--model"),  type = "character", default = default_model,
              help = "Model id used to generate indicators. [default: %default]"),
  make_option(c("-f","--file"),   type = "character", default = NA_character_,
              help = "Path to microchip indicator CSV. If omitted, uses latest in output/indicators/.")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog [options] [MODEL] [INDICATOR_FILE]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

posargs    <- parsed$args
model      <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
file_override <- parsed$options$file

# ────────────────────────────────────────────────────────────────────
# Load microchip indicator
# ────────────────────────────────────────────────────────────────────
if (!is.na(file_override)) {
  indicator_file <- file_override
  if (!file.exists(indicator_file)) stop("Indicator file not found: ", indicator_file)
} else {
  safe_model <- gsub("[^A-Za-z0-9]+", "-", model)
  cand <- list.files(
    file.path("output", "indicators"),
    pattern = paste0("^microchip_hs6_.*", safe_model, ".*\\.csv$"),
    full.names = TRUE
  )
  if (length(cand) == 0) {
    # Fall back to any microchip indicator
    cand <- list.files(
      file.path("output", "indicators"),
      pattern = "^microchip_hs6_.*\\.csv$",
      full.names = TRUE
    )
  }
  if (length(cand) == 0) stop("No microchip_hs6_*.csv found in output/indicators/. Run pipeline first.")
  indicator_file <- cand[which.max(file.info(cand)$mtime)]
}

message("Loading microchip indicator: ", indicator_file)
llm_dt <- fread(indicator_file, colClasses = "character")

if (!all(c("code", "microchip_content") %in% names(llm_dt))) {
  stop("Indicator file must have columns 'code' and 'microchip_content'.")
}

llm_dt[, hs6_code       := str_pad(code, 6, pad = "0")]
llm_dt[, llm_microchip  := as.logical(microchip_content)]
llm_dt <- llm_dt[!is.na(llm_microchip), .(hs6_code, llm_microchip, confidence)]

message("LLM microchip records: ", nrow(llm_dt))

# ────────────────────────────────────────────────────────────────────
# Load OECD ICT benchmark
# ────────────────────────────────────────────────────────────────────
bench_file <- file.path("input", "benchmarks", "oecd_ict_hs.csv")
if (!file.exists(bench_file)) stop("Benchmark file not found: ", bench_file)

bench_dt <- fread(bench_file, colClasses = "character")
if (!all(c("hs6_code", "ict_good") %in% names(bench_dt))) {
  stop("Benchmark file must have columns 'hs6_code' and 'ict_good'.")
}

bench_dt[, hs6_code  := str_pad(hs6_code, 6, pad = "0")]
bench_dt[, ict_good  := as.logical(ict_good)]
bench_dt <- bench_dt[!is.na(ict_good), .(hs6_code, ict_good)]

message("OECD ICT benchmark records: ", nrow(bench_dt))

# ────────────────────────────────────────────────────────────────────
# Merge
# ────────────────────────────────────────────────────────────────────
merged <- merge(llm_dt, bench_dt, by = "hs6_code", all = FALSE)
n_matched <- nrow(merged)
message("Matched codes: ", n_matched)

if (n_matched == 0) stop("No codes matched between indicator and benchmark.")

# ────────────────────────────────────────────────────────────────────
# Confusion matrix
# ────────────────────────────────────────────────────────────────────
# Rows = LLM prediction, Cols = OECD ICT truth
conf <- merged[, .N, by = .(llm_microchip, ict_good)]
conf[, llm_label  := ifelse(llm_microchip, "LLM: chip", "LLM: no chip")]
conf[, oecd_label := ifelse(ict_good,      "OECD: ICT", "OECD: non-ICT")]

conf_wide <- dcast(conf, llm_label ~ oecd_label, value.var = "N", fill = 0L)

# Key metrics
tp <- merged[llm_microchip == TRUE  & ict_good == TRUE,  .N]   # both true
fp <- merged[llm_microchip == TRUE  & ict_good == FALSE, .N]   # LLM=chip, OECD=non-ICT (expected: many — embedded chips in non-ICT goods)
fn <- merged[llm_microchip == FALSE & ict_good == TRUE,  .N]   # LLM=no chip, OECD=ICT (these are errors)
tn <- merged[llm_microchip == FALSE & ict_good == FALSE, .N]   # both false

precision  <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
recall     <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
f1         <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
                2 * precision * recall / (precision + recall)
              } else NA_real_
accuracy   <- (tp + tn) / n_matched

# Asymmetry: share of LLM=TRUE that are OECD non-ICT (expected high = embedded chips)
superset_rate <- if ((tp + fp) > 0) fp / (tp + fp) else NA_real_
# Share of OECD ICT captured by LLM (recall/coverage)
ict_coverage <- recall

# ────────────────────────────────────────────────────────────────────
# Save confusion matrix CSV
# ────────────────────────────────────────────────────────────────────
out_metrics <- file.path("output", "metrics")
dir.create(out_metrics, recursive = TRUE, showWarnings = FALSE)

fwrite(conf_wide, file.path(out_metrics, "microchip_confusion_matrix.csv"))

# ────────────────────────────────────────────────────────────────────
# Confusion matrix plot
# ────────────────────────────────────────────────────────────────────
conf_plot_dt <- copy(conf)
conf_plot_dt[, llm_label  := factor(llm_label,  levels = c("LLM: chip", "LLM: no chip"))]
conf_plot_dt[, oecd_label := factor(oecd_label, levels = c("OECD: non-ICT", "OECD: ICT"))]

p_conf <- ggplot(conf_plot_dt, aes(x = oecd_label, y = llm_label, fill = N)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = N), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "#f7f7f7", high = "#2166ac") +
  labs(
    title    = "Microchip Content: LLM vs OECD ICT Benchmark",
    subtitle = paste0("Note: OECD ICT is NARROWER than microchip_content.\n",
                      "Many LLM=chip / OECD=non-ICT cases are expected (vehicles, appliances with embedded chips)."),
    x = "OECD ICT benchmark",
    y = "LLM microchip classification",
    fill = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text      = element_text(size = 11),
    plot.subtitle  = element_text(size = 9, color = "gray40")
  )

ggsave(file.path(out_metrics, "microchip_confusion_matrix.png"),
       p_conf, width = 7, height = 5, dpi = 300)

# ────────────────────────────────────────────────────────────────────
# Distribution by confidence
# ────────────────────────────────────────────────────────────────────
merged[, conf_num := suppressWarnings(as.numeric(confidence))]
conf_by_class <- merged[, .(
  n       = .N,
  mean_confidence = mean(conf_num, na.rm = TRUE),
  pct_agree = mean(llm_microchip == ict_good, na.rm = TRUE) * 100
), by = .(llm_microchip, ict_good)]

# ────────────────────────────────────────────────────────────────────
# Write report
# ────────────────────────────────────────────────────────────────────
report_path <- file.path(out_metrics, "microchip_validation_report.md")
sink(report_path)
cat("# Microchip Content Classification: Validation Report\n\n")
cat(sprintf("**Indicator file:** `%s`\n\n", basename(indicator_file)))
cat(sprintf("**Benchmark:** `input/benchmarks/oecd_ict_hs.csv`\n\n"))
cat(sprintf("**Matched HS-6 codes:** %d\n\n", n_matched))

cat("## Asymmetry Note\n\n")
cat("OECD ICT goods list is **narrower** than microchip content:\n\n")
cat("- OECD ICT = dedicated ICT products (computers, phones, telecom equipment)\n")
cat("- microchip_content = also includes goods with *embedded* semiconductors (vehicles with ECUs,\n")
cat("  smart appliances, medical devices, industrial robots)\n\n")
cat("**Expected pattern:** LLM=chip should be a superset of OECD ICT=TRUE.\n")
cat("High `fp` (LLM=chip, OECD=non-ICT) is NOT a model error — it reflects embedded chip goods.\n\n")

cat("## Confusion Matrix\n\n")
cat("| | OECD: ICT | OECD: non-ICT |\n")
cat("|---|---|---|\n")
cat(sprintf("| **LLM: chip** | %d (TP) | %d (FP — expected: embedded chips) |\n", tp, fp))
cat(sprintf("| **LLM: no chip** | %d (FN — errors) | %d (TN) |\n", fn, tn))
cat("\n")

cat("## Performance Metrics\n\n")
cat(sprintf("- **Accuracy:** %.1f%%\n", accuracy * 100))
cat(sprintf("- **Precision** (of chip=TRUE, share that are OECD ICT): %.1f%%\n", precision * 100))
cat(sprintf("- **Recall / ICT coverage** (OECD ICT goods classified as chip): %.1f%%\n", recall * 100))
cat(sprintf("- **F1 score (vs OECD ICT):** %.3f\n", f1))
cat("\n")

cat("## Asymmetry Metrics\n\n")
cat(sprintf("- **Superset extension rate:** %.1f%% of LLM=chip codes are OECD non-ICT\n",
            superset_rate * 100))
cat("  (This represents embedded-chip goods not in OECD ICT — expected to be high)\n\n")
cat(sprintf("- **OECD ICT coverage:** %.1f%% of OECD ICT goods are classified as microchip=TRUE\n",
            ict_coverage * 100))
cat("  (This should be close to 100%%; FN here are genuine classification errors)\n\n")

cat("## Cell Breakdown by Confidence\n\n")
cat("| LLM chip | OECD ICT | N | Mean confidence | % agree |\n")
cat("|---|---|---|---|---|\n")
for (i in seq_len(nrow(conf_by_class))) {
  r <- conf_by_class[i]
  cat(sprintf("| %s | %s | %d | %.2f | %.1f%% |\n",
              r$llm_microchip, r$ict_good, r$N, r$mean_confidence, r$pct_agree))
}
cat("\n")

cat("## Outputs\n\n")
cat("- `output/metrics/microchip_confusion_matrix.csv`\n")
cat("- `output/metrics/microchip_confusion_matrix.png`\n")
cat("- `output/metrics/microchip_validation_report.md` (this file)\n")
sink()

message("Validation report written to: ", report_path)
message(sprintf("Accuracy: %.1f%%, Recall (OECD ICT coverage): %.1f%%",
                accuracy * 100, recall * 100))

# -------------------------------------------------------------------
# Chapter face validity (added 2026-04-10 per paper revision)
# -------------------------------------------------------------------
source("code/common/plot_chapter_shares.R")

consensus_mc <- fread("output/database/PLAID_v0.1_microchip_H5.csv.gz")
consensus_mc[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
consensus_mc[, chapter := substr(hs6_code, 1, 2)]
consensus_mc[, microchip := as.logical(microchip_content)]

expected_micro <- c("84", "85", "90")   # machinery, electrical, optical/medical
consensus_mc[, chapter_group := fcase(
  chapter %in% expected_micro, "Expected microchip-bearing (84, 85, 90)",
  default = "Other chapters"
)]

grp_mc <- consensus_mc[,
  .(n_codes = .N, share = mean(microchip, na.rm = TRUE)),
  by = chapter_group][order(-share)]

cat("\n=== Microchip chapter-group share ===\n")
print(grp_mc)

# Per-chapter detail for the figure
chap_mc <- consensus_mc[chapter_group != "Other chapters",
  .(share = mean(microchip, na.rm = TRUE)), by = chapter]
chap_mc[, label := hs_chapter_label(chapter)]
chap_mc <- chap_mc[order(-share, chapter)]

plot_mc_df <- rbind(
  chap_mc[, .(chapter_group = label, category = "with chip", share)],
  chap_mc[, .(chapter_group = label, category = "without chip", share = 1 - share)]
)
plot_mc_df[, chapter_group := factor(chapter_group, levels = rev(chap_mc$label))]

p_mc <- plot_chapter_shares(plot_mc_df, indicator = "microchip",
                            category_levels = c("without chip", "with chip"),
                            title = "Microchip-containing share by HS chapter")
save_chapter_shares(p_mc, "output/analysis/figures/microchip_by_chapter.pdf",
                    width = 6, height = 3)

# LaTeX table
tex_mc <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "\\textbf{Chapter group} & \\textbf{N codes} & \\textbf{Microchip share} \\\\",
  "\\midrule",
  apply(grp_mc, 1, function(r) {
    sprintf("%s & %s & %.1f\\%% \\\\", r[["chapter_group"]], r[["n_codes"]],
            100 * as.numeric(r[["share"]]))
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
writeLines(tex_mc, "output/analysis/tables/microchip_chapter_face_validity.tex")
message("Wrote: output/analysis/tables/microchip_chapter_face_validity.tex")

# -------------------------------------------------------------------
# Confusion matrix PDF (replaces PNG)
# -------------------------------------------------------------------
source("code/common/plot_confusion_matrix.R")

# Reuse the `merged` data.table built earlier in this script.
# It has: hs6_code, llm_microchip (logical), ict_good (logical), confidence.
cm_mc <- merged[, .N, by = .(reference = ict_good, predicted = llm_microchip)]
setnames(cm_mc, "N", "n")
cm_mc[, reference := factor(ifelse(reference, "positive", "negative"),
                            levels = c("positive", "negative"))]
cm_mc[, predicted := factor(ifelse(predicted, "positive", "negative"),
                            levels = c("positive", "negative"))]
p_cm_mc <- plot_confusion_matrix(cm_mc, indicator = "microchip",
                                 labels = c("positive", "negative"),
                                 title = "Microchip LLM vs OECD/UNCTAD ICT")
save_confusion_matrix(p_cm_mc,
                      "output/analysis/figures/microchip_confusion_matrix.pdf",
                      n_levels = 2)
