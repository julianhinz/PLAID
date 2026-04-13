#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 05_validate.R — Perishability indicator validation
# -------------------------------------------------------------------
# Cross-validates perishability classifications against HS chapter
# expectations and optionally against SPS notification frequency.
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr, ggplot2)

args <- commandArgs(trailingOnly = TRUE)

# Find latest perishability indicator file
ind_dir <- "output/indicators"
perish_files <- list.files(ind_dir, pattern = "^perishability_hs6_.*\\.csv$", full.names = TRUE)
if (length(perish_files) == 0) stop("No perishability indicator files found.")
perish_file <- if (length(args) >= 1) args[[1]] else perish_files[which.max(file.info(perish_files)$mtime)]
message("LLM indicators: ", perish_file)

dt <- fread(perish_file)
dt[, code := str_pad(as.character(code), 6, pad = "0")]
dt[, chapter := as.integer(substr(code, 1, 2))]

# Expected perishability by chapter group
dt[, chapter_group := fcase(
  chapter <= 24, "Agriculture & food (01-24)",
  chapter <= 40, "Chemicals & plastics (25-40)",
  chapter <= 67, "Textiles & misc mfg (41-67)",
  chapter <= 83, "Metals (68-83)",
  chapter <= 97, "Machinery & transport (84-97)",
  default = "Other"
)]

# Summary by chapter group
summary_dt <- dt[!is.na(perishability_class),
  .(mean_class = mean(perishability_class),
    median_class = as.double(median(perishability_class)),
    pct_class_1_2 = mean(perishability_class <= 2),
    n = .N),
  by = chapter_group]

message("\nPerishability by chapter group:")
print(summary_dt)

# Validation: agriculture should be more perishable than metals
agri_mean <- summary_dt[grepl("Agriculture", chapter_group), mean_class]
metal_mean <- summary_dt[grepl("Metal", chapter_group), mean_class]
message(sprintf("\nSanity check: Agriculture mean class = %.2f, Metals mean class = %.2f", agri_mean, metal_mean))
if (agri_mean >= metal_mean) warning("Agriculture is NOT more perishable than metals!")

# Save outputs
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)

fwrite(summary_dt, "output/metrics/perishability_validation_by_chapter.csv")

# Distribution plot
ggplot(dt[!is.na(perishability_class)],
       aes(x = factor(perishability_class), fill = chapter_group)) +
  geom_bar(position = "fill") +
  labs(title = "Perishability Class Distribution by HS Chapter Group",
       x = "Perishability class", y = "Share", fill = "Chapter group") +
  theme_minimal()
ggsave("output/analysis/figures/perishability_validation_by_chapter.png",
       width = 9, height = 5, dpi = 200)

# Optional SPS benchmark
sps_file <- "input/benchmarks/sps_hs_notifications.csv"
if (file.exists(sps_file)) {
  sps <- fread(sps_file)
  sps[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
  merged <- merge(dt, sps, by.x = "code", by.y = "hs6_code", all.x = FALSE)
  if (nrow(merged) > 0) {
    cor_val <- cor(merged$perishability_class, merged$sps_count, use = "complete.obs")
    message(sprintf("Correlation(perishability_class, SPS notifications) = %.3f", cor_val))
  }
} else {
  message("No SPS benchmark found at ", sps_file, " — skipping SPS validation.")
}

report <- sprintf(
  "Perishability Validation Report\n================================\nIndicator file: %s\nTotal codes: %d\nAgriculture mean class: %.2f\nMetals mean class: %.2f\n",
  basename(perish_file), nrow(dt[!is.na(perishability_class)]), agri_mean, metal_mean
)
writeLines(report, "output/metrics/perishability_validation_report.md")
message("Validation complete.")
