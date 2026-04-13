# code/recreate_rauch_1995/11_compare_pilot_to_rauch.R
# -------------------------------------------------------------------
# Compare 2-model SITC Rev.2 pilot consensus against Rauch (1999)
# conservative and liberal variants. Produces:
#   - output/metrics/rauch_sitc_primary_validation.md
#   - output/analysis/tables/rauch_sitc_validation.tex  (label: tab:rauch_validation)
#   - output/analysis/figures/rauch_confusion_matrix.pdf (via helper)
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)

if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

source("code/common/plot_confusion_matrix.R")

# Define null-coalesce helper EARLY so downstream lookups work.
`%||%` <- function(x, y) if (is.null(x) || is.na(x)) y else x

# ── Load pilot consensus ────────────────────────────────────────────
pilot <- fread("output/indicators/rauch_sitc2_aggregated_pilot_20260410.csv")
pilot[, code4 := str_pad(as.character(code), 4, pad = "0")]

# ── Load Rauch (1999) conservative + liberal ────────────────────────
rauch_path <- "temp/rauch/Rauch_classification_revised.csv"
if (!file.exists(rauch_path)) {
  dir.create(dirname(rauch_path), recursive = TRUE, showWarnings = FALSE)
  download.file("https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv",
                rauch_path, mode = "wb")
}
rauch <- fread(rauch_path)
rauch[, code4 := str_pad(as.character(sitc4), 4, pad = "0")]
rauch_u <- unique(rauch[, .(code4, con, lib)])

# ── Merge ───────────────────────────────────────────────────────────
merged <- merge(pilot, rauch_u, by = "code4", all.x = TRUE)
cat("Merged rows:", nrow(merged), "\n")
cat("With conservative Rauch label:", sum(!is.na(merged$con)), "\n")
cat("With liberal Rauch label:", sum(!is.na(merged$lib)), "\n")

# ── Compute agreement, per-class precision/recall ──────────────────
compute_agreement <- function(dt, ref_col) {
  ok <- dt[!is.na(dt[[ref_col]]) & !is.na(dt$rauch_consensus)]
  classes <- c("w", "r", "n")
  per_class_recall <- setNames(sapply(classes, function(cl) {
    sub <- ok[ok[[ref_col]] == cl]
    if (nrow(sub) == 0) NA_real_ else mean(sub$rauch_consensus == cl)
  }), classes)
  per_class_precision <- setNames(sapply(classes, function(cl) {
    sub <- ok[rauch_consensus == cl]
    if (nrow(sub) == 0) NA_real_ else mean(sub[[ref_col]] == cl)
  }), classes)
  list(
    n = nrow(ok),
    overall = mean(ok$rauch_consensus == ok[[ref_col]]),
    per_class_recall = per_class_recall,
    per_class_precision = per_class_precision
  )
}

cons_result <- compute_agreement(merged, "con")
lib_result  <- compute_agreement(merged, "lib")

pilot_agreement <- mean(pilot$agreement, na.rm = TRUE)

# ── Confusion matrix vs conservative ───────────────────────────────
cm_df <- merged[!is.na(con) & !is.na(rauch_consensus),
                .N, by = .(reference = con, predicted = rauch_consensus)]
setnames(cm_df, "N", "n")

p_cm <- plot_confusion_matrix(cm_df, indicator = "rauch", labels = c("w", "r", "n"))
save_confusion_matrix(p_cm,
                      "output/analysis/figures/rauch_confusion_matrix.pdf",
                      n_levels = 3)

# ── Markdown report ────────────────────────────────────────────────
md_lines <- c(
  "# Rauch SITC Rev.2 primary validation (2-model pilot)",
  "",
  "**Models:** openai/gpt-5 (Nov 2025), anthropic/claude-3.5-sonnet (Nov 2025)",
  sprintf("**Pilot inter-model agreement:** %.1f%% (N=%d)",
          100 * pilot_agreement, nrow(pilot)),
  "",
  "## Agreement with Rauch (1999) — conservative variant",
  sprintf("- N matched: %d", cons_result$n),
  sprintf("- Overall accuracy: %.1f%%", 100 * cons_result$overall),
  "- Per-class recall:",
  sprintf("  - w (organized exchange): %.1f%%", 100 * (cons_result$per_class_recall[["w"]] %||% NA_real_)),
  sprintf("  - r (reference priced): %.1f%%", 100 * (cons_result$per_class_recall[["r"]] %||% NA_real_)),
  sprintf("  - n (differentiated): %.1f%%", 100 * (cons_result$per_class_recall[["n"]] %||% NA_real_)),
  "- Per-class precision:",
  sprintf("  - w: %.1f%%", 100 * (cons_result$per_class_precision[["w"]] %||% NA_real_)),
  sprintf("  - r: %.1f%%", 100 * (cons_result$per_class_precision[["r"]] %||% NA_real_)),
  sprintf("  - n: %.1f%%", 100 * (cons_result$per_class_precision[["n"]] %||% NA_real_)),
  "",
  "## Agreement with Rauch (1999) — liberal variant",
  sprintf("- N matched: %d", lib_result$n),
  sprintf("- Overall accuracy: %.1f%%", 100 * lib_result$overall),
  "- Per-class recall:",
  sprintf("  - w: %.1f%%", 100 * (lib_result$per_class_recall[["w"]] %||% NA_real_)),
  sprintf("  - r: %.1f%%", 100 * (lib_result$per_class_recall[["r"]] %||% NA_real_)),
  sprintf("  - n: %.1f%%", 100 * (lib_result$per_class_recall[["n"]] %||% NA_real_))
)

dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)
writeLines(md_lines, "output/metrics/rauch_sitc_primary_validation.md")
cat("Wrote: output/metrics/rauch_sitc_primary_validation.md\n")

# ── LaTeX table (label: tab:rauch_validation) ──────────────────────
fmt_pct <- function(x) {
  if (is.null(x) || is.na(x)) "--" else sprintf("%.1f\\%%", 100 * x)
}
tex_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\textbf{Conservative} & \\textbf{Liberal} \\\\",
  "\\midrule",
  sprintf("N matched & %d & %d \\\\", cons_result$n, lib_result$n),
  sprintf("Overall agreement & %s & %s \\\\",
          fmt_pct(cons_result$overall), fmt_pct(lib_result$overall)),
  "\\midrule",
  "\\multicolumn{3}{l}{\\textit{Per-category agreement}} \\\\",
  sprintf("\\quad w (organized exchange) & %s & %s \\\\",
          fmt_pct(cons_result$per_class_recall[["w"]]),
          fmt_pct(lib_result$per_class_recall[["w"]])),
  sprintf("\\quad r (reference priced) & %s & %s \\\\",
          fmt_pct(cons_result$per_class_recall[["r"]]),
          fmt_pct(lib_result$per_class_recall[["r"]])),
  sprintf("\\quad n (differentiated) & %s & %s \\\\",
          fmt_pct(cons_result$per_class_recall[["n"]]),
          fmt_pct(lib_result$per_class_recall[["n"]])),
  "\\midrule",
  sprintf("Pilot inter-model agreement & \\multicolumn{2}{c}{%s} \\\\",
          fmt_pct(pilot_agreement)),
  "\\bottomrule",
  "\\end{tabular}"
)

dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
writeLines(tex_lines, "output/analysis/tables/rauch_sitc_validation.tex")
cat("Wrote: output/analysis/tables/rauch_sitc_validation.tex\n")

cat("\n=== Summary ===\n")
cat("Conservative accuracy:", round(100 * cons_result$overall, 1), "%\n")
cat("Liberal accuracy:", round(100 * lib_result$overall, 1), "%\n")
cat("Inter-model agreement:", round(100 * pilot_agreement, 1), "%\n")
