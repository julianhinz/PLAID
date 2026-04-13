# code/create_hs_hazmat/05_validate_hazardous.R
# -------------------------------------------------------------------
# Chapter-level face validity for the hazardous flag.
#
# Expected hazardous chapters: 27 (mineral fuels), 28 (inorganic chem),
#   29 (organic chem), 31 (fertilizers), 36 (explosives), 38 (misc chem).
# Expected non-hazardous chapters: 41-46 (hides, leather, furs, wood,
#   cork, straw), 61-67 (apparel, headgear), 94 (furniture), 95 (toys).
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr, knitr)

if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

source("code/common/plot_chapter_shares.R")

# ── Load consensus hazmat CSV at H5 ────────────────────────────────
consensus <- fread("output/database/PLAID_v0.1_hazmat_H5.csv.gz")
consensus[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
consensus[, chapter := substr(hs6_code, 1, 2)]
consensus[, hazardous := as.logical(hazardous)]

# ── Chapter group classification ───────────────────────────────────
expected_high <- c("27", "28", "29", "31", "36", "38")
expected_low  <- c("41", "42", "43", "44", "45", "46",
                   "61", "62", "63", "64", "65", "66", "67",
                   "94", "95")

consensus[, chapter_group := fcase(
  chapter %in% expected_high, "Expected hazardous (27-38)",
  chapter %in% expected_low,  "Expected non-hazardous",
  default = "Other"
)]

# ── Chapter-group summary ──────────────────────────────────────────
grp_summary <- consensus[,
  .(n_codes = .N,
    hazardous_share = mean(hazardous, na.rm = TRUE)),
  by = chapter_group][order(-hazardous_share)]

cat("\n=== Chapter-group hazardous share ===\n")
print(grp_summary)

# ── Per-chapter detail for the figure ──────────────────────────────
chap_summary <- consensus[,
  .(n_codes = .N,
    hazardous_share = mean(hazardous, na.rm = TRUE)),
  by = .(chapter, chapter_group)]
chap_summary <- chap_summary[chapter_group != "Other"]
chap_summary[, label := hs_chapter_label(chapter)]
# Sort by share so expected-high chapters bubble to the top of the plot
chap_summary <- chap_summary[order(-hazardous_share, chapter)]

# Build a plot-ready data.frame: stacked bar (hazardous vs non-hazardous share) by chapter
plot_df <- rbind(
  chap_summary[, .(chapter_group = label,
                   category = "hazardous", share = hazardous_share)],
  chap_summary[, .(chapter_group = label,
                   category = "non-hazardous", share = 1 - hazardous_share)]
)
plot_df[, chapter_group := factor(chapter_group, levels = rev(chap_summary$label))]

p <- plot_chapter_shares(plot_df, indicator = "hazmat",
                         category_levels = c("non-hazardous", "hazardous"),
                         title = "Hazardous flag share by HS chapter")
save_chapter_shares(p, "output/analysis/figures/hazmat_by_chapter.pdf",
                    width = 7, height = 6)

# ── LaTeX summary table ────────────────────────────────────────────
tex_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "\\textbf{Chapter group} & \\textbf{N codes} & \\textbf{Hazardous share} \\\\",
  "\\midrule",
  apply(grp_summary, 1, function(r) {
    sprintf("%s & %s & %.1f\\%% \\\\", r[["chapter_group"]], r[["n_codes"]],
            100 * as.numeric(r[["hazardous_share"]]))
  }),
  "\\bottomrule",
  "\\end{tabular}"
)
dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
writeLines(tex_lines, "output/analysis/tables/hazmat_chapter_face_validity.tex")
message("Wrote: output/analysis/tables/hazmat_chapter_face_validity.tex")

# ── Markdown report ────────────────────────────────────────────────
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)
md_lines <- c(
  "# Hazardous materials chapter face validity",
  "",
  "## Expected-high chapter group (27, 28, 29, 31, 36, 38)",
  sprintf("Mean hazardous share: %.1f%%",
          100 * grp_summary[chapter_group == "Expected hazardous (27-38)"]$hazardous_share),
  "",
  "## Expected-low chapter group",
  sprintf("Mean hazardous share: %.1f%%",
          100 * grp_summary[chapter_group == "Expected non-hazardous"]$hazardous_share),
  "",
  "## All chapter groups",
  knitr::kable(grp_summary, format = "markdown") |> paste(collapse = "\n"),
  "",
  "## Interpretation",
  "A correctly calibrated hazardous flag should exhibit high share in the",
  "expected-high chapter group and near-zero share in the expected-low group.",
  "Monotonic ordering across groups is the principal face-validity test."
)
writeLines(md_lines, "output/metrics/hazmat_validation_report.md")
message("Wrote: output/metrics/hazmat_validation_report.md")
