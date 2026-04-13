# code/create_hs_bec/06_validate_bec.R
# -------------------------------------------------------------------
# BEC validation: H4-primary against UNSD BEC Rev.5 -> HS 2012 native
# concordance, with H4/H5/H6 cross-revision extension.
#
# Primary reference: input/benchmarks/bec_hs2012_concordance.csv (native)
# Cross-revision reference: input/benchmarks/bec_hs2017_concordance.csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)
source("code/common/plot_confusion_matrix.R")

if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

# ── Helper: normalize BEC5 category string → three-way end-use ─────
# Handles both short codes (CAP/INT/CONS) and already-spelled-out labels
# (capital/intermediate/consumption).
bec5_to_enduse <- function(x) {
  x <- toupper(as.character(x))
  fcase(
    startsWith(x, "CAP"),  "capital",
    startsWith(x, "INT"),  "intermediate",
    startsWith(x, "CONS"), "consumption",
    default = NA_character_
  )
}

# ── Load benchmarks ────────────────────────────────────────────────
h2012 <- fread("input/benchmarks/bec_hs2012_concordance.csv")
h2012[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
# H2012 already has bec_enduse column from prepare_benchmarks.R
stopifnot("bec_enduse" %in% names(h2012))
h2012_ref <- h2012[!is.na(bec_enduse), .(hs6_code, bench = bec_enduse)]

h2017 <- fread("input/benchmarks/bec_hs2017_concordance.csv")
h2017[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
h2017[, bench := bec5_to_enduse(bec_category)]
h2017_ref <- h2017[!is.na(bench), .(hs6_code, bench)]

# ── Helper: compute agreement for a given revision + benchmark ─────
evaluate_rev <- function(rev, bench_dt, label) {
  db_path <- sprintf("output/database/PLAID_v0.1_bec_%s.csv.gz", rev)
  if (!file.exists(db_path)) {
    warning("Missing database file: ", db_path)
    return(NULL)
  }
  cons <- fread(db_path)
  cons[, hs6_code := str_pad(as.character(hs6_code), 6, pad = "0")]
  bec_col <- intersect(c("bec", "bec_consensus", "consensus"), names(cons))
  if (length(bec_col) == 0) {
    stop("No bec consensus column in ", db_path,
         "; columns: ", paste(names(cons), collapse = ", "))
  }
  cons[, llm := cons[[bec_col[1]]]]
  m <- merge(cons[, .(hs6_code, llm)], bench_dt, by = "hs6_code")
  m <- m[!is.na(llm) & !is.na(bench)]
  list(
    revision = rev,
    benchmark = label,
    n = nrow(m),
    overall = mean(m$llm == m$bench),
    data = m
  )
}

# ── Primary: H4 vs native H2012 ────────────────────────────────────
primary <- evaluate_rev("H4", h2012_ref, "BEC Rev.5 / HS 2012 (native)")
stopifnot(!is.null(primary))
merged_primary <- primary$data

# Cohen's kappa
labels_bec <- c("capital", "intermediate", "consumption")
tab <- table(
  factor(merged_primary$bench, levels = labels_bec),
  factor(merged_primary$llm,   levels = labels_bec)
)
po <- sum(diag(tab)) / sum(tab)
pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
kappa <- (po - pe) / (1 - pe)

per_class <- sapply(labels_bec, function(cl) {
  sub <- merged_primary[bench == cl]
  if (nrow(sub) == 0) NA_real_ else mean(sub$llm == cl)
})

# ── Confusion matrix (primary) ────────────────────────────────────
cm_df <- merged_primary[, .N, by = .(reference = bench, predicted = llm)]
setnames(cm_df, "N", "n")
p_cm <- plot_confusion_matrix(cm_df, indicator = "bec",
                              labels = labels_bec)
save_confusion_matrix(p_cm, "output/analysis/figures/bec_confusion_matrix.pdf",
                      n_levels = 3)

# ── Cross-revision extension: H4, H5, H6 ──────────────────────────
rev_results <- list(
  evaluate_rev("H4", h2012_ref, "HS 2012 native"),
  evaluate_rev("H5", h2017_ref, "HS 2017"),
  evaluate_rev("H6", h2017_ref, "HS 2017 (crosswalked forward)")
)
rev_results <- Filter(Negate(is.null), rev_results)

per_rev <- rbindlist(lapply(rev_results, function(r) {
  data.table(revision = r$revision, benchmark = r$benchmark,
             n = r$n, agreement = r$overall)
}))

# ── Markdown report ───────────────────────────────────────────────
md_lines <- c(
  "# BEC validation report",
  "",
  "## Primary: H4 (HS 2012) vs. BEC Rev.5 native",
  sprintf("- N matched: %d", primary$n),
  sprintf("- Overall agreement: %.1f%%", 100 * primary$overall),
  sprintf("- Cohen's kappa: %.3f", kappa),
  "- Per-class recall:",
  sprintf("  - capital: %.1f%%", 100 * per_class[["capital"]]),
  sprintf("  - intermediate: %.1f%%", 100 * per_class[["intermediate"]]),
  sprintf("  - consumption: %.1f%%", 100 * per_class[["consumption"]]),
  "",
  "## Cross-revision extension",
  unlist(lapply(seq_len(nrow(per_rev)), function(i) {
    r <- per_rev[i]
    sprintf("- %s (%s): %.1f%% (N=%d)",
            r$revision, r$benchmark, 100 * r$agreement, r$n)
  }))
)

dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)
writeLines(md_lines, "output/metrics/bec_validation_report.md")
message("Wrote: output/metrics/bec_validation_report.md")

# ── LaTeX: primary H4 table ───────────────────────────────────────
tex_primary <- c(
  "\\begin{tabular}{lc}",
  "\\toprule",
  "\\multicolumn{2}{c}{\\textbf{BEC validation at H4 (native HS 2012)}} \\\\",
  "\\midrule",
  sprintf("N matched & %d \\\\", primary$n),
  sprintf("Overall agreement & %.1f\\%% \\\\", 100 * primary$overall),
  "\\midrule",
  "\\multicolumn{2}{l}{\\textit{Per-category agreement}} \\\\",
  sprintf("\\quad Capital & %.1f\\%% \\\\", 100 * per_class[["capital"]]),
  sprintf("\\quad Intermediate & %.1f\\%% \\\\", 100 * per_class[["intermediate"]]),
  sprintf("\\quad Consumption & %.1f\\%% \\\\", 100 * per_class[["consumption"]]),
  "\\bottomrule",
  "\\end{tabular}"
)
dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
writeLines(tex_primary, "output/analysis/tables/bec_validation_h4.tex")
message("Wrote: output/analysis/tables/bec_validation_h4.tex")

# ── LaTeX: cross-revision extension ───────────────────────────────
tex_rev_lines <- c(
  "\\begin{tabular}{llcc}",
  "\\toprule",
  "\\textbf{Revision} & \\textbf{Benchmark} & \\textbf{N} & \\textbf{Agreement} \\\\",
  "\\midrule",
  unlist(lapply(seq_len(nrow(per_rev)), function(i) {
    r <- per_rev[i]
    sprintf("%s & %s & %d & %.1f\\%% \\\\",
            r$revision, r$benchmark, r$n, 100 * r$agreement)
  })),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex_rev_lines, "output/analysis/tables/bec_agreement_by_revision.tex")
message("Wrote: output/analysis/tables/bec_agreement_by_revision.tex")

cat("\n=== BEC validation summary ===\n")
cat("Primary revision: H4 (HS 2012 native benchmark)\n")
cat(sprintf("Overall agreement: %.1f%% (N=%d)\n", 100 * primary$overall, primary$n))
cat(sprintf("Cohen's kappa: %.3f\n", kappa))
cat("Per-class recall:\n")
cat(sprintf("  capital:      %.1f%%\n", 100 * per_class[["capital"]]))
cat(sprintf("  intermediate: %.1f%%\n", 100 * per_class[["intermediate"]]))
cat(sprintf("  consumption:  %.1f%%\n", 100 * per_class[["consumption"]]))
cat("\nCross-revision extension:\n")
print(per_rev)
