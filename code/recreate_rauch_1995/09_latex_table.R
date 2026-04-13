#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 09_latex_table.R
# -------------------------------------------------------------------
# Generates the LaTeX validation table (Panels A, B, C) from the
# most recent aggregated indicators file.
#
# Usage:
#   Rscript 09_latex_table.R             # uses latest aggregated CSV
#   Rscript 09_latex_table.R <file.csv>  # explicit aggregated CSV
#
# Output:
#   output/tables/rauch_validation_<stamp>.tex
#
# -------------------------------------------------------------------
# PAPER COMMENT (update when re-running with new data)
# -------------------------------------------------------------------
# Table~\ref{tab:rauch_validation} compares the ensemble classification
# to the Rauch (1999) concordance on the 702 SITC Rev.~2 4-digit codes
# matched in both variants. Overall accuracy is 76.6\% under the
# conservative coding and 72.5\% under the liberal coding. The gap is
# driven almost entirely by the organized-exchange category ($w$): under
# the liberal variant, 38 additional codes are classified as
# exchange-traded, and these are often assigned by the ensemble to the
# adjacent $r$ or $n$ categories. For the two larger categories,
# performance is substantially stronger, with accuracy of 83.2\% and
# 85.9\% for differentiated goods ($n$), and 80.8\% and 75.4\% for
# reference-priced goods ($r$), under the conservative and liberal
# variants respectively.
# Panels~B and~C show that ensemble disagreement is strongly informative
# about classification error. Accuracy declines monotonically with
# disagreement: under the conservative variant, unanimous classifications
# achieve 82.8\% accuracy, compared with 64.3\% for three-model agreement
# and 51.6\% for evenly split cases. The same pattern appears when
# conditioning on correctness. Misclassified products have substantially
# higher mean uncertainty and entropy, and are far less likely to be
# unanimous, than correctly classified products; these differences are
# similar under both concordance variants. This close relationship between
# disagreement and error supports the use of $u_k$ as a practical
# screening device. Restricting attention to unanimous products retains
# 72.8\% of matched codes while increasing conservative accuracy to
# 82.8\%. More generally, the uncertainty measure can be carried forward
# directly into robustness analysis.
# -------------------------------------------------------------------

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  library(data.table)
})

# ── CLI ──────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

.find_ind_dir <- function() {
  wd <- getwd()
  candidates <- c(
    file.path(wd, "output", "indicators"),
    file.path(wd, "code", "recreate_rauch_1995", "output", "indicators")
  )
  hit <- Filter(dir.exists, candidates)
  if (length(hit)) hit[1] else candidates[1]
}
ind_dir <- .find_ind_dir()
base_dir <- dirname(ind_dir)

if (length(args) >= 1 && file.exists(args[1])) {
  agg_file <- args[1]
} else {
  candidates <- list.files(ind_dir, pattern = "^rauch_sitc2_aggregated_.*\\.csv$",
                           full.names = TRUE)
  if (!length(candidates)) stop("No aggregated file found in: ", ind_dir)
  agg_file <- candidates[which.max(file.info(candidates)$mtime)]
}
cat("Using:", agg_file, "\n")

# ── Load ─────────────────────────────────────────────────────────────
dt <- fread(agg_file, showProgress = FALSE)
dt[, u_k := 1 - agreement_rate]

rnd <- function(x, k = 3) formatC(round(x, k), format = "f", digits = k)

# ── Panel A ──────────────────────────────────────────────────────────
cats <- c("n", "r", "w")

pa_con <- dt[!is.na(con) & con %in% cats & !is.na(plurality_label),
             .(N_con = .N, Acc_con = round(mean(plurality_label == con), 3)),
             by = con][order(con)]

pa_lib <- dt[!is.na(lib) & lib %in% cats & !is.na(plurality_label),
             .(N_lib = .N, Acc_lib = round(mean(plurality_label == lib), 3)),
             by = .(con = lib)][order(con)]

pa <- merge(pa_con, pa_lib, by = "con")

tot_con <- dt[!is.na(con) & con %in% cats & !is.na(plurality_label),
              .(N = .N, Acc = round(mean(plurality_label == con), 3))]
tot_lib <- dt[!is.na(lib) & lib %in% cats & !is.na(plurality_label),
              .(N = .N, Acc = round(mean(plurality_label == lib), 3))]

# ── Panel B ──────────────────────────────────────────────────────────
m <- dt[!is.na(con) & con %in% cats & !is.na(plurality_label)]
m[, u_bin := fcase(u_k == 0,    "unanimous",
                   u_k == 0.25, "partial",
                   u_k == 0.5,  "split",
                   default = NA_character_)]

pb <- m[!is.na(u_bin),
        .(N     = .N,
          Acc_con = round(mean(plurality_label == con), 3),
          Acc_lib = round(mean(plurality_label == lib, na.rm = TRUE), 3)),
        by = u_bin]
pb[, ord := fcase(u_bin == "unanimous", 1L,
                  u_bin == "partial",   2L,
                  u_bin == "split",     3L)]
setorder(pb, ord)

mean_u   <- round(mean(dt$u_k,      na.rm = TRUE), 3)
mean_ent <- round(mean(dt$entropy,   na.rm = TRUE), 3)

# ── Panel C ──────────────────────────────────────────────────────────
m[, correct_con := plurality_label == con]
m[, correct_lib := fifelse(!is.na(lib) & lib %in% cats,
                           plurality_label == lib, NA)]

pc_con <- m[, .(N        = .N,
                mean_u   = round(mean(u_k),      3),
                mean_ent = round(mean(entropy),   3),
                pct_unani = round(mean(u_k == 0) * 100, 1)),
            by = correct_con][order(correct_con)]

pc_lib <- m[!is.na(correct_lib),
            .(N        = .N,
              mean_u   = round(mean(u_k),      3),
              mean_ent = round(mean(entropy),   3),
              pct_unani = round(mean(u_k == 0) * 100, 1)),
            by = correct_lib][order(correct_lib)]

# ── Label helpers ────────────────────────────────────────────────────
cat_label <- c(n = "$n$ (differentiated)",
               r = "$r$ (reference-priced)",
               w = "$w$ (organized-exchange)")

ubin_label <- c(unanimous = "Unanimous ($u_k = 0$, all 4 agree)",
                partial   = "Partial agreement ($u_k = 0.25$, 3 of 4)",
                split     = "Maximum disagreement ($u_k = 0.50$)")

correct_label <- c(`FALSE` = "\\quad Misclassified",
                   `TRUE`  = "\\quad Correct")

# ── Build LaTeX ──────────────────────────────────────────────────────
lines <- character(0)
L <- function(...) { lines <<- c(lines, paste0(...)) }

L("\\begin{table}[htbp]")
L("\\centering")
L("\\caption{Ensemble validation against Rauch (1999) --- SITC Rev.~2}")
L("\\label{tab:rauch_validation}")
L("\\begin{tabular}{lccccc}")
L("\\toprule")
L(" & \\multicolumn{2}{c}{Conservative} & \\multicolumn{2}{c}{Liberal} \\\\")
L("\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}")
L(" & $N$ & Acc. & $N$ & Acc. \\\\")
L("\\midrule")
L("\\multicolumn{5}{l}{\\textit{Panel A: Accuracy by category}} \\\\[2pt]")

for (i in seq_len(nrow(pa))) {
  r <- pa[i]
  L(cat_label[r$con], " & ", r$N_con, " & ", rnd(r$Acc_con),
    " & ", r$N_lib, " & ", rnd(r$Acc_lib), " \\\\")
}

L("\\addlinespace")
L("\\textit{Total matched} & \\multicolumn{2}{c}{", tot_con$N,
  " \\quad Acc.\\ ", rnd(tot_con$Acc), "} & \\multicolumn{2}{c}{",
  tot_lib$N, " \\quad Acc.\\ ", rnd(tot_lib$Acc), "} \\\\")

L("\\midrule")
L("\\multicolumn{5}{l}{\\textit{Panel B: Accuracy by ensemble uncertainty ",
  "($N_{\\mathrm{matched}} = ", tot_con$N, "$)}} \\\\[2pt]")

for (i in seq_len(nrow(pb))) {
  r <- pb[i]
  L(ubin_label[r$u_bin], " & ", r$N, " & ", rnd(r$Acc_con),
    " & ", r$N, " & ", rnd(r$Acc_lib), " \\\\")
}

L("\\addlinespace")
L("Mean $\\bar{u}_k$ (all ", nrow(dt), " products)",
  " & \\multicolumn{4}{l}{", mean_u,
  " \\quad Mean Shannon entropy $H_k$: ", mean_ent, " bits} \\\\")

L("\\midrule")
L("\\multicolumn{5}{l}{\\textit{Panel C: Uncertainty by classification outcome}} \\\\[2pt]")
L(" & $N$ & Mean $\\bar{u}_k$ & Mean $H_k$ (bits) & \\% unanimous \\\\")

L("\\addlinespace")
L("\\multicolumn{5}{l}{\\quad\\textit{Conservative}} \\\\[1pt]")
for (i in seq_len(nrow(pc_con))) {
  r <- pc_con[i]
  lbl <- correct_label[as.character(r$correct_con)]
  L(lbl, " & ", r$N, " & ", rnd(r$mean_u), " & ", rnd(r$mean_ent),
    " & ", formatC(r$pct_unani, format = "f", digits = 1), " \\\\")
}

L("\\addlinespace")
L("\\multicolumn{5}{l}{\\quad\\textit{Liberal}} \\\\[1pt]")
for (i in seq_len(nrow(pc_lib))) {
  r <- pc_lib[i]
  lbl <- correct_label[as.character(r$correct_lib)]
  L(lbl, " & ", r$N, " & ", rnd(r$mean_u), " & ", rnd(r$mean_ent),
    " & ", formatC(r$pct_unani, format = "f", digits = 1), " \\\\")
}

L("\\bottomrule")
L("\\end{tabular}")
L("\\end{table}")

tex <- paste(lines, collapse = "\n")

# ── Write output ─────────────────────────────────────────────────────
tab_dir <- file.path(base_dir, "output", "tables")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
stamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_tex <- file.path(tab_dir, sprintf("rauch_validation_%s.tex", stamp))
writeLines(tex, out_tex)

cat("LaTeX table written to:", out_tex, "\n")
cat("\n--- Preview ---\n")
cat(tex, "\n")
