# code/recreate_rauch_1995/10_aggregate_pilot.R
# -------------------------------------------------------------------
# Aggregate the 2-model SITC Rev.2 pilot classifications into a
# consensus CSV for paper validation. Used as primary Rauch validation
# (direct on native SITC Rev.2 nomenclature).
#
# Models: openai/gpt-5, anthropic/claude-3.5-sonnet (Nov 2025 pilot).
# Output: output/indicators/rauch_sitc2_aggregated_pilot_20260410.csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr)

if (requireNamespace("here", quietly = TRUE)) setwd(here::here())

# ── Input files (per Codex audit) ───────────────────────────────────
gpt_path <- "output/indicators/rauch_sitc2_20251127_openai-gpt-5_y1995_20251104_100056.csv"
cla_path <- "output/indicators/rauch_sitc2_20251127_anthropic-claude-3-5-sonnet_20251106_094312.csv"

stopifnot(file.exists(gpt_path))
stopifnot(file.exists(cla_path))

gpt <- fread(gpt_path)
cla <- fread(cla_path)

# Normalize column names — the two files may have slightly different names
find_code_col <- function(dt) {
  cands <- intersect(c("code", "sitc2", "sitc2_code"), names(dt))
  if (length(cands) == 0) stop("No code column found in: ", paste(names(dt), collapse = ", "))
  cands[[1]]
}
find_label_col <- function(dt) {
  cands <- intersect(c("rauch_category", "rauch", "category"), names(dt))
  if (length(cands) == 0) stop("No label column found in: ", paste(names(dt), collapse = ", "))
  cands[[1]]
}

gpt_code  <- find_code_col(gpt);  gpt_label  <- find_label_col(gpt)
cla_code  <- find_code_col(cla);  cla_label  <- find_label_col(cla)

gpt_m <- gpt[, .(code = as.character(get(gpt_code)),
                 rauch_gpt5 = as.character(get(gpt_label)))]
cla_m <- cla[, .(code = as.character(get(cla_code)),
                 rauch_claude35 = as.character(get(cla_label)))]

joined <- merge(gpt_m, cla_m, by = "code", all = TRUE)

# Majority vote across two models: agreement OR tie-breaker preferring the
# more standardized category (w > r > n), matching Rauch's original decision order.
consensus <- function(a, b) {
  if (is.na(a) && is.na(b)) return(NA_character_)
  if (is.na(a)) return(b)
  if (is.na(b)) return(a)
  if (a == b) return(a)
  order_map <- c(w = 3L, r = 2L, n = 1L)
  if (!a %in% names(order_map) || !b %in% names(order_map)) return(NA_character_)
  if (order_map[[a]] >= order_map[[b]]) a else b
}

joined[, rauch_consensus := mapply(consensus, rauch_gpt5, rauch_claude35)]
joined[, agreement := !is.na(rauch_gpt5) & !is.na(rauch_claude35) & rauch_gpt5 == rauch_claude35]

out_path <- "output/indicators/rauch_sitc2_aggregated_pilot_20260410.csv"
fwrite(joined, out_path)

cat("Wrote:", out_path, "\n")
cat("Rows:", nrow(joined), "\n")
cat("Agreement rate:", round(mean(joined$agreement, na.rm = TRUE), 3), "\n")
cat("Consensus class distribution:\n")
print(table(joined$rauch_consensus, useNA = "ifany"))
