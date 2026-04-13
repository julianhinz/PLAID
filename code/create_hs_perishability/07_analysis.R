###
# 07 - Perishability Gravity Analysis (PPML)
# 260402
###

# Structural gravity estimation (PPML) for the perishability classification.
# Tests whether perishable goods show a larger contiguity (shared-border)
# effect than non-perishable goods, consistent with time-sensitivity
# favouring nearby trading partners.
#
# Specifications:
#   A. By perishability class (1-5) without pair FEs — bilateral controls estimated
#   B. By class with pair FEs — time-invariant bilaterals absorbed
#   C. Pooled with perishability class × bilateral interactions
#
# All specs include origin×year + destination×year FEs.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, stringr, fixest, ggplot2)

setFixest_nthreads(max(1, parallel::detectCores() - 1))

dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)

# 0 - settings ----

category_labels = c(
  "1" = "Ultra-perishable",
  "2" = "Highly perishable",
  "3" = "Moderately perishable",
  "4" = "Low perishability",
  "5" = "Non-perishable"
)
valid_categories = names(category_labels)

gravity_main_gz = "input/gravity/Gravity_V202211.csv.gz"

hs_file_for_year = function(year) {
  if (year >= 1995 && year <= 1999) return("input/trade_data/HS92/hs92_country_country_product_year_6_1995_1999.csv.gz")
  if (year >= 2000 && year <= 2009) return("input/trade_data/HS92/hs92_country_country_product_year_6_2000_2009.csv.gz")
  if (year >= 2010 && year <= 2019) return("input/trade_data/HS92/hs92_country_country_product_year_6_2010_2019.csv.gz")
  if (year >= 2020 && year <= 2023) return("input/trade_data/HS92/hs92_country_country_product_year_6_2020_2023.csv.gz")
  stop("Year outside supported range: ", year)
}

hs_select_cols = c("country_iso3_code", "partner_iso3_code", "product_hs92_code", "year", "export_value")
hs_col_classes = list(character = c("country_iso3_code", "partner_iso3_code", "product_hs92_code"),
                      integer = "year", numeric = "export_value")

# 1 - load data ----

## 1.1 - CEPII gravity ----

if (!file.exists(gravity_main_gz)) {
  stop("CEPII gravity data not found: ", gravity_main_gz,
       "\nDownload from https://www.cepii.fr/DATA_DOWNLOAD/gravity/data/Gravity_csv_V202211.zip")
}

message("Loading CEPII gravity data...")
gravity = fread(
  gravity_main_gz,
  select = c("year", "iso3_o", "iso3_d",
             "distw_harmonic", "contig", "comlang_off", "col_dep_ever",
             "country_exists_o", "country_exists_d"),
  showProgress = FALSE
)
gravity = gravity[country_exists_o == 1 & country_exists_d == 1 &
                    !is.na(distw_harmonic) & distw_harmonic > 0]
gravity[, `:=`(country_exists_o = NULL, country_exists_d = NULL)]
gravity[, ln_dist := log(distw_harmonic)]
message(sprintf("  Gravity: %d bilateral-year pairs (%d-%d)",
                nrow(gravity), min(gravity$year), max(gravity$year)))

## 1.2 - consensus perishability classification (HS92) ----

db_file = "output/database/PLAID_v0.1_perishability_H0.csv.gz"
if (!file.exists(db_file)) {
  stop("Consensus database not found: ", db_file, ". Run code/build_database.R first.")
}

message("Loading consensus classification: ", db_file)
perish = fread(db_file, colClasses = list(character = "hs6_code"))
perish[, hs6_code := str_pad(hs6_code, 6, pad = "0")]
perish = perish[perishability_class %in% as.integer(valid_categories),
                .(hs6_code, perishability_class = as.character(perishability_class))]
message(sprintf("  Classification: %d codes (%s)",
                nrow(perish),
                paste(perish[, .N, by = perishability_class][order(perishability_class),
                      paste(perishability_class, N, sep = "=")], collapse = ", ")))

## 1.3 - trade data ----

# cross-section for main table, panel for pair FE spec and over-time analysis
main_year = 2019L
panel_years = c(2005L, 2010L, 2015L, 2019L)  # panel for pair FE spec
time_years = seq(1995L, 2023L)

# load main year
message(sprintf("Loading trade data for %d...", main_year))
trade_main = fread(
  hs_file_for_year(main_year),
  select = hs_select_cols, colClasses = hs_col_classes, showProgress = FALSE
)[year == main_year]
trade_main[, hs6_code := str_pad(product_hs92_code, 6, pad = "0")]
trade_main[, product_hs92_code := NULL]

# 2 - prepare estimation data (cross-section) ----

trade_class = merge(trade_main, perish, by = "hs6_code", all.x = FALSE)

bilat = trade_class[, .(trade_value = sum(export_value, na.rm = TRUE)),
                    by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code,
                           year, perishability_class)]

est_data = merge(bilat, gravity, by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)

message(sprintf("  Cross-section sample: %d obs (%s)",
                nrow(est_data),
                paste(est_data[, .N, by = perishability_class][order(perishability_class),
                      paste(perishability_class, N, sep = "=")], collapse = ", ")))

rm(trade_main, trade_class, bilat); gc()

# 3 - PPML estimation ----

## 3.1 - by category, bilateral controls estimated ----

message("Spec 1: by-category PPML...")
models_bycat = list()
for (cat in valid_categories) {
  models_bycat[[category_labels[cat]]] = fepois(
    trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
    data = est_data[perishability_class == cat],
    cluster = ~ iso3_o + iso3_d
  )
}

## 3.2 - pooled with category interactions ----

message("Spec 2: pooled PPML with perishability interactions...")
est_data[, cat_f := factor(perishability_class, levels = valid_categories)]

model_pooled = fepois(
  trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
  data = est_data,
  cluster = ~ iso3_o + iso3_d
)

model_interact = fepois(
  trade_value ~ cat_f:(ln_dist + contig + comlang_off + col_dep_ever) | iso3_o + iso3_d,
  data = est_data,
  cluster = ~ iso3_o + iso3_d
)

## 3.3 - panel with pair FEs ----

message("Spec 3: panel PPML with pair FEs...")
panel_list = list()
for (yy in panel_years) {
  tf = hs_file_for_year(yy)
  ty = fread(tf, select = hs_select_cols, colClasses = hs_col_classes, showProgress = FALSE)[year == yy]
  ty[, hs6_code := str_pad(product_hs92_code, 6, pad = "0")]
  ty[, product_hs92_code := NULL]
  ty = merge(ty, perish, by = "hs6_code", all.x = FALSE)
  panel_list[[as.character(yy)]] = ty[, .(trade_value = sum(export_value, na.rm = TRUE)),
                                      by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code,
                                             year, perishability_class)]
}
panel_bilat = rbindlist(panel_list)
panel_data = merge(panel_bilat, gravity, by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)
panel_data[, pair := paste(iso3_o, iso3_d, sep = "_")]
panel_data[, cat_f := factor(perishability_class, levels = valid_categories)]

message(sprintf("  Panel sample: %d obs (%d years)",
                nrow(panel_data), uniqueN(panel_data$year)))

models_pair = list()
for (cat in valid_categories) {
  models_pair[[category_labels[cat]]] = fepois(
    trade_value ~ 1 | iso3_o^year + iso3_d^year + pair,
    data = panel_data[perishability_class == cat],
    cluster = ~ pair
  )
}

model_pair_interact = fepois(
  trade_value ~ cat_f:contig + cat_f:comlang_off + cat_f:col_dep_ever | iso3_o^year + iso3_d^year + pair,
  data = panel_data,
  cluster = ~ pair
)

rm(panel_list, panel_bilat); gc()

# 4 - save tables ----

message("Saving tables...")

# Table 1: by-category, cross-section
etable(
  models_bycat,
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity by perishability class (PPML, %d)", main_year),
  notes = "FEs: origin, destination. Dependent variable: bilateral trade value.",
  file = "output/analysis/tables/perishability_ppml_by_category.tex"
)

# Table 2: pooled and interactions
etable(
  list("Pooled" = model_pooled, "Class interactions" = model_interact),
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity with perishability interactions (PPML, %d)", main_year),
  notes = "FEs: origin, destination. Dependent variable: bilateral trade value. Reference category: 1 (ultra-perishable).",
  file = "output/analysis/tables/perishability_ppml_interactions.tex"
)

# Table 3: panel with pair FEs
etable(
  c(models_pair, list("Interactions" = model_pair_interact)),
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity with pair FEs (PPML, %s)", paste(panel_years, collapse = "/")),
  notes = "FEs: origin$\\times$year, destination$\\times$year, pair. Time-invariant bilaterals (distance) absorbed by pair FE.",
  file = "output/analysis/tables/perishability_ppml_pair_fe.tex"
)

# console summary
message("\n=== By-category (cross-section) ===")
print(etable(models_bycat, digits = 3))

message("\n=== Pooled + interactions ===")
print(etable(model_pooled, model_interact, digits = 3))

message("\n=== Pair FE panel ===")
print(etable(c(models_pair, list("Interactions" = model_pair_interact)), digits = 3))

# (The year-by-year over-time block was removed 2026-04-10 per the paper
# revision: the paper now reports only the 2019 snapshot and the panel
# specification. See git history for the dropped block.)

message("Done.")
