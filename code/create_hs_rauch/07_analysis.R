###
# 07 - Rauch Gravity Analysis (PPML)
# 260402
###

# Structural gravity estimation (PPML) for the Rauch classification.
# Tests whether distance and network proxies matter more for
# differentiated goods than for exchange-traded commodities.
#
# Specifications:
#   1. Cross-section (2019): pooled + split by category, exporter + importer FEs
#   2. Panel (2005/2010/2015/2019): split by category, origin×year + destination×year + pair FEs
#
# The year-by-year over-time block was removed 2026-04-10 per the paper
# revision (coauthor call): the paper now reports only the 2019 snapshot
# and the panel specification; over-time figures were dropped.

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, stringr, fixest, ggplot2)

setFixest_nthreads(max(1, parallel::detectCores() - 1))

dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)

# 0 - settings ----

category_labels = c(w = "Organized exchange", r = "Reference priced", n = "Differentiated")
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

## 1.2 - consensus Rauch classification (HS92) ----

db_file = "output/database/PLAID_v0.1_rauch_H0.csv.gz"
if (!file.exists(db_file)) {
  stop("Consensus database not found: ", db_file, ". Run code/build_database.R first.")
}

message("Loading consensus classification: ", db_file)
rauch = fread(db_file, colClasses = list(character = "hs6_code"))
rauch[, hs6_code := str_pad(hs6_code, 6, pad = "0")]
rauch = rauch[rauch %in% valid_categories, .(hs6_code, category = rauch)]
message(sprintf("  Classification: %d codes (%s)",
                nrow(rauch),
                paste(rauch[, .N, by = category][order(category), paste(category, N, sep = "=")], collapse = ", ")))

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

trade_class = merge(trade_main, rauch, by = "hs6_code", all.x = FALSE)

bilat = trade_class[, .(trade_value = sum(export_value, na.rm = TRUE)),
                    by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code,
                           year, category)]

est_data = merge(bilat, gravity, by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)

message(sprintf("  Cross-section sample: %d obs (%s)",
                nrow(est_data),
                paste(est_data[, .N, by = category][order(category), paste(category, N, sep = "=")], collapse = ", ")))

rm(trade_main, trade_class, bilat); gc()

# 3 - PPML estimation ----

## 3.1 - by category, bilateral controls estimated ----

message("Spec 1: by-category PPML...")
models_bycat = list()
for (cat in valid_categories) {
  models_bycat[[category_labels[cat]]] = fepois(
    trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
    data = est_data[category == cat],
    cluster = ~ iso3_o + iso3_d
  )
}

## 3.2 - pooled (all goods) ----

message("Spec 2: pooled PPML (all goods)...")
est_data[, cat_f := factor(category, levels = valid_categories)]

model_pooled = fepois(
  trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
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
  ty = merge(ty, rauch, by = "hs6_code", all.x = FALSE)
  panel_list[[as.character(yy)]] = ty[, .(trade_value = sum(export_value, na.rm = TRUE)),
                                      by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code,
                                             year, category)]
}
panel_bilat = rbindlist(panel_list)
panel_data = merge(panel_bilat, gravity, by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)
panel_data[, pair := paste(iso3_o, iso3_d, sep = "_")]
panel_data[, cat_f := factor(category, levels = valid_categories)]

message(sprintf("  Panel sample: %d obs (%d years)",
                nrow(panel_data), uniqueN(panel_data$year)))

models_pair = list()
for (cat in valid_categories) {
  models_pair[[category_labels[cat]]] = fepois(
    trade_value ~ 1 | iso3_o^year + iso3_d^year + pair,
    data = panel_data[category == cat],
    cluster = ~ pair
  )
}

rm(panel_list, panel_bilat); gc()

# 4 - save tables ----

message("Saving tables...")

# Table 1: pooled + split samples
etable(
  c(list("All goods" = model_pooled), models_bycat),
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity by Rauch category (PPML, %d)", main_year),
  notes = "FEs: exporter, importer. Dependent variable: bilateral trade value. Clustered SEs (exporter, importer).",
  file = "output/analysis/tables/rauch_ppml_by_category.tex"
)

# Table 2: panel with pair FEs
etable(
  models_pair,
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity with pair FEs (PPML, %s)", paste(panel_years, collapse = "/")),
  notes = "FEs: exporter$\\times$year, importer$\\times$year, pair. Time-invariant bilaterals absorbed by pair FE.",
  file = "output/analysis/tables/rauch_ppml_pair_fe.tex"
)

# console summary
message("\n=== Split samples (cross-section) ===")
print(etable(c(list("All goods" = model_pooled), models_bycat), digits = 3))

message("\n=== Pair FE panel ===")
print(etable(models_pair, digits = 3))

# (The year-by-year over-time block was removed 2026-04-10 per the paper
# revision: the paper now reports only the 2019 snapshot and panel
# specifications. See git history for the dropped block.)

message("Done.")
