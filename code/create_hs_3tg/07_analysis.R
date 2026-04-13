###
# 07 - 3TG Conflict Minerals Gravity Analysis (PPML)
# 260402
###

# Structural gravity estimation (PPML) for the 3TG conflict minerals indicator.
# Main test: diff-in-diff around the EU Conflict Minerals Regulation (Jan 2021).
# EU importers of 3TG-classified goods should see a decline relative to
# non-3TG goods after 2021, compared to non-EU importers.
#
# Specifications:
#   A. Triple-difference: conflict_mineral x eu_importer x post_2021 (panel)
#   B. Separate diff-in-diff for 3TG vs. non-3TG goods (panel)
#   C. Event study: year-specific treatment effects (panel)
#   D. Cross-section by category (2019) for comparability with other indicators

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, stringr, fixest, ggplot2)

setFixest_nthreads(max(1, parallel::detectCores() - 1))

dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)

# 0 - settings ----

category_labels = c("TRUE" = "3TG mineral goods", "FALSE" = "Non-3TG goods")

EU27 = c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
         "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
         "POL","PRT","ROU","SVK","SVN","ESP","SWE")

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

## 1.2 - consensus 3TG classification (HS92) ----

db_file = "output/database/PLAID_v0.1_3tg_H0.csv.gz"
if (!file.exists(db_file)) {
  stop("Consensus database not found: ", db_file, ". Run code/build_database.R first.")
}

message("Loading consensus classification: ", db_file)
tg = fread(db_file, colClasses = list(character = "hs6_code"))
tg[, hs6_code := str_pad(hs6_code, 6, pad = "0")]
tg = tg[!is.na(conflict_mineral), .(hs6_code, conflict_mineral)]
message(sprintf("  Classification: %d codes (3TG=%d, non-3TG=%d)",
                nrow(tg), sum(tg$conflict_mineral), sum(!tg$conflict_mineral)))

## 1.3 - trade data (diff-in-diff panel: 2017-2023) ----

panel_years = 2017L:2023L

message(sprintf("Loading trade data for diff-in-diff panel (%d-%d)...",
                min(panel_years), max(panel_years)))

panel_list = list()
for (yy in panel_years) {
  tf = hs_file_for_year(yy)
  if (!file.exists(tf)) next
  ty = fread(tf, select = hs_select_cols, colClasses = hs_col_classes, showProgress = FALSE)[year == yy]
  if (!nrow(ty)) next
  ty[, hs6_code := str_pad(product_hs92_code, 6, pad = "0")]
  ty[, product_hs92_code := NULL]
  ty = merge(ty, tg, by = "hs6_code", all.x = FALSE)
  panel_list[[as.character(yy)]] = ty[, .(trade_value = sum(export_value, na.rm = TRUE)),
                                      by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code,
                                             year, conflict_mineral)]
  cat(yy, "")
}
cat("\n")

panel_bilat = rbindlist(panel_list)
panel_data = merge(panel_bilat, gravity, by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)

# 2 - prepare diff-in-diff variables ----

panel_data[, pair := paste(iso3_o, iso3_d, sep = "_")]
panel_data[, eu_importer := iso3_d %in% EU27]
panel_data[, post_2021 := year >= 2021L]
panel_data[, treated := conflict_mineral & eu_importer]
panel_data[, cm_f := factor(conflict_mineral, levels = c(FALSE, TRUE),
                            labels = c("Non-3TG goods", "3TG mineral goods"))]

message(sprintf("  Panel sample: %d obs (%d years, EU importer obs: %d, 3TG obs: %d)",
                nrow(panel_data), uniqueN(panel_data$year),
                sum(panel_data$eu_importer), sum(panel_data$conflict_mineral)))

rm(panel_list, panel_bilat); gc()

# 3 - PPML diff-in-diff estimation ----

## 3.1 - triple-difference ----

# With origin×year + destination×year + pair FEs, the two-way interactions
# (eu_importer:post_2021, etc.) are absorbed. The triple interaction
# conflict_mineral × eu_importer × post_2021 is identified because
# conflict_mineral varies within pair-year (3TG vs non-3TG stacked).
# We also include conflict_mineral:post_2021 (varies within pair) to
# control for global 3TG trends.

message("Spec 1: triple-difference PPML...")

panel_data[, cm := as.integer(conflict_mineral)]

model_did = fepois(
  trade_value ~ cm:eu_importer:post_2021 + cm:post_2021 |
    iso3_o^year + iso3_d^year + pair,
  data = panel_data,
  cluster = ~ pair
)

# Spec without pair FEs — can estimate more interactions
model_did_nopair = fepois(
  trade_value ~ cm:eu_importer:post_2021 + cm:post_2021 + cm:eu_importer +
    ln_dist + contig + comlang_off + col_dep_ever |
    iso3_o^year + iso3_d^year,
  data = panel_data,
  cluster = ~ iso3_o + iso3_d
)

# 4 - cross-section by category (2019) ----

message("Spec 4: cross-section by category (2019)...")

main_year = 2019L
est_cross = panel_data[year == main_year]

models_bycat = list()
for (cm in c(TRUE, FALSE)) {
  lbl = category_labels[as.character(cm)]
  models_bycat[[lbl]] = fepois(
    trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
    data = est_cross[conflict_mineral == cm],
    cluster = ~ iso3_o + iso3_d
  )
}

model_pooled = fepois(
  trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
  data = est_cross,
  cluster = ~ iso3_o + iso3_d
)

model_interact = fepois(
  trade_value ~ cm_f:(ln_dist + contig + comlang_off + col_dep_ever) | iso3_o + iso3_d,
  data = est_cross,
  cluster = ~ iso3_o + iso3_d
)

# 6 - save tables ----

message("Saving tables...")

## 6.1 - diff-in-diff (main result) ----

etable(
  list("With pair FE" = model_did,
       "Without pair FE" = model_did_nopair),
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  order = c("ln_dist", "contig", "comlang_off", "col_dep_ever",
            "^cm:post_2021", "^cm:eu_importerTRUE$", "^cm:eu_importerTRUE:"),
  title = sprintf("EU Conflict Minerals Regulation diff-in-diff (PPML, %d-%d)",
                  min(panel_years), max(panel_years)),
  notes = "FEs: origin$\\times$year, destination$\\times$year (+ pair in col 1). Clustered by pair / origin+destination.",
  file = "output/analysis/tables/3tg_ppml_did.tex"
)

## 6.2 - cross-section by category ----

etable(
  models_bycat,
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity by 3TG classification (PPML, %d)", main_year),
  notes = "FEs: origin, destination. Dependent variable: bilateral trade value.",
  file = "output/analysis/tables/3tg_ppml_by_category.tex"
)

## 6.3 - pooled and interactions ----

etable(
  list("Pooled" = model_pooled, "Category interactions" = model_interact),
  tex = TRUE,
  float = FALSE,
  signif.code = "letters",
  digits = 3,
  title = sprintf("Structural gravity with 3TG interactions (PPML, %d)", main_year),
  notes = "FEs: origin, destination. Dependent variable: bilateral trade value. Reference: non-3TG goods.",
  file = "output/analysis/tables/3tg_ppml_interactions.tex"
)

# console summary
message("\n=== Diff-in-diff (main result) ===")
print(etable(model_did, model_did_nopair, digits = 3))

message("\n=== Cross-section by category ===")
print(etable(models_bycat, digits = 3))

message("\n=== Pooled + interactions ===")
print(etable(model_pooled, model_interact, digits = 3))

# (The year-by-year over-time block was removed 2026-04-10 per the paper
# revision: the paper now reports only the diff-in-diff and 2019 snapshot
# specifications. See git history for the dropped block.)

message("Done.")
