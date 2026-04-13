###
# 09 - Gravity by Rauch Category: Replication & Extension
# 260413
###

# Replicates Rauch (1999) Table 6 structure (OLS, split by category)
# and extends with modern PPML. Compares original Rauch SITC
# classifications with LLM-generated SITC and PLAID HS92 classifications.
#
# Tables produced:
#   1. Rauch replication (OLS): SITC 1995, con + lib (6 cols)
#   2. Rauch replication (OLS): LLM SITC 1995 + PLAID HS 1995 (6 cols)
#   3. Rauch replication (OLS): PLAID HS 1995 + Rauch-via-concordance HS 1995 (6 cols)
#   4. Rauch replication (OLS): PLAID HS 2019 + Rauch-via-concordance HS 2019 (6 cols)
#   5. PPML with FEs: SITC 1995, Rauch con + lib (6 cols)
#   6. PPML with FEs: PLAID HS 1995 + 2019 (6 cols)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, stringr, fixest)

setFixest_nthreads(max(1, parallel::detectCores() - 1))

dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)

cat_labels = c(w = "Org.", r = "Ref.", n = "Dif.")
valid_cats = c("w", "r", "n")

# ═══════════════════════════════════════════════════════════════════════
# 1. LOAD GRAVITY
# ═══════════════════════════════════════════════════════════════════════

gravity_gz = "input/gravity/Gravity_V202211.csv.gz"
stopifnot(file.exists(gravity_gz))

message("Loading CEPII gravity data...")
grav = fread(
  gravity_gz,
  select = c("year", "iso3_o", "iso3_d",
             "distw_harmonic", "contig", "comlang_off", "col_dep_ever",
             "gdp_o", "gdp_d", "gdpcap_o", "gdpcap_d",
             "eu_o", "eu_d",
             "country_exists_o", "country_exists_d"),
  showProgress = FALSE
)
grav = grav[country_exists_o == 1 & country_exists_d == 1 &
              !is.na(distw_harmonic) & distw_harmonic > 0]
grav[, `:=`(country_exists_o = NULL, country_exists_d = NULL)]
grav[, ln_dist := log(distw_harmonic)]
grav[, ln_gdp_prod := fifelse(!is.na(gdp_o) & !is.na(gdp_d) & gdp_o > 0 & gdp_d > 0,
                               log(gdp_o * gdp_d), NA_real_)]
grav[, ln_gdpcap_prod := fifelse(!is.na(gdpcap_o) & !is.na(gdpcap_d) & gdpcap_o > 0 & gdpcap_d > 0,
                                  log(gdpcap_o * gdpcap_d), NA_real_)]
grav[, both_eu := as.integer(!is.na(eu_o) & !is.na(eu_d) & eu_o == 1 & eu_d == 1)]

# EFTA: year-varying membership
efta_members = function(yr) {
  # Pre-1995: Austria, Finland, Iceland, Liechtenstein, Norway, Sweden, Switzerland
  # 1995+: Austria/Finland/Sweden joined EU; Iceland, Liechtenstein, Norway, Switzerland remain
  core = c("ISL", "LIE", "NOR", "CHE")
  if (yr < 1995) return(c(core, "AUT", "FIN", "SWE"))
  return(core)
}

message("  Computing EFTA dummies...")
grav[, both_efta := 0L]
for (yr in unique(grav$year)) {
  ef = efta_members(yr)
  grav[year == yr, both_efta := as.integer(iso3_o %in% ef & iso3_d %in% ef)]
}

# ═══════════════════════════════════════════════════════════════════════
# 2. LOAD CLASSIFICATIONS
# ═══════════════════════════════════════════════════════════════════════

## 2a. Original Rauch (1999) — conservative + liberal
rauch_file = "temp/rauch/Rauch_classification_revised.csv"
if (!file.exists(rauch_file)) {
  dir.create("temp/rauch", recursive = TRUE, showWarnings = FALSE)
  download.file("https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv",
                rauch_file)
}
rauch_orig = fread(rauch_file, colClasses = list(character = "sitc4"))
rauch_orig[, sitc4 := str_pad(sitc4, 4, pad = "0")]

rauch_con = rauch_orig[con %in% valid_cats, .(product_sitc_code = sitc4, category = con)]
rauch_lib = rauch_orig[lib %in% valid_cats, .(product_sitc_code = sitc4, category = lib)]
message(sprintf("  Rauch con: %d codes | lib: %d codes", nrow(rauch_con), nrow(rauch_lib)))

## 2b. LLM SITC classifications (majority vote across models)
sitc_files = Sys.glob("output/indicators/rauch_sitc2_*.csv")
sitc_files = sitc_files[!grepl("aggregated", sitc_files)]
if (length(sitc_files) == 0) stop("No LLM SITC classifications found")

sitc_llm_all = rbindlist(lapply(sitc_files, function(f) {
  dt = fread(f, colClasses = list(character = "code"))
  dt[, .(code = str_pad(code, 4, pad = "0"), rauch_category)]
}))
rauch_llm_sitc = sitc_llm_all[rauch_category %in% valid_cats,
                               .N, by = .(product_sitc_code = code, category = rauch_category)]
rauch_llm_sitc = rauch_llm_sitc[rauch_llm_sitc[, .I[which.max(N)], by = product_sitc_code]$V1]
rauch_llm_sitc[, N := NULL]
message(sprintf("  LLM SITC: %d codes (%d models)", nrow(rauch_llm_sitc), length(sitc_files)))

## 2c. PLAID HS92 consensus
hs_db = "output/database/PLAID_v0.1_rauch_H0.csv.gz"
stopifnot(file.exists(hs_db))
rauch_hs = fread(hs_db, colClasses = list(character = "hs6_code"))
rauch_hs[, hs6_code := str_pad(hs6_code, 6, pad = "0")]
rauch_hs = rauch_hs[rauch %in% valid_cats, .(hs6_code, category = rauch)]
message(sprintf("  PLAID HS92: %d codes", nrow(rauch_hs)))

## 2d. Original Rauch mapped to HS via concordance (HS92 → SITC2)
message("  Building Rauch-via-concordance HS classification...")
conc_zip = "input/concordance/Concordance_H0_to_S2.zip"
stopifnot(file.exists(conc_zip))
conc_csv = unzip(conc_zip, list = TRUE)$Name[1]
conc = fread(cmd = sprintf("unzip -p '%s' '%s'", conc_zip, conc_csv), showProgress = FALSE)

# Detect column names (flexible)
hs_col = grep("HS.*Code", names(conc), value = TRUE)[1]
sitc_col = grep("SITC.*Code", names(conc), value = TRUE)[1]
conc = conc[, .(hs6 = str_pad(as.character(get(hs_col)), 6, pad = "0"),
                sitc4 = str_sub(str_pad(as.character(get(sitc_col)), 5, pad = "0"), 1, 4))]
conc = unique(conc)

# Map: HS6 → SITC4 → Rauch category (conservative), then majority vote at HS6 level
rauch_via_conc_con = merge(conc, rauch_con, by.x = "sitc4", by.y = "product_sitc_code",
                            allow.cartesian = TRUE)
rauch_via_conc_con = rauch_via_conc_con[, .N, by = .(hs6_code = hs6, category)]
rauch_via_conc_con = rauch_via_conc_con[rauch_via_conc_con[, .I[which.max(N)], by = hs6_code]$V1]
rauch_via_conc_con[, N := NULL]

rauch_via_conc_lib = merge(conc, rauch_lib, by.x = "sitc4", by.y = "product_sitc_code",
                            allow.cartesian = TRUE)
rauch_via_conc_lib = rauch_via_conc_lib[, .N, by = .(hs6_code = hs6, category)]
rauch_via_conc_lib = rauch_via_conc_lib[rauch_via_conc_lib[, .I[which.max(N)], by = hs6_code]$V1]
rauch_via_conc_lib[, N := NULL]

message(sprintf("  Rauch-via-concordance: con %d codes | lib %d codes",
                nrow(rauch_via_conc_con), nrow(rauch_via_conc_lib)))

# ═══════════════════════════════════════════════════════════════════════
# 3. LOAD TRADE DATA
# ═══════════════════════════════════════════════════════════════════════

sitc_sel = c("country_iso3_code", "partner_iso3_code", "product_sitc_code", "year", "export_value")
sitc_cls = list(character = c("country_iso3_code", "partner_iso3_code", "product_sitc_code"),
                integer = "year", numeric = "export_value")
hs_sel = c("country_iso3_code", "partner_iso3_code", "product_hs92_code", "year", "export_value")
hs_cls = list(character = c("country_iso3_code", "partner_iso3_code", "product_hs92_code"),
              integer = "year", numeric = "export_value")

message("Loading trade data...")
trade_sitc_1995 = fread("input/trade_data/SITC/sitc_country_country_product_year_4_1990_1999.csv.gz",
                        select = sitc_sel, colClasses = sitc_cls, showProgress = FALSE)[year == 1995L]
trade_sitc_1995[, product_sitc_code := str_pad(product_sitc_code, 4, pad = "0")]

trade_hs_1995 = fread("input/trade_data/HS92/hs92_country_country_product_year_6_1995_1999.csv.gz",
                      select = hs_sel, colClasses = hs_cls, showProgress = FALSE)[year == 1995L]
trade_hs_1995[, hs6_code := str_pad(product_hs92_code, 6, pad = "0")][, product_hs92_code := NULL]

trade_hs_2019 = fread("input/trade_data/HS92/hs92_country_country_product_year_6_2010_2019.csv.gz",
                      select = hs_sel, colClasses = hs_cls, showProgress = FALSE)[year == 2019L]
trade_hs_2019[, hs6_code := str_pad(product_hs92_code, 6, pad = "0")][, product_hs92_code := NULL]

# ═══════════════════════════════════════════════════════════════════════
# 4. HELPERS
# ═══════════════════════════════════════════════════════════════════════

aggregate_bilat = function(trade, classification, merge_col) {
  tc = merge(trade, classification, by = merge_col, all.x = FALSE)
  tc[, .(trade_value = sum(export_value, na.rm = TRUE)),
     by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code,
            year, category)]
}

build_ols_data = function(bilat, year_val) {
  est = merge(bilat, grav[year == year_val], by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)
  est = est[trade_value > 0 & !is.na(ln_gdp_prod) & !is.na(ln_gdpcap_prod)]
  est[, ln_trade := log(trade_value)]
  est
}

build_ppml_data = function(bilat, year_val) {
  est = merge(bilat, grav[year == year_val, .(iso3_o, iso3_d, year, ln_dist, contig, comlang_off, col_dep_ever)],
              by = c("iso3_o", "iso3_d", "year"), all.x = FALSE)
  est
}

# OLS formula (Rauch Table 6)
fml_ols = ln_trade ~ ln_gdp_prod + ln_gdpcap_prod + ln_dist + contig + comlang_off + col_dep_ever + both_eu + both_efta

run_ols_split = function(est, label) {
  message(sprintf("  %s: %d obs (w=%d, r=%d, n=%d)", label, nrow(est),
                  nrow(est[category == "w"]), nrow(est[category == "r"]), nrow(est[category == "n"])))
  models = list()
  for (cat in valid_cats) {
    models[[cat_labels[cat]]] = feols(fml_ols, data = est[category == cat], vcov = "hetero")
  }
  models
}

run_ppml_split = function(est, label) {
  message(sprintf("  %s: %d obs (w=%d, r=%d, n=%d)", label, nrow(est),
                  nrow(est[category == "w"]), nrow(est[category == "r"]), nrow(est[category == "n"])))
  models = list()
  for (cat in valid_cats) {
    models[[cat_labels[cat]]] = fepois(
      trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | iso3_o + iso3_d,
      data = est[category == cat],
      cluster = ~ iso3_o + iso3_d
    )
  }
  models
}

# ═══════════════════════════════════════════════════════════════════════
# 5. BUILD ESTIMATION DATASETS
# ═══════════════════════════════════════════════════════════════════════

message("\nAggregating bilateral trade...")

# SITC 1995
bilat_sitc_con_1995 = aggregate_bilat(trade_sitc_1995, rauch_con, "product_sitc_code")
bilat_sitc_lib_1995 = aggregate_bilat(trade_sitc_1995, rauch_lib, "product_sitc_code")
bilat_sitc_llm_1995 = aggregate_bilat(trade_sitc_1995, rauch_llm_sitc, "product_sitc_code")

# HS 1995
bilat_hs_plaid_1995  = aggregate_bilat(trade_hs_1995, rauch_hs, "hs6_code")
bilat_hs_conccon_1995 = aggregate_bilat(trade_hs_1995, rauch_via_conc_con, "hs6_code")
bilat_hs_conclib_1995 = aggregate_bilat(trade_hs_1995, rauch_via_conc_lib, "hs6_code")

# HS 2019
bilat_hs_plaid_2019  = aggregate_bilat(trade_hs_2019, rauch_hs, "hs6_code")
bilat_hs_conccon_2019 = aggregate_bilat(trade_hs_2019, rauch_via_conc_con, "hs6_code")
bilat_hs_conclib_2019 = aggregate_bilat(trade_hs_2019, rauch_via_conc_lib, "hs6_code")

rm(trade_sitc_1995, trade_hs_1995, trade_hs_2019); gc()

# ═══════════════════════════════════════════════════════════════════════
# 6. OLS REGRESSIONS (Rauch Table 6 replication)
# ═══════════════════════════════════════════════════════════════════════

message("\n── OLS Regressions (Rauch Table 6 style) ──")

# Table 1: SITC 1995, Rauch con + lib
message("\nTable 1: SITC 1995, Rauch (1999) con + lib")
ols_1 = c(
  run_ols_split(build_ols_data(bilat_sitc_con_1995, 1995L), "Rauch con"),
  run_ols_split(build_ols_data(bilat_sitc_lib_1995, 1995L), "Rauch lib")
)

# Table 2: LLM SITC 1995 + PLAID HS 1995
message("\nTable 2: LLM SITC 1995 + PLAID HS 1995")
ols_2 = c(
  run_ols_split(build_ols_data(bilat_sitc_llm_1995, 1995L), "LLM SITC"),
  run_ols_split(build_ols_data(bilat_hs_plaid_1995, 1995L), "PLAID HS")
)

# Table 3: PLAID HS 1995 + Rauch-via-concordance con HS 1995
message("\nTable 3: PLAID HS 1995 + Rauch-via-concordance 1995")
ols_3 = c(
  run_ols_split(build_ols_data(bilat_hs_plaid_1995, 1995L), "PLAID HS 1995"),
  run_ols_split(build_ols_data(bilat_hs_conccon_1995, 1995L), "Rauch→HS 1995")
)

# Table 4: PLAID HS 2019 + Rauch-via-concordance con HS 2019
message("\nTable 4: PLAID HS 2019 + Rauch-via-concordance 2019")
ols_4 = c(
  run_ols_split(build_ols_data(bilat_hs_plaid_2019, 2019L), "PLAID HS 2019"),
  run_ols_split(build_ols_data(bilat_hs_conccon_2019, 2019L), "Rauch→HS 2019")
)

# ═══════════════════════════════════════════════════════════════════════
# 7. PPML REGRESSIONS (modern structural gravity)
# ═══════════════════════════════════════════════════════════════════════

message("\n── PPML Regressions (structural gravity) ──")

# Table 5: SITC 1995, Rauch con + lib
message("\nTable 5: PPML SITC 1995, Rauch con + lib")
ppml_5 = c(
  run_ppml_split(build_ppml_data(bilat_sitc_con_1995, 1995L), "Rauch con"),
  run_ppml_split(build_ppml_data(bilat_sitc_lib_1995, 1995L), "Rauch lib")
)

# Table 6: PPML PLAID HS 1995 + 2019
message("\nTable 6: PPML PLAID HS 1995 + 2019")
ppml_6 = c(
  run_ppml_split(build_ppml_data(bilat_hs_plaid_1995, 1995L), "PLAID HS 1995"),
  run_ppml_split(build_ppml_data(bilat_hs_plaid_2019, 2019L), "PLAID HS 2019")
)

gc()

# ═══════════════════════════════════════════════════════════════════════
# 8. OUTPUT
# ═══════════════════════════════════════════════════════════════════════

message("\n── Saving tables ──")

save_table = function(models, hdr_left, hdr_right, title, notes, filename) {
  cat(sprintf("\n=== %s ===\n", title))
  tab = etable(models, digits = 3,
               headers = list(" " = c(rep(hdr_left, 3), rep(hdr_right, 3))))
  print(tab)
  etable(models, tex = TRUE, float = FALSE,
         signif.code = "letters", digits = 3,
         title = title, notes = notes,
         headers = list(" " = c(rep(hdr_left, 3), rep(hdr_right, 3))),
         file = sprintf("output/analysis/tables/%s.tex", filename))
}

ols_notes = "OLS. Dependent variable: log bilateral trade value. Zero trade observations dropped. Heteroskedasticity-robust standard errors."
ppml_notes = "PPML. Dependent variable: bilateral trade value. Exporter and importer fixed effects. Two-way clustered standard errors (exporter, importer)."

# OLS tables
save_table(ols_1, "Conservative", "Liberal",
           "Bilateral trade by Rauch category (OLS, SITC 1995)",
           ols_notes, "rauch_ols_sitc_1995_conlib")

save_table(ols_2, "LLM (SITC)", "PLAID (HS92)",
           "Bilateral trade by Rauch category: LLM vs PLAID (OLS, 1995)",
           ols_notes, "rauch_ols_llm_vs_plaid_1995")

save_table(ols_3, "PLAID", "Rauch via concordance",
           "Bilateral trade by Rauch category: PLAID vs concordance (OLS, HS 1995)",
           ols_notes, "rauch_ols_plaid_vs_conc_1995")

save_table(ols_4, "PLAID", "Rauch via concordance",
           "Bilateral trade by Rauch category: PLAID vs concordance (OLS, HS 2019)",
           ols_notes, "rauch_ols_plaid_vs_conc_2019")

# PPML tables
save_table(ppml_5, "Conservative", "Liberal",
           "Structural gravity by Rauch category (PPML, SITC 1995)",
           ppml_notes, "rauch_ppml_sitc_1995_conlib")

save_table(ppml_6, "1995", "2019",
           "Structural gravity by Rauch category (PPML, PLAID HS92)",
           ppml_notes, "rauch_ppml_plaid_hs_1995_2019")

message("\nAll tables saved to output/analysis/tables/rauch_*.tex")
message("Done.")
