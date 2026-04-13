#!/usr/bin/env Rscript

###
# 08_compare_sitc_hs.R — Compare SITC and HS Rauch classifications
#   - Compare trade shares by category (SITC vs HS) for overlapping years
#   - Map HS→SITC via concordance and compare LLM classifications
#   - Side-by-side gravity estimates
# 250104
###

if (!require("pacman")) install.packages("pacman"); library("pacman")
pacman::p_load(data.table)
pacman::p_load(stringr)
pacman::p_load(fixest)
pacman::p_load(ggplot2)
pacman::p_load(knitr)

setFixest_nthreads(max(1, parallel::detectCores() - 1))

dir.create("output/analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("input/gravity", recursive = TRUE, showWarnings = FALSE)
dir.create("temp/rauch", recursive = TRUE, showWarnings = FALSE)
dir.create("temp/concordance", recursive = TRUE, showWarnings = FALSE)

category_labels = c(w = "Organized exchange (w)",
                    r = "Reference priced (r)",
                    n = "Differentiated (n)")
valid_categories = names(category_labels)

pad4 = function(x) str_pad(as.character(x), 4, pad = "0")
pad6 = function(x) str_pad(as.character(x), 6, pad = "0")

safe_slug = function(x) {
  slug = tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_+|_+$", "", slug)
}

md_table = function(df, digits = 3, rownames_label = NULL) {
  df = as.data.frame(df)
  if (!nrow(df)) return("_(none)_\n")
  num = vapply(df, is.numeric, logical(1))
  if (any(num)) df[num] = lapply(df[num], function(z) ifelse(is.finite(z), round(z, digits), z))
  esc = function(x) { x = gsub("\\|", "\\\\|", x); gsub("\r?\n", "<br>", x) }
  for (j in seq_along(df)) if (is.character(df[[j]]) || is.factor(df[[j]])) df[[j]] = esc(as.character(df[[j]]))
  if (!is.null(rownames_label) && is.null(df[[rownames_label]])) {
    df = cbind(setNames(list(rownames(df)), rownames_label), df)
  }
  hdr = paste(names(df), collapse = " | "); sep = paste(rep("---", ncol(df)), collapse = " | ")
  rows = apply(df, 1, function(r) paste(ifelse(is.na(r), "", r), collapse = " | "))
  paste(c(hdr, sep, rows), collapse = "\n")
}


# 1 - Load classifications ----

message("Loading classifications …")

## 1.1 - Original Rauch SITC classification ----

rauch_path = "temp/rauch/Rauch_classification_revised.csv"
if (!file.exists(rauch_path)) {
  download.file("https://econweb.ucsd.edu/~jrauch/rauchclass/Rauch_classification_revised.csv",
                destfile = rauch_path, mode = "wb", quiet = TRUE)
}
rauch_class = fread(rauch_path, colClasses = "character")
setnames(rauch_class, "sitc4", "product_sitc_code")
rauch_class_long = melt(rauch_class,
                        id.vars = "product_sitc_code",
                        measure.vars = c("con", "lib"),
                        variable.name = "classification",
                        value.name = "category")
rauch_class_long[, classification := as.character(classification)]
rauch_class_long[, source := "original"]
rauch_class_long[, system := "SITC"]

## 1.2 - SITC LLM classifications ----

sitc_llm_files = list.files("output/indicators", pattern = "^rauch_sitc.*\\.csv$", full.names = TRUE)
sitc_llm_files = sitc_llm_files[!str_detect(sitc_llm_files, "sample")]

sitc_llm_class = NULL
if (length(sitc_llm_files) > 0) {
  for (lf in sitc_llm_files) {
    model_name = gsub("^rauch_sitc2?_?([0-9]{8})?_?([^_]+)_.*\\.csv$", "\\2", basename(lf))
    if (identical(model_name, basename(lf))) {
      model_name = tools::file_path_sans_ext(basename(lf))
    }
    message("Loading SITC LLM classification: ", basename(lf), " | source = ", model_name)
    llm_dt = fread(lf, colClasses = "character")
    if (!("code" %in% names(llm_dt)) || !("rauch_category" %in% names(llm_dt))) {
      warning("Could not parse 'code' or 'rauch_category' in ", basename(lf), "; skipping.")
      next
    }
    llm_tmp = llm_dt[, .(product_sitc_code = pad4(code),
                         classification = "llm",
                         category = tolower(rauch_category),
                         source = model_name,
                         system = "SITC")]
    sitc_llm_class = rbindlist(list(sitc_llm_class, llm_tmp), use.names = TRUE, fill = TRUE)
  }
}

## 1.3 - HS LLM classifications ----

hs_llm_files = list.files("output/indicators", pattern = "^rauch_hs6_.*\\.csv$", full.names = TRUE)
hs_llm_files = hs_llm_files[!str_detect(hs_llm_files, "sample")]

hs_llm_class = NULL
if (length(hs_llm_files) > 0) {
  for (lf in hs_llm_files) {
    model_name = gsub("^rauch_hs6_([0-9]{8})_([^_]+)_.*\\.csv$", "\\2", basename(lf))
    if (identical(model_name, basename(lf))) {
      model_name = tools::file_path_sans_ext(basename(lf))
    }
    message("Loading HS LLM classification: ", basename(lf), " | source = ", model_name)
    llm_dt = fread(lf, colClasses = "character")
    if (!("code" %in% names(llm_dt)) || !("rauch_category" %in% names(llm_dt))) {
      warning("Could not parse 'code' or 'rauch_category' in ", basename(lf), "; skipping.")
      next
    }
    llm_tmp = llm_dt[, .(product_hs_code = pad6(code),
                         classification = "llm",
                         category = tolower(rauch_category),
                         source = model_name,
                         system = "HS")]
    hs_llm_class = rbindlist(list(hs_llm_class, llm_tmp), use.names = TRUE, fill = TRUE)
  }
}

if (is.null(sitc_llm_class) && is.null(hs_llm_class)) {
  stop("No LLM classifications found for SITC or HS. Run classification pipelines first.")
}


# 2 - HS→SITC concordance mapping ----

message("Loading concordance …")

# Download concordance if needed
concord_path = "temp/concordance/Concordance_H0_to_S2.csv"
if (!file.exists(concord_path)) {
  # Try to find any existing concordance
  concord_files = list.files("temp/concordance", pattern = "Concordance_H._to_S2\\.csv", full.names = TRUE)
  if (length(concord_files) > 0) {
    concord_path = concord_files[1]
  } else {
    message("Downloading H0→S2 concordance from WITS …")
    dir.create("temp/concordance", recursive = TRUE, showWarnings = FALSE)
    download.file("https://wits.worldbank.org/data/public/concordance/Concordance_H0_to_S2.zip",
                  destfile = "temp/concordance/Concordance_H0_to_S2.zip", mode = "wb", quiet = TRUE)
    unzip("temp/concordance/Concordance_H0_to_S2.zip", exdir = "temp/concordance")
    unlink("temp/concordance/Concordance_H0_to_S2.zip")
  }
}

concordance = fread(concord_path, showProgress = FALSE)
# Find column names
find_col = function(df, patterns) {
  hits = NULL
  for (p in patterns) {
    hits = grep(p, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hits)) break
  }
  if (!length(hits)) return(NULL)
  hits[1]
}
hs_col = find_col(concordance, c("^hs\\b", "HS.*Product.*Code"))
sitc_col = find_col(concordance, c("^sitc\\b", "SITC.*Product.*Code"))

if (is.null(hs_col) || is.null(sitc_col)) {
  stop("Could not find HS and SITC columns in concordance file: ", concord_path)
}

concordance[, hs6 := pad6(get(hs_col))]
concordance[, sitc_raw := str_pad(as.character(get(sitc_col)), width = 5, pad = "0")]
concordance[, sitc4 := substr(sitc_raw, 1, 4)]
concordance = unique(concordance[!is.na(hs6) & !is.na(sitc4), .(hs6, sitc4)])


# 3 - Map HS classifications to SITC for comparison ----

message("Mapping HS classifications to SITC via concordance …")

if (!is.null(hs_llm_class)) {
  # Join HS classifications with concordance
  hs_mapped = merge(
    hs_llm_class,
    concordance,
    by.x = "product_hs_code",
    by.y = "hs6",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  hs_mapped = hs_mapped[!is.na(sitc4)]

  # Aggregate to SITC4 by majority vote (or weighted by confidence if available)
  hs_sitc_agg = hs_mapped[, .(
    w_count = sum(category == "w"),
    r_count = sum(category == "r"),
    n_count = sum(category == "n")
  ), by = .(sitc4, source)]

  hs_sitc_agg[, total := w_count + r_count + n_count]
  hs_sitc_agg[, category_from_hs := c("w", "r", "n")[max.col(.SD, ties.method = "first")],
              .SDcols = c("w_count", "r_count", "n_count")]
  hs_sitc_agg[, prob_w := w_count / total]
  hs_sitc_agg[, prob_r := r_count / total]
  hs_sitc_agg[, prob_n := n_count / total]

  hs_as_sitc = hs_sitc_agg[, .(product_sitc_code = sitc4,
                               classification = "hs_mapped",
                               category = category_from_hs,
                               source = source,
                               system = "HS→SITC")]
}


# 4 - Compare classifications on SITC products ----

message("Comparing classifications …")

# Combine all SITC-level classifications for comparison
all_sitc_class = rbindlist(list(
  rauch_class_long,
  sitc_llm_class,
  if (!is.null(hs_llm_class)) hs_as_sitc else NULL
), use.names = TRUE, fill = TRUE)

# Get common SITC codes for comparison
common_codes = Reduce(intersect, list(
  unique(rauch_class_long[classification == "con"]$product_sitc_code),
  if (!is.null(sitc_llm_class)) unique(sitc_llm_class$product_sitc_code) else character(0),
  if (!is.null(hs_llm_class)) unique(hs_as_sitc$product_sitc_code) else character(0)
))

message("Found ", length(common_codes), " common SITC codes across all classifications")


# 5 - Generate comparison report ----

message("Generating comparison report …")

report_path = sprintf("output/metrics/sitc_hs_comparison_%s.md", format(Sys.time(), "%Y%m%d-%H%M%S"))
sink(report_path)

cat("# SITC vs HS Rauch Classification Comparison\n\n")
cat(sprintf("**Generated:** %s\n\n", Sys.time()))

## 5.1 - Classification coverage ----

cat("## Classification Coverage\n\n")

coverage_tbl = data.table(
  System = c("SITC (Original Rauch con)", "SITC (Original Rauch lib)"),
  Products = c(nrow(rauch_class_long[classification == "con"]),
               nrow(rauch_class_long[classification == "lib"]))
)

if (!is.null(sitc_llm_class)) {
  sitc_sources = unique(sitc_llm_class$source)
  for (src in sitc_sources) {
    coverage_tbl = rbindlist(list(coverage_tbl,
                                   data.table(System = paste0("SITC LLM: ", src),
                                              Products = nrow(sitc_llm_class[source == src]))))
  }
}

if (!is.null(hs_llm_class)) {
  hs_sources = unique(hs_llm_class$source)
  for (src in hs_sources) {
    coverage_tbl = rbindlist(list(coverage_tbl,
                                   data.table(System = paste0("HS LLM: ", src),
                                              Products = nrow(hs_llm_class[source == src]))))
  }
  for (src in hs_sources) {
    coverage_tbl = rbindlist(list(coverage_tbl,
                                   data.table(System = paste0("HS→SITC mapped: ", src),
                                              Products = nrow(hs_as_sitc[source == src]))))
  }
}

cat(md_table(coverage_tbl), "\n\n")

## 5.2 - Category distribution comparison ----

cat("## Category Distribution\n\n")

dist_list = list()

# Original Rauch
for (cls in c("con", "lib")) {
  tmp = rauch_class_long[classification == cls, .N, by = category]
  tmp[, share := N / sum(N) * 100]
  tmp[, source := paste0("Original (", cls, ")")]
  tmp[, system := "SITC"]
  dist_list[[length(dist_list) + 1]] = tmp
}

# SITC LLM
if (!is.null(sitc_llm_class)) {
  for (src in unique(sitc_llm_class$source)) {
    tmp = sitc_llm_class[source == src, .N, by = category]
    tmp[, share := N / sum(N) * 100]
    tmp[, source := paste0("LLM: ", src)]
    tmp[, system := "SITC"]
    dist_list[[length(dist_list) + 1]] = tmp
  }
}

# HS LLM
if (!is.null(hs_llm_class)) {
  for (src in unique(hs_llm_class$source)) {
    tmp = hs_llm_class[source == src, .N, by = category]
    tmp[, share := N / sum(N) * 100]
    tmp[, source := paste0("LLM: ", src)]
    tmp[, system := "HS"]
    dist_list[[length(dist_list) + 1]] = tmp
  }
}

dist_all = rbindlist(dist_list, use.names = TRUE, fill = TRUE)
dist_wide = dcast(dist_all, system + source ~ category, value.var = "share", fill = 0)
setcolorder(dist_wide, c("system", "source", "w", "r", "n"))

cat("### Share of products by category (%)\n\n")
cat(md_table(dist_wide), "\n\n")

## 5.3 - Agreement between classifications ----

cat("## Agreement Analysis\n\n")

if (length(common_codes) > 0 && !is.null(sitc_llm_class) && !is.null(hs_llm_class)) {
  # Compare SITC LLM vs HS→SITC mapped
  sitc_src = unique(sitc_llm_class$source)[1]  # use first SITC source
  hs_src = unique(hs_as_sitc$source)[1]  # use first HS source

  sitc_for_comp = sitc_llm_class[source == sitc_src & product_sitc_code %in% common_codes,
                                  .(product_sitc_code, sitc_cat = category)]
  hs_for_comp = hs_as_sitc[source == hs_src & product_sitc_code %in% common_codes,
                            .(product_sitc_code, hs_cat = category)]

  comp = merge(sitc_for_comp, hs_for_comp, by = "product_sitc_code")
  comp[, agree := sitc_cat == hs_cat]

  cat(sprintf("### SITC LLM (%s) vs HS→SITC (%s)\n\n", sitc_src, hs_src))
  cat(sprintf("- Common products: %d\n", nrow(comp)))
  cat(sprintf("- Agreement rate: %.1f%%\n\n", mean(comp$agree) * 100))

  # Confusion matrix
  conf = comp[, .N, by = .(sitc_cat, hs_cat)]
  conf_wide = dcast(conf, sitc_cat ~ hs_cat, value.var = "N", fill = 0)
  cat("**Confusion matrix (rows=SITC, cols=HS→SITC):**\n\n")
  cat(md_table(conf_wide), "\n\n")

  # Agreement by category
  cat("**Agreement by category:**\n\n")
  agree_by_cat = comp[, .(n = .N, agree_pct = mean(agree) * 100), by = sitc_cat]
  setnames(agree_by_cat, "sitc_cat", "category")
  cat(md_table(agree_by_cat), "\n\n")
}

# Compare Original Rauch vs SITC LLM
if (!is.null(sitc_llm_class)) {
  sitc_src = unique(sitc_llm_class$source)[1]
  rauch_con = rauch_class_long[classification == "con", .(product_sitc_code, rauch_cat = category)]
  sitc_llm = sitc_llm_class[source == sitc_src, .(product_sitc_code, llm_cat = category)]

  comp_rauch_sitc = merge(rauch_con, sitc_llm, by = "product_sitc_code")
  comp_rauch_sitc[, agree := rauch_cat == llm_cat]

  cat(sprintf("### Original Rauch (con) vs SITC LLM (%s)\n\n", sitc_src))
  cat(sprintf("- Common products: %d\n", nrow(comp_rauch_sitc)))
  cat(sprintf("- Agreement rate: %.1f%%\n\n", mean(comp_rauch_sitc$agree) * 100))
}

# Compare Original Rauch vs HS→SITC
if (!is.null(hs_llm_class)) {
  hs_src = unique(hs_as_sitc$source)[1]
  rauch_con = rauch_class_long[classification == "con", .(product_sitc_code, rauch_cat = category)]
  hs_sitc = hs_as_sitc[source == hs_src, .(product_sitc_code, hs_cat = category)]

  comp_rauch_hs = merge(rauch_con, hs_sitc, by = "product_sitc_code")
  comp_rauch_hs[, agree := rauch_cat == hs_cat]

  cat(sprintf("### Original Rauch (con) vs HS→SITC (%s)\n\n", hs_src))
  cat(sprintf("- Common products: %d\n", nrow(comp_rauch_hs)))
  cat(sprintf("- Agreement rate: %.1f%%\n\n", mean(comp_rauch_hs$agree) * 100))
}

sink()
message("Wrote comparison report: ", report_path)


# 6 - Trade shares comparison (overlapping years: 1995-2023) ----

message("Computing trade shares for SITC and HS …")

## 6.1 - SITC trade shares ----

sitc_file_for_year = function(year) {
  if (year >= 1962 && year <= 1969) return("input/trade_data/SITC/sitc_country_country_product_year_4_1962_1969.csv.gz")
  if (year >= 1970 && year <= 1979) return("input/trade_data/SITC/sitc_country_country_product_year_4_1970_1979.csv.gz")
  if (year >= 1980 && year <= 1989) return("input/trade_data/SITC/sitc_country_country_product_year_4_1980_1989.csv.gz")
  if (year >= 1990 && year <= 1999) return("input/trade_data/SITC/sitc_country_country_product_year_4_1990_1999.csv.gz")
  if (year >= 2000 && year <= 2009) return("input/trade_data/SITC/sitc_country_country_product_year_4_2000_2009.csv.gz")
  if (year >= 2010 && year <= 2019) return("input/trade_data/SITC/sitc_country_country_product_year_4_2010_2019.csv.gz")
  if (year >= 2020 && year <= 2023) return("input/trade_data/SITC/sitc_country_country_product_year_4_2020_2023.csv.gz")
  stop("Year outside supported range: ", year)
}

hs_file_for_year = function(year) {
  if (year >= 1995 && year <= 1999) return("input/trade_data/HS92/hs92_country_country_product_year_6_1995_1999.csv.gz")
  if (year >= 2000 && year <= 2009) return("input/trade_data/HS92/hs92_country_country_product_year_6_2000_2009.csv.gz")
  if (year >= 2010 && year <= 2019) return("input/trade_data/HS92/hs92_country_country_product_year_6_2010_2019.csv.gz")
  if (year >= 2020 && year <= 2023) return("input/trade_data/HS92/hs92_country_country_product_year_6_2020_2023.csv.gz")
  stop("Year outside supported range: ", year)
}

# Overlapping years
overlap_years = 1995:2023

# Prepare SITC classifications for merging
sitc_class_for_trade = rbindlist(list(
  rauch_class_long[classification == "con", .(product_sitc_code, category, source = "Original (con)", system = "SITC")],
  if (!is.null(sitc_llm_class)) sitc_llm_class[, .(product_sitc_code, category, source = paste0("LLM: ", source), system = "SITC")] else NULL
), use.names = TRUE, fill = TRUE)

# Prepare HS classifications for merging
hs_class_for_trade = NULL
if (!is.null(hs_llm_class)) {
  hs_class_for_trade = hs_llm_class[, .(product_hs_code, category, source = paste0("LLM: ", source), system = "HS")]
}

# Compute SITC shares
sitc_share_list = list()
sitc_files = unique(vapply(overlap_years, sitc_file_for_year, character(1)))

for (sitc_file in sitc_files) {
  if (!file.exists(sitc_file)) next
  message("Reading SITC: ", basename(sitc_file))
  trade_dt = fread(
    sitc_file,
    select = c("country_iso3_code", "partner_iso3_code", "product_sitc_code", "year", "export_value"),
    showProgress = FALSE,
    colClasses = list(character = c("country_iso3_code", "partner_iso3_code", "product_sitc_code"),
                      integer = "year", numeric = "export_value")
  )
  trade_dt[, product_sitc_code := pad4(product_sitc_code)]
  trade_dt = trade_dt[year %in% overlap_years]

  trade_with_class = merge(trade_dt, sitc_class_for_trade, by = "product_sitc_code", all.x = TRUE, allow.cartesian = TRUE)
  trade_with_class = trade_with_class[!is.na(category)]

  totals = trade_with_class[, .(total = sum(export_value, na.rm = TRUE)), by = .(year, source, system)]
  shares = trade_with_class[, .(value = sum(export_value, na.rm = TRUE)), by = .(year, source, system, category)]
  shares = merge(shares, totals, by = c("year", "source", "system"))
  shares[, share_pct := value / total * 100]

  sitc_share_list[[length(sitc_share_list) + 1]] = shares
}

# Compute HS shares
hs_share_list = list()
if (!is.null(hs_class_for_trade)) {
  hs_files = unique(vapply(overlap_years, hs_file_for_year, character(1)))

  for (hs_file in hs_files) {
    if (!file.exists(hs_file)) next
    message("Reading HS: ", basename(hs_file))
    trade_dt = fread(
      hs_file,
      select = c("country_iso3_code", "partner_iso3_code", "product_hs92_code", "year", "export_value"),
      showProgress = FALSE,
      colClasses = list(character = c("country_iso3_code", "partner_iso3_code", "product_hs92_code"),
                        integer = "year", numeric = "export_value")
    )
    trade_dt[, product_hs_code := pad6(product_hs92_code)]
    trade_dt[, product_hs92_code := NULL]
    trade_dt = trade_dt[year %in% overlap_years]

    trade_with_class = merge(trade_dt, hs_class_for_trade, by = "product_hs_code", all.x = TRUE, allow.cartesian = TRUE)
    trade_with_class = trade_with_class[!is.na(category)]

    totals = trade_with_class[, .(total = sum(export_value, na.rm = TRUE)), by = .(year, source, system)]
    shares = trade_with_class[, .(value = sum(export_value, na.rm = TRUE)), by = .(year, source, system, category)]
    shares = merge(shares, totals, by = c("year", "source", "system"))
    shares[, share_pct := value / total * 100]

    hs_share_list[[length(hs_share_list) + 1]] = shares
  }
}

all_shares = rbindlist(c(sitc_share_list, hs_share_list), use.names = TRUE, fill = TRUE)

if (nrow(all_shares) > 0) {
  # Save combined shares
  fwrite(all_shares, "output/analysis/sitc_hs_trade_shares_comparison.csv")

  # Plot comparison
  plot_data = copy(all_shares)
  plot_data[, category := factor(category, levels = valid_categories,
                                 labels = category_labels[valid_categories])]
  plot_data = plot_data[category %in% levels(category)]

  # Simplify source labels for plotting
  plot_data[, source_short := gsub("LLM: ", "", source)]
  plot_data[, facet_label := paste0(system, ": ", source_short)]

  p = ggplot(plot_data, aes(x = year, y = share_pct, color = category)) +
    geom_line() +
    facet_wrap(~ facet_label, ncol = 2) +
    labs(x = "Year", y = "Share of trade (%)", color = "Category",
         title = "Trade shares by Rauch category: SITC vs HS comparison") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8))

  ggsave("output/analysis/figures/sitc_hs_trade_shares_comparison.png", p, width = 10, height = 8, dpi = 300)
  ggsave("output/analysis/figures/sitc_hs_trade_shares_comparison.pdf", p, width = 10, height = 8, dpi = 300)

  message("Saved trade shares comparison plot")
}


# 7 - Side-by-side gravity comparison ----

message("Running gravity comparison for 2010 …")

# Load gravity data
gravity_main_gz = "input/gravity/Gravity_V202211.csv.gz"

if (!file.exists(gravity_main_gz)) {
  message("Downloading CEPII gravity data …")
  download.file("https://www.cepii.fr/DATA_DOWNLOAD/gravity/data/Gravity_csv_V202211.zip",
                "input/gravity/Gravity_csv_V202211.zip", mode = "wb", quiet = TRUE)
  unzip("input/gravity/Gravity_csv_V202211.zip", exdir = "input/gravity")
  unlink("input/gravity/Gravity_csv_V202211.zip")
  system2("gzip", c("-f", list.files("input/gravity", pattern = "\\.csv$", full.names = TRUE)))
}

gravity_covars = fread(
  gravity_main_gz,
  select = c("year", "iso3_o", "iso3_d", "gdp_o", "gdp_d", "gdpcap_o", "gdpcap_d",
             "distw_harmonic", "contig", "comlang_off", "col_dep_ever",
             "country_exists_o", "country_exists_d"),
  showProgress = FALSE
)[year == 2010 & country_exists_o == 1 & country_exists_d == 1]
gravity_covars = gravity_covars[!is.na(distw_harmonic) & distw_harmonic > 0 &
                                 !is.na(gdp_o) & !is.na(gdp_d)]
gravity_covars[, `:=`(country_exists_o = NULL, country_exists_d = NULL)]

# SITC 2010 trade
sitc_2010 = fread(
  sitc_file_for_year(2010),
  select = c("country_iso3_code", "partner_iso3_code", "product_sitc_code", "year", "export_value"),
  showProgress = FALSE,
  colClasses = list(character = c("country_iso3_code", "partner_iso3_code", "product_sitc_code"),
                    integer = "year", numeric = "export_value")
)[year == 2010]
sitc_2010[, product_sitc_code := pad4(product_sitc_code)]

# HS 2010 trade
hs_2010 = fread(
  hs_file_for_year(2010),
  select = c("country_iso3_code", "partner_iso3_code", "product_hs92_code", "year", "export_value"),
  showProgress = FALSE,
  colClasses = list(character = c("country_iso3_code", "partner_iso3_code", "product_hs92_code"),
                    integer = "year", numeric = "export_value")
)[year == 2010]
hs_2010[, product_hs_code := pad6(product_hs92_code)]
hs_2010[, product_hs92_code := NULL]

# Run PPML for each system/source combination
ppml_results = list()

# SITC Original
rauch_con_class = rauch_class_long[classification == "con", .(product_sitc_code, category)]
sitc_rauch = merge(sitc_2010, rauch_con_class, by = "product_sitc_code", all.x = TRUE)
sitc_rauch = sitc_rauch[!is.na(category)]

for (cat in valid_categories) {
  bilat = sitc_rauch[category == cat, .(trade_value = sum(export_value, na.rm = TRUE)),
                     by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code)]
  tmp = merge(bilat, gravity_covars, by = c("iso3_o", "iso3_d"), all = FALSE)
  tmp = tmp[trade_value > 0]
  if (nrow(tmp) == 0) next
  tmp[, ln_dist := log(distw_harmonic)]
  tmp[, fe_o := iso3_o]
  tmp[, fe_d := iso3_d]

  model = fepois(trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | fe_o + fe_d, data = tmp)
  ct = summary(model)$coeftable
  ppml_results[[length(ppml_results) + 1]] = data.table(
    system = "SITC",
    source = "Original (con)",
    category = cat,
    beta_dist = ct["ln_dist", "Estimate"],
    se_dist = ct["ln_dist", "Std. Error"],
    n_obs = nrow(tmp)
  )
}

# SITC LLM (first source)
if (!is.null(sitc_llm_class)) {
  sitc_src = unique(sitc_llm_class$source)[1]
  sitc_llm_subset = sitc_llm_class[source == sitc_src, .(product_sitc_code, category)]
  sitc_llm_trade = merge(sitc_2010, sitc_llm_subset, by = "product_sitc_code", all.x = TRUE)
  sitc_llm_trade = sitc_llm_trade[!is.na(category)]

  for (cat in valid_categories) {
    bilat = sitc_llm_trade[category == cat, .(trade_value = sum(export_value, na.rm = TRUE)),
                           by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code)]
    tmp = merge(bilat, gravity_covars, by = c("iso3_o", "iso3_d"), all = FALSE)
    tmp = tmp[trade_value > 0]
    if (nrow(tmp) == 0) next
    tmp[, ln_dist := log(distw_harmonic)]
    tmp[, fe_o := iso3_o]
    tmp[, fe_d := iso3_d]

    model = fepois(trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | fe_o + fe_d, data = tmp)
    ct = summary(model)$coeftable
    ppml_results[[length(ppml_results) + 1]] = data.table(
      system = "SITC",
      source = paste0("LLM: ", sitc_src),
      category = cat,
      beta_dist = ct["ln_dist", "Estimate"],
      se_dist = ct["ln_dist", "Std. Error"],
      n_obs = nrow(tmp)
    )
  }
}

# HS LLM (first source)
if (!is.null(hs_llm_class)) {
  hs_src = unique(hs_llm_class$source)[1]
  hs_llm_subset = hs_llm_class[source == hs_src, .(product_hs_code, category)]
  hs_llm_trade = merge(hs_2010, hs_llm_subset, by = "product_hs_code", all.x = TRUE)
  hs_llm_trade = hs_llm_trade[!is.na(category)]

  for (cat in valid_categories) {
    bilat = hs_llm_trade[category == cat, .(trade_value = sum(export_value, na.rm = TRUE)),
                         by = .(iso3_o = country_iso3_code, iso3_d = partner_iso3_code)]
    tmp = merge(bilat, gravity_covars, by = c("iso3_o", "iso3_d"), all = FALSE)
    tmp = tmp[trade_value > 0]
    if (nrow(tmp) == 0) next
    tmp[, ln_dist := log(distw_harmonic)]
    tmp[, fe_o := iso3_o]
    tmp[, fe_d := iso3_d]

    model = fepois(trade_value ~ ln_dist + contig + comlang_off + col_dep_ever | fe_o + fe_d, data = tmp)
    ct = summary(model)$coeftable
    ppml_results[[length(ppml_results) + 1]] = data.table(
      system = "HS",
      source = paste0("LLM: ", hs_src),
      category = cat,
      beta_dist = ct["ln_dist", "Estimate"],
      se_dist = ct["ln_dist", "Std. Error"],
      n_obs = nrow(tmp)
    )
  }
}

ppml_comparison = rbindlist(ppml_results, use.names = TRUE, fill = TRUE)

if (nrow(ppml_comparison) > 0) {
  ppml_comparison[, ci_lower := beta_dist - 1.96 * se_dist]
  ppml_comparison[, ci_upper := beta_dist + 1.96 * se_dist]
  ppml_comparison[, category_label := category_labels[category]]

  fwrite(ppml_comparison, "output/analysis/sitc_hs_ppml_comparison_2010.csv")

  # Plot comparison
  ppml_comparison[, label := paste0(system, "\n", gsub("LLM: ", "", source))]

  p_gravity = ggplot(ppml_comparison, aes(x = label, y = beta_dist, fill = category_label)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  position = position_dodge(width = 0.8), width = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    labs(x = "Classification system", y = "Distance coefficient (PPML)",
         fill = "Category",
         title = "Distance elasticity by Rauch category: SITC vs HS (2010)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 8))

  ggsave("output/analysis/figures/sitc_hs_ppml_comparison_2010.png", p_gravity, width = 10, height = 6, dpi = 300)
  ggsave("output/analysis/figures/sitc_hs_ppml_comparison_2010.pdf", p_gravity, width = 10, height = 6, dpi = 300)

  message("Saved PPML comparison plot")
}


message("Done! Check output/metrics/ for comparison report and output/analysis/figures/ for plots.")

# EOF
