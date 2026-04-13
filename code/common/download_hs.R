#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 01_download_hs.R
# -------------------------------------------------------------------
# Minimal wrapper for the HS Rauch pipeline.
# - Reads HSCodeandDescription.xlsx and exports H0–H6 sheets as CSVs
#   into temp/hs/ with standardised column names.
# - Source: https://unstats.un.org/unsd/classifications/Econ/download/In%20Text/HSCodeandDescription.xlsx
# - Downloads HS→SITC concordances (H0–H6 → S2) into input/concordance/
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, data.table, utils, jsonlite, stringr, readr, readxl)

input_dir    <- file.path("input")
temp_dir     <- file.path("temp", "hs")
product_dir  <- file.path(input_dir, "product_descriptions")
xlsx_path    <- file.path(product_dir, "HSCodeandDescription.xlsx")
concord_zip_dir <- file.path(input_dir, "concordance")   # store downloaded ZIPs
concord_out_dir <- file.path("temp", "concordance")      # extracted/standardised CSVs

# Ensure directories exist
invisible(lapply(c(input_dir, temp_dir, product_dir, concord_zip_dir, concord_out_dir), dir.create, showWarnings = FALSE, recursive = TRUE))

stopifnot(file.exists(xlsx_path))

# Extract and convert Excel sheets to CSVs (once)
unzipped_flag <- file.path(temp_dir, ".extracted")
if (!file.exists(unzipped_flag)) {
  message("Extracting HS products from HSCodeandDescription.xlsx …")

  # Map sheet names to H0–H6 nomenclature codes
  sheet_map <- c(
    "HS92" = "H0", "HS96" = "H1", "HS02" = "H2", "HS07" = "H3",
    "HS12" = "H4", "HS17" = "H5", "HS22" = "H6"
  )

  available <- excel_sheets(xlsx_path)
  for (sheet in names(sheet_map)) {
    if (!(sheet %in% available)) {
      warning("Sheet '", sheet, "' not found in workbook; skipping.")
      next
    }
    message("Reading sheet '", sheet, "' (", sheet_map[[sheet]], ") ...")
    df <- data.table(read_excel(xlsx_path, sheet = sheet))

    # Standardise columns to match downstream expectations
    setnames(df, c("Classification", "Code", "Description", "Level"),
                 c("NomenclatureCode", "ProductCode", "ProductDescription", "Tier"),
             skip_absent = TRUE)

    # Convert Level (2/4/6) to Tier (1/2/3)
    df[, Tier := as.integer(Tier) / 2L]

    csv_path <- file.path(temp_dir, paste0(sheet_map[[sheet]], "_Nomenclature.csv"))
    write_csv(df, csv_path)
    message("Saved ", csv_path)
  }

  file.create(unzipped_flag)
  message("All nomenclature sheets processed into CSVs in ", temp_dir)
} else {
  message("HSCodeandDescription.xlsx already extracted — skipping.")
}

# Download HS→SITC concordances (starting with H5→S2)
download_concordance <- function(hs_ver = "H5", sitc_ver = "S2") {
  hs_ver <- toupper(hs_ver)
  sitc_ver <- toupper(sitc_ver)
  zip_name <- sprintf("Concordance_%s_to_%s.zip", hs_ver, sitc_ver)
  csv_name <- sprintf("Concordance_%s_to_%s.csv", hs_ver, sitc_ver)
  zip_path <- file.path(concord_zip_dir, zip_name)
  csv_path <- file.path(concord_out_dir, csv_name)

  if (file.exists(csv_path)) {
    message(csv_name, " already present — skipping download.")
    return(invisible(csv_path))
  }

  url <- sprintf("https://wits.worldbank.org/data/public/concordance/%s", zip_name)
  if (!file.exists(zip_path)) {
    message("Downloading ", zip_name, " …")
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
  }

  unzipped <- tryCatch(unzip(zip_path, exdir = concord_out_dir), error = function(e) character(0))
  if (!length(unzipped)) stop("Could not unzip concordance: ", zip_path)

  csv_found <- unzipped[grepl("\\.csv$", unzipped, ignore.case = TRUE)]
  if (!length(csv_found)) stop("No CSV found inside ", zip_path)

  file.copy(csv_found[1], csv_path, overwrite = TRUE)
  message("Saved concordance CSV: ", csv_path)

  # Clean up extracted job files to avoid duplicates (keep the standardized copy)
  for (f in unzipped) {
    if (file.exists(f) && normalizePath(f) != normalizePath(csv_path)) unlink(f)
  }
}

# Download all HS revisions H0–H6 to SITC2 and SITC4 (for different Rauch variants)
hs_versions <- paste0("H", 0:6)
sitc_versions <- c("S2", "S4")
for (hs in hs_versions) {
  for (sv in sitc_versions) {
    download_concordance(hs_ver = hs, sitc_ver = sv)
  }
}

# Remove legacy convenience copy if present to avoid duplication
legacy_path <- file.path(input_dir, "hs6_to_sitc2.csv")
if (file.exists(legacy_path)) {
  message("Removing legacy concordance copy: ", legacy_path)
  unlink(legacy_path, recursive = FALSE, force = TRUE)
}
