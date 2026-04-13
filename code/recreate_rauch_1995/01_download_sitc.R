#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 01_download_sitc.R
# -------------------------------------------------------------------
# Downloads sitcproducts.xls (if absent) and saves to input/
# Exports ALL Excel sheets to CSVs under temp/sitc/
# Usage: Rscript 01_download_sitc.R
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, data.table, utils, jsonlite, stringr, readr, readxl, tools)

args         <- commandArgs(trailingOnly = TRUE)
input_dir    <- file.path("input")
product_dir  <- file.path(input_dir, "product_descriptions")
temp_dir     <- file.path("temp", "sitc")
xls_url      <- "https://wits.worldbank.org/data/public/sitcproducts.xls"
xls_path     <- file.path(product_dir, "sitcproducts.xls")

# Create necessary directories
invisible(lapply(c(input_dir, product_dir, temp_dir), dir.create, showWarnings = FALSE, recursive = TRUE))

# Download workbook if not already present (retry once if needed)
if (!file.exists(xls_path)) {
  message("Downloading sitcproducts.xls …")
  ok <- tryCatch({
    download.file(xls_url, xls_path, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) FALSE)
  
  if (!ok || !file.exists(xls_path) || file.size(xls_path) == 0) {
    message("First attempt failed, retrying with httr …")
    resp <- tryCatch(httr::GET(xls_url, httr::write_disk(xls_path, overwrite = TRUE), httr::timeout(60)),
                     error = function(e) NULL)
    if (is.null(resp) || httr::http_error(resp) || !file.exists(xls_path) || file.size(xls_path) == 0) {
      stop("Could not download ", xls_url)
    }
  }
}

# Read workbook + export all sheets
message("Reading workbook: ", xls_path)
sheets <- readxl::excel_sheets(xls_path)
if (length(sheets) == 0) stop("No sheets found in ", xls_path)

message("Found sheets: ", paste(sheets, collapse = " | "))

for (sheet in sheets) {
  message("Processing sheet '", sheet, "' …")
  df <- readxl::read_excel(xls_path, sheet = sheet)
  
  # Sanitize filename: lower, underscores, no slashes
  fname <- sheet |>
    tolower() |>
    gsub("[^a-z0-9]+", "_", x = _) |>
    gsub("^_|_$", "", x = _)
  
  csv_path <- file.path(temp_dir, paste0(fname, ".csv"))
  readr::write_csv(df, csv_path)
  message("Saved: ", csv_path)
}

message("All SITC sheets exported to CSV in ", temp_dir)
