###
# Prepare Benchmark Datasets for Indicator Validation
# 260402
###

# Reads raw benchmark files from input/benchmarks/raw/ and produces
# the CSV files expected by each indicator's 05_validate.R script.
#
# Raw sources (downloaded manually, see input/benchmarks/README.md):
#   - BEC Rev.5:  HS2012-17-BEC5 -- 08 Nov 2018.xlsx  (UNSD)
#   - ICT goods:  ict_hs2017.xls                       (UNCTAD/OECD)
#   - 3TG:        hardcoded from EU Regulation 2017/821 Annex I
#
# Usage:
#   Rscript code/common/prepare_benchmarks.R

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, readxl, stringr)

# 0 - settings ----

raw_dir <- "input/benchmarks/raw"
out_dir <- "input/benchmarks"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 1 - BEC Rev.5 → HS 2017 concordance ----

message("--- BEC Rev.5 → HS 2017 concordance ---")

bec_raw <- as.data.table(read_excel(
  file.path(raw_dir, "HS2012-17-BEC5 -- 08 Nov 2018.xlsx"),
  sheet = "HS17BEC5"
))

# map BEC5EndUse to our three categories
# for mixed codes like INT/CONS, take the first (primary) category
bec_raw[, primary_enduse := str_extract(BEC5EndUse, "^[A-Z]+")]

enduse_map <- c(
  "CAP"  = "capital",
  "INT"  = "intermediate",
  "CONS" = "consumption"
)

bec_raw[, bec_category := enduse_map[primary_enduse]]

bec_out <- bec_raw[
  !is.na(bec_category),
  .(hs6_code = str_pad(HS6, 6, pad = "0"), bec_category)
]

# deduplicate (should be 1:1 but just in case)
bec_out <- unique(bec_out, by = "hs6_code")

fwrite(bec_out, file.path(out_dir, "bec_hs2017_concordance.csv"))
message(sprintf("  Saved %d HS6 codes: %s",
                nrow(bec_out),
                paste(bec_out[, .N, by = bec_category][order(bec_category), paste(bec_category, N, sep = "=")], collapse = ", ")))

# 2 - BEC Rev.5 → HS 2012 concordance (native source revision) ----

# The HS12BEC5 sheet in the same xlsx is BEC Rev.5 defined against HS 2012,
# which is BEC Rev.5's native source revision. Used by 06_validate_bec.R as
# the primary validation reference. Column names in this sheet:
#   HS, HS4, HS4Desc, HS6, HS6Desc, BEC5Code1, BEC5Category, BEC5EndUse, ...
# BEC5EndUse values: CAP (capital), INT (intermediate), CONS (consumption).
message("--- BEC Rev.5 → HS 2012 concordance (native) ---")

bec_h2012 <- as.data.table(read_excel(
  file.path(raw_dir, "HS2012-17-BEC5 -- 08 Nov 2018.xlsx"),
  sheet = "HS12BEC5"
))

bec_h2012[, primary_enduse := str_extract(BEC5EndUse, "^[A-Z]+")]
bec_h2012[, bec_enduse := enduse_map[primary_enduse]]

bec_h2012_long <- bec_h2012[
  !is.na(HS6) & !is.na(bec_enduse),
  .(hs6_code = str_pad(as.character(HS6), 6, pad = "0"),
    bec_enduse,
    bec_category = BEC5Category)
]

# Deduplicate: some HS6 codes appear multiple times when multiple BEC5
# categories apply; pick the most-frequent end-use per hs6_code.
bec_h2012_unique <- bec_h2012_long[
  , .(bec_enduse = names(sort(table(bec_enduse), decreasing = TRUE))[1],
      bec_category = bec_category[1]),
  by = hs6_code
]

fwrite(bec_h2012_unique, file.path(out_dir, "bec_hs2012_concordance.csv"))
message(sprintf("  Saved %d HS6 codes: %s",
                nrow(bec_h2012_unique),
                paste(bec_h2012_unique[, .N, by = bec_enduse][order(bec_enduse), paste(bec_enduse, N, sep = "=")], collapse = ", ")))

# 3 - OECD/UNCTAD ICT goods ----

message("--- OECD/UNCTAD ICT goods ---")

ict_raw <- as.data.table(read_excel(
  file.path(raw_dir, "ict_hs2017.xls")
))

# the file has messy headers; columns are unnamed/named oddly
# actual data is in columns 2 (Code) and 3 (Label), starting from row 3
setnames(ict_raw, c("header", "code", "label"))

# keep only rows where code looks like a 6-digit HS code (not ICT00, ICT01 etc.)
ict_raw[, code := trimws(as.character(code))]
ict_codes <- ict_raw[grepl("^\\d{6}$", code), .(hs6_code = code)]

# load the universe of HS6 codes to build full TRUE/FALSE benchmark
hs_files <- list.files("input/product_descriptions", pattern = "\\.csv$", full.names = TRUE)
if (length(hs_files) == 0) {
  ind_files <- list.files("output/indicators", pattern = "^rauch_hs6_.*H5\\.csv$", full.names = TRUE)
  if (length(ind_files) > 0) {
    all_hs6 <- unique(fread(ind_files[1], select = "code", colClasses = "character")$code)
  } else {
    all_hs6 <- ict_codes$hs6_code
    warning("Could not find full HS6 universe; ICT file will only contain listed codes.")
  }
} else {
  all_hs6 <- unique(unlist(lapply(hs_files, function(f) {
    dt <- fread(f, select = 1, colClasses = "character")
    str_pad(dt[[1]], 6, pad = "0")
  })))
}
all_hs6 <- str_pad(all_hs6, 6, pad = "0")

# build full benchmark: all HS6 codes with ict_good TRUE/FALSE
ict_out <- data.table(
  hs6_code = all_hs6,
  ict_good = all_hs6 %in% ict_codes$hs6_code
)

fwrite(ict_out, file.path(out_dir, "oecd_ict_hs.csv"))
message(sprintf("  Saved %d HS6 codes (%d ICT goods)",
                nrow(ict_out), sum(ict_out$ict_good)))

# 4 - EU 3TG conflict minerals (Regulation 2017/821 Annex I) ----

message("--- EU 3TG conflict minerals ---")

# hardcoded from EU Regulation 2017/821 Annex I
# CN codes truncated to HS6; "ex" prefixes dropped (partial coverage noted)
eu_3tg <- data.table(
  hs6_code = c(
    # Part A: ores
    "260900",  # tin ores and concentrates
    "261100",  # tungsten ores and concentrates
    "261590",  # tantalum/niobium ores (ex 2615 90)
    "261690",  # gold ores and concentrates (ex 2616 90)
    # Part B: metals and compounds
    "282590",  # tungsten oxides/hydroxides; tin oxides/hydroxides (ex)
    "282739",  # tin chlorides
    "284180",  # tungstates
    "284190",  # tantalates (ex 2841 90)
    "284990",  # carbides of tungsten; carbides of tantalum (ex)
    "710811",  # gold, non-monetary, powder (ex 7108)
    "710812",  # gold, non-monetary, other unwrought (ex 7108)
    "710813",  # gold, non-monetary, semi-manufactured (ex 7108)
    "720280",  # ferro-tungsten and ferro-silico-tungsten
    "800100",  # tin, unwrought
    "800300",  # tin bars, rods, profiles, wires
    "800700",  # tin, other articles
    "810110",  # tungsten, powders
    "810194",  # tungsten, unwrought (sintered bars)
    "810196",  # tungsten wire
    "810199",  # tungsten bars, rods, profiles, plates, sheets
    "810320",  # tantalum, unwrought (sintered); powders
    "810390"   # tantalum bars, rods, profiles, wire, plates, sheets
  ),
  mineral = c(
    "tin", "tungsten", "tantalum", "gold",
    "tungsten",  # 282590 covers both W and Sn oxides; primary = tungsten
    "tin", "tungsten", "tantalum", "tungsten",
    "gold", "gold", "gold",
    "tungsten",
    "tin", "tin", "tin",
    "tungsten", "tungsten", "tungsten", "tungsten",
    "tantalum", "tantalum"
  ),
  regulated = TRUE
)

fwrite(eu_3tg, file.path(out_dir, "eu_conflict_minerals_hs.csv"))
message(sprintf("  Saved %d regulated 3TG HS6 codes: %s",
                nrow(eu_3tg),
                paste(eu_3tg[, .N, by = mineral][order(mineral), paste(mineral, N, sep = "=")], collapse = ", ")))

# done ----

message("\nAll benchmark files saved to ", out_dir, "/")
message("  bec_hs2017_concordance.csv")
message("  bec_hs2012_concordance.csv")
message("  oecd_ict_hs.csv")
message("  eu_conflict_minerals_hs.csv")
