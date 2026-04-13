#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 04_parse_llm_sitc_rauch.R
# -------------------------------------------------------------------
# Parses JSON outputs from LLM classification runs (any prompt variant)
# Usage:
#   Rscript 04_parse_llm_sitc_rauch.R --model openai/gpt-5 --run_id RUN123
#   Rscript 04_parse_llm_sitc_rauch.R openai/gpt-5 RUN123
#   Rscript 04_parse_llm_sitc_rauch.R  # uses env or default; latest run
# Writes: output/indicators/rauch_sitc2_<YYYYMMDD>_<MODEL>_<RUN_ID>.csv
#   where <MODEL> is a filename-safe version of the model id
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, jsonlite, stringr, tools, optparse)

# ────────────────────────────────────────────────────────────────────
# 1) Args
# ────────────────────────────────────────────────────────────────────
default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-5"

option_list <- list(
  make_option(c("-m","--model"),  type = "character", default = default_model,
              help = "Model id used in 03_call_llms.R (e.g., openai/gpt-5). [default: %default]"),
  make_option(c("-i","--run_id"), type = "character", default = NA_character_,
              help = "Run identifier (folder under temp/llm/<model>/). If omitted, pick latest.")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog [options] [MODEL] [RUN_ID]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

posargs <- parsed$args
model   <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
run_id  <- if (length(posargs) >= 2) posargs[[2]] else parsed$options$run_id

# for directory naming, keep the old behavior (just replace "/")
path_model <- gsub("/", "_", model)
llm_root   <- file.path("temp", "llm", path_model)
if (!dir.exists(llm_root)) stop("Directory not found: ", llm_root)

if (is.na(run_id)) {
  subdirs <- list.dirs(llm_root, full.names = FALSE, recursive = FALSE)
  if (length(subdirs) == 0) stop("No run directories found in ", llm_root)
  run_id <- sort(subdirs, decreasing = TRUE)[1]
}

llm_dir <- file.path(llm_root, run_id)
if (!dir.exists(llm_dir)) stop("Run directory does not exist: ", llm_dir)
message("Parsing LLM results from: ", llm_dir)

# ────────────────────────────────────────────────────────────────────
# 2) Helpers
# ────────────────────────────────────────────────────────────────────
map_rauch <- function(val) {
  val <- tolower(as.character(val))
  if (val %in% c("w","organized-exchange","homogeneous")) return("w")
  if (val %in% c("r","reference-priced"))                 return("r")
  if (val %in% c("n","differentiated"))                   return("n")
  NA_character_
}

parse_one <- function(file) {
  fname <- basename(file)
  m <- stringr::str_match(fname, "^([0-9]{1,5})_rauch(?:.*)?\\.json$")
  if (is.na(m[1,2])) {
    message("[skip] Could not parse filename: ", fname)
    return(NULL)
  }
  code <- m[1,2]
  
  # Read top-level JSON (OpenRouter-style)
  obj <- tryCatch(jsonlite::read_json(file, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) {
    warning("Could not read JSON: ", fname)
    return(data.table::data.table(
      code             = code,
      short_description = NA_character_,
      rauch_category    = NA_character_,
      reasoning         = NA_character_,
      confidence        = NA_real_
    ))
  }
  
  # Safely get the message object
  msg <- tryCatch(obj$choices[[1]]$message, error = function(e) NULL)
  if (is.null(msg)) {
    warning("No 'message' in JSON: ", fname)
    return(data.table::data.table(
      code             = code,
      short_description = NA_character_,
      rauch_category    = NA_character_,
      reasoning         = NA_character_,
      confidence        = NA_real_
    ))
  }
  
  # ------------------------------------------------------------------
  # 1) Extract the inner JSON string (content_text) if present
  # ------------------------------------------------------------------
  content_text <- tryCatch(msg$content, error = function(e) "")
  if (is.list(content_text)) {
    content_text <- tryCatch(content_text[[1]]$text, error = function(e) "")
  }
  if (is.null(content_text)) content_text <- ""
  
  # ------------------------------------------------------------------
  # 2) Collect any free-text reasoning to use as fallback signal
  #    (for cases where content_text is empty or malformed)
  # ------------------------------------------------------------------
  extra_text <- ""
  
  # message$reasoning
  reason_text <- tryCatch(msg$reasoning, error = function(e) "")
  if (is.null(reason_text)) reason_text <- ""
  if (is.list(reason_text)) reason_text <- paste(unlist(reason_text), collapse = " ")
  
  # message$reasoning_details[[i]]$text
  rd <- tryCatch(msg$reasoning_details, error = function(e) NULL)
  if (!is.null(rd) && length(rd) > 0) {
    rd_texts <- tryCatch(
      vapply(rd, function(x) x$text, FUN.VALUE = character(1)),
      error = function(e) character()
    )
    extra_text <- paste(reason_text, paste(rd_texts, collapse = "\n"), sep = "\n")
  } else {
    extra_text <- reason_text
  }
  
  # ------------------------------------------------------------------
  # 3) Try to parse the JSON in content_text first
  # ------------------------------------------------------------------
  rauch_val <- reasoning <- short_desc <- NA_character_
  conf_val  <- NA_real_
  
  if (nzchar(content_text)) {
    parsed_inner <- tryCatch(jsonlite::fromJSON(content_text), error = function(e) NULL)
    
    if (is.list(parsed_inner)) {
      # Normal, non-truncated case
      if (!is.null(parsed_inner$rauch_category))    rauch_val  <- map_rauch(parsed_inner$rauch_category)
      if (!is.null(parsed_inner$reasoning))         reasoning  <- as.character(parsed_inner$reasoning)
      if (!is.null(parsed_inner$short_description)) short_desc <- as.character(parsed_inner$short_description)
      if (!is.null(parsed_inner$confidence))        conf_val   <- suppressWarnings(as.numeric(parsed_inner$confidence))
    } else {
      # JSON parsing failed (e.g. truncated mid-string). Try to salvage from content_text.
      m_cat <- stringr::str_match(content_text, '"rauch_category"\\s*:\\s*"([^"]+)"')
      if (!is.na(m_cat[1,2])) rauch_val <- map_rauch(m_cat[1,2])
      
      m_sd <- stringr::str_match(content_text, '"short_description"\\s*:\\s*"([^"]+)"')
      if (!is.na(m_sd[1,2])) short_desc <- m_sd[1,2]
      
      m_reas <- stringr::str_match(content_text, '"reasoning"\\s*:\\s*"([^"]*)')
      if (!is.na(m_reas[1,2])) reasoning <- m_reas[1,2]
      
      m_conf <- stringr::str_match(content_text, '"confidence"\\s*:\\s*([0-9.]+)')
      if (!is.na(m_conf[1,2])) conf_val <- suppressWarnings(as.numeric(m_conf[1,2]))
    }
  }
  
  # ------------------------------------------------------------------
  # 4) Fallback: infer rauch_category from any text we have
  #     - uses both content_text and "reasoning"/"reasoning_details"
  # ------------------------------------------------------------------
  fallback_text <- paste(content_text, extra_text, sep = "\n")
  
  if (is.na(rauch_val) && nzchar(fallback_text)) {
    # 4a) Try explicit JSON-style rauch_category "w"/"r"/"n"
    m_txt <- stringr::str_match(fallback_text, '(?i)"rauch_category"\\s*:\\s*"(w|r|n)"')
    if (!is.na(m_txt[1,2])) {
      rauch_val <- map_rauch(m_txt[1,2])
    } else {
      # 4b) Try backticked `w` / `r` / `n` — use the LAST mention as the final decision
      m_bt_all <- stringr::str_match_all(fallback_text, "`(w|r|n)`")[[1]]
      if (!is.null(m_bt_all) && nrow(m_bt_all) > 0) {
        last_label <- utils::tail(m_bt_all[, 2], 1)   # last captured label
        rauch_val  <- map_rauch(last_label)
      } else {
        # 4c) Last resort: full-word heuristics
        if (grepl("organized exchange|homogeneous", fallback_text, ignore.case = TRUE)) {
          rauch_val <- "w"
        } else if (grepl("reference-?priced|benchmark price", fallback_text, ignore.case = TRUE)) {
          rauch_val <- "r"
        } else if (grepl("differentiated", fallback_text, ignore.case = TRUE)) {
          rauch_val <- "n"
        }
      }
    }
  }
  
  data.table::data.table(
    code              = code,
    short_description = short_desc,
    rauch_category    = rauch_val,
    reasoning         = reasoning,
    confidence        = conf_val
  )
}

# ────────────────────────────────────────────────────────────────────
# 3) Run parsing
# ────────────────────────────────────────────────────────────────────
json_files <- list.files(llm_dir, pattern = "^[0-9]+_rauch.*\\.json$", full.names = TRUE)
if (length(json_files) == 0) stop("No JSON files found in: ", llm_dir)

dt <- rbindlist(lapply(json_files, parse_one), fill = TRUE)
dt <- unique(dt[!is.na(rauch_category)])

if (nrow(dt) == 0L) stop("No parsed results. Check filename pattern and model outputs.")

# ────────────────────────────────────────────────────────────────────
# 4) Save outputs
# ────────────────────────────────────────────────────────────────────
out_dir <- file.path("output", "indicators")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

today <- format(Sys.Date(), "%Y%m%d")

# make model string filename-safe (no slashes, spaces, etc.)
safe_model <- gsub("[^A-Za-z0-9]+", "-", model)

outfile <- file.path(
  out_dir,
  sprintf("rauch_sitc2_%s_%s_%s.csv", today, safe_model, run_id)
)

fwrite(dt, outfile)
message("Saved: ", outfile)
