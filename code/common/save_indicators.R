#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# save_indicators.R
# -------------------------------------------------------------------
# Parses JSON outputs from any indicator LLM run into a tidy CSV.
# Reads indicator-specific parsing config from
#   code/create_hs_{indicator}/config.yaml
#
# Usage:
#   Rscript code/common/save_indicators.R --indicator rauch --model openai/gpt-5 --run_id RUN123
#   Rscript code/common/save_indicators.R --indicator bec openai/gpt-5 RUN123
#   Rscript code/common/save_indicators.R --indicator rauch  # picks latest run
#
# Writes: output/indicators/{prefix}_hs6_<YYYYMMDD>_<MODEL>_<RUN_ID>.csv
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, jsonlite, stringr, tools, optparse, yaml)

# ────────────────────────────────────────────────────────────────────
# Args
# ────────────────────────────────────────────────────────────────────
default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-5"

option_list <- list(
  make_option(c("-m","--model"),     type = "character", default = default_model,
              help = "Model id used in call_llm.R (e.g., openai/gpt-5). [default: %default]"),
  make_option(c("-i","--run_id"),    type = "character", default = NA_character_,
              help = "Run identifier (folder under temp/llm_{indicator}/<model>/). If omitted, picks latest."),
  make_option(c("-I","--indicator"), type = "character", default = NA_character_,
              help = "Indicator name (must have code/create_hs_{name}/config.yaml). [required]")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog --indicator IND [options] [MODEL] [RUN_ID]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

posargs   <- parsed$args
model     <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
run_id    <- if (length(posargs) >= 2) posargs[[2]] else parsed$options$run_id
indicator <- parsed$options$indicator

if (is.na(indicator)) stop("--indicator is required (e.g., rauch, bec, hazmat, ...)")

# ────────────────────────────────────────────────────────────────────
# Load config
# ────────────────────────────────────────────────────────────────────
config_file <- file.path("code", paste0("create_hs_", indicator), "config.yaml")
if (!file.exists(config_file)) stop("Config not found: ", config_file)
cfg <- yaml::read_yaml(config_file)
pcfg <- cfg$parse
if (is.null(pcfg)) stop("config.yaml is missing the 'parse:' section for indicator: ", indicator)

# ────────────────────────────────────────────────────────────────────
# Resolve LLM directory
# ────────────────────────────────────────────────────────────────────
path_model <- gsub("/", "_", model)
llm_root   <- file.path("temp", cfg$llm_dir_name, path_model)
if (!dir.exists(llm_root)) stop("Directory not found: ", llm_root)

if (is.na(run_id)) {
  subdirs <- list.dirs(llm_root, full.names = FALSE, recursive = FALSE)
  if (length(subdirs) == 0) stop("No run directories found in ", llm_root)
  run_id <- sort(subdirs, decreasing = TRUE)[1]
}

llm_dir <- file.path(llm_root, run_id)
if (!dir.exists(llm_dir)) stop("Run directory does not exist: ", llm_dir)
message("Parsing LLM results from: ", llm_dir)
message("Indicator: ", indicator)

# ────────────────────────────────────────────────────────────────────
# Field-type coercion helpers
# ────────────────────────────────────────────────────────────────────

coerce_field <- function(val, field_cfg) {
  type <- field_cfg$type
  if (type == "character") {
    if (is.null(val) || (length(val) == 1 && is.na(val))) return(NA_character_)
    return(as.character(val))
  }
  if (type == "numeric") {
    if (is.null(val)) return(NA_real_)
    return(suppressWarnings(as.numeric(val)))
  }
  if (type == "integer") {
    if (is.null(val)) return(NA_integer_)
    v <- suppressWarnings(as.integer(val))
    rng <- field_cfg$valid_range
    if (!is.null(rng) && !is.na(v)) {
      if (v < rng[[1]] || v > rng[[2]]) return(NA_integer_)
    }
    return(v)
  }
  if (type == "boolean") {
    if (is.logical(val) && !is.na(val)) return(val)
    if (is.null(val) || is.na(val)) return(NA)
    v <- tolower(trimws(as.character(val)))
    if (v %in% c("true", "yes", "1")) return(TRUE)
    if (v %in% c("false", "no", "0")) return(FALSE)
    if (is.numeric(val)) return(as.logical(as.integer(val)))
    return(NA)
  }
  if (type == "categorical") {
    if (is.null(val) || is.na(val)) return(NA_character_)
    v <- tolower(trimws(as.character(val)))
    # Check aliases first
    aliases <- field_cfg$aliases
    if (!is.null(aliases)) {
      for (canon in names(aliases)) {
        if (v %in% c(canon, aliases[[canon]])) return(canon)
      }
    }
    # Check valid_values
    valid <- field_cfg$valid_values
    if (!is.null(valid) && v %in% valid) return(v)
    return(NA_character_)
  }
  # Fallback: return as-is
  val
}

# Build NA value for a given type
na_for_type <- function(type) {
  if (type == "character")   return(NA_character_)
  if (type == "numeric")     return(NA_real_)
  if (type == "integer")     return(NA_integer_)
  if (type == "boolean")     return(NA)
  if (type == "categorical") return(NA_character_)
  NA
}

# ────────────────────────────────────────────────────────────────────
# Build a blank row (all NA) for error cases
# ────────────────────────────────────────────────────────────────────
blank_row <- function(code, desc_hash) {
  row <- list(code = code, desc_hash = desc_hash)
  for (fld in pcfg$fields) {
    row[[fld$column]] <- na_for_type(fld$type)
  }
  as.data.table(row)
}

# ────────────────────────────────────────────────────────────────────
# Regex fallback for a single field
# ────────────────────────────────────────────────────────────────────
regex_fallback <- function(content_text, field_cfg) {
  fname <- field_cfg$name
  type  <- field_cfg$type

  if (type %in% c("character", "categorical")) {
    pat <- sprintf('"%s"\\s*:\\s*"([^"]+)"', fname)
    m   <- stringr::str_match(content_text, pat)
    if (!is.na(m[1,2])) return(coerce_field(m[1,2], field_cfg))
  } else if (type == "numeric") {
    pat <- sprintf('"%s"\\s*:\\s*([0-9.]+)', fname)
    m   <- stringr::str_match(content_text, pat)
    if (!is.na(m[1,2])) return(coerce_field(m[1,2], field_cfg))
  } else if (type == "integer") {
    pat <- sprintf('"%s"\\s*:\\s*([0-9]+)', fname)
    m   <- stringr::str_match(content_text, pat)
    if (!is.na(m[1,2])) return(coerce_field(m[1,2], field_cfg))
  } else if (type == "boolean") {
    pat <- sprintf('"%s"\\s*:\\s*(true|false)', fname)
    m   <- stringr::str_match(content_text, pat)
    if (!is.na(m[1,2])) return(m[1,2] == "true")
  }

  # Special partial-match fallback for reasoning (avoids truncation by greedy match)
  if (fname == "reasoning") {
    pat <- sprintf('"%s"\\s*:\\s*"([^"]*)', fname)
    m   <- stringr::str_match(content_text, pat)
    if (!is.na(m[1,2])) return(m[1,2])
  }

  na_for_type(type)
}

# ────────────────────────────────────────────────────────────────────
# Indicator-specific fallback for the primary categorical/boolean field
# Uses extra fallback_patterns if defined in config
# ────────────────────────────────────────────────────────────────────
primary_fallback <- function(fallback_text, primary_fname, field_cfg) {
  # Use config-defined fallback_patterns if present
  fb_patterns <- pcfg$fallback_patterns
  if (!is.null(fb_patterns) && !is.null(fb_patterns[[primary_fname]])) {
    pats <- fb_patterns[[primary_fname]]
    for (p in pats) {
      if (!is.null(p$pattern)) {
        full_pat <- if (!is.null(p$flags)) paste0(p$flags, p$pattern) else p$pattern
        m <- stringr::str_match(fallback_text, full_pat)
        if (!is.na(m[1,2])) return(coerce_field(m[1,2], field_cfg))
      }
      if (!is.null(p$backtick_pattern)) {
        m_all <- stringr::str_match_all(fallback_text, p$backtick_pattern)[[1]]
        if (!is.null(m_all) && nrow(m_all) > 0) {
          last_val <- utils::tail(m_all[, 2], 1)
          return(coerce_field(last_val, field_cfg))
        }
      }
      if (!is.null(p$text_patterns)) {
        for (canon in names(p$text_patterns)) {
          pats_for <- p$text_patterns[[canon]]
          for (tp in pats_for) {
            if (grepl(tp, fallback_text, ignore.case = TRUE)) return(canon)
          }
        }
      }
    }
  }

  # Generic fallback: just try regex on the primary field
  regex_fallback(fallback_text, field_cfg)
}

# ────────────────────────────────────────────────────────────────────
# Parse one JSON file
# ────────────────────────────────────────────────────────────────────
parse_one <- function(file) {
  fname     <- basename(file)
  suffix    <- cfg$file_suffix
  # Escape any regex metacharacters in suffix (e.g. "3tg" is fine, but be safe)
  pat       <- sprintf("^([0-9]{5,6})_([a-f0-9]+)_%s(?:.*)?[.]json$", suffix)
  m         <- stringr::str_match(fname, pat)
  if (is.na(m[1,2])) {
    message("[skip] Could not parse filename: ", fname)
    return(NULL)
  }
  code      <- m[1,2]
  desc_hash <- m[1,3]

  obj <- tryCatch(jsonlite::read_json(file, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) {
    warning("Could not read JSON: ", fname)
    return(blank_row(code, desc_hash))
  }

  msg <- tryCatch(obj$choices[[1]]$message, error = function(e) NULL)
  if (is.null(msg)) {
    warning("No 'message' in JSON: ", fname)
    return(blank_row(code, desc_hash))
  }

  # Extract content text
  content_text <- tryCatch(msg$content, error = function(e) "")
  if (is.list(content_text)) {
    content_text <- tryCatch(content_text[[1]]$text, error = function(e) "")
  }
  if (is.null(content_text)) content_text <- ""

  # Extract reasoning / reasoning_details for fallback
  reason_text <- tryCatch(msg$reasoning, error = function(e) "")
  if (is.null(reason_text)) reason_text <- ""
  if (is.list(reason_text)) reason_text <- paste(unlist(reason_text), collapse = " ")

  extra_text <- ""
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

  # Strip markdown code blocks
  content_text <- gsub("^```json[[:space:]]*", "", content_text)
  content_text <- gsub("[[:space:]]*```[[:space:]]*$", "", content_text)
  content_text <- trimws(content_text)

  # Initialise result list with code/desc_hash
  result <- list(code = code, desc_hash = desc_hash)
  for (fld in pcfg$fields) {
    result[[fld$column]] <- na_for_type(fld$type)
  }

  # Parse JSON content
  if (nzchar(content_text)) {
    parsed_inner <- tryCatch(jsonlite::fromJSON(content_text), error = function(e) NULL)

    if (is.list(parsed_inner)) {
      for (fld in pcfg$fields) {
        raw <- parsed_inner[[fld$name]]
        if (!is.null(raw)) {
          result[[fld$column]] <- coerce_field(raw, fld)
        }
      }
    } else {
      # JSON parse failed — use regex fallback per field
      for (fld in pcfg$fields) {
        result[[fld$column]] <- regex_fallback(content_text, fld)
      }
    }
  }

  # Fallback for primary field(s) if still NA
  fallback_text <- paste(content_text, extra_text, sep = "\n")

  primary_fname <- pcfg$primary_field
  primary_fld   <- Filter(function(f) f$name == primary_fname, pcfg$fields)[[1]]
  primary_col   <- primary_fld$column

  if (is.na(result[[primary_col]]) && nzchar(fallback_text)) {
    result[[primary_col]] <- primary_fallback(fallback_text, primary_fname, primary_fld)
  }

  # Boolean warning for hazmat-style (both fields required)
  if (!is.null(pcfg$dedup_condition) && pcfg$dedup_condition == "both") {
    primary2_fname <- pcfg$primary_field2
    if (!is.null(primary2_fname)) {
      p2fld <- Filter(function(f) f$name == primary2_fname, pcfg$fields)[[1]]
      p2col <- p2fld$column
      # Also attempt fallback on field2
      if (is.na(result[[p2col]]) && nzchar(fallback_text)) {
        result[[p2col]] <- regex_fallback(fallback_text, p2fld)
      }
      # Emit warnings like original hazmat script
      if (p2fld$type == "boolean") {
        if (!is.logical(result[[p2col]]) || is.na(result[[p2col]])) {
          warning("Could not parse '", primary2_fname, "' boolean for code: ", code)
        }
      }
      if (primary_fld$type == "boolean") {
        if (!is.logical(result[[primary_col]]) || is.na(result[[primary_col]])) {
          warning("Could not parse '", primary_fname, "' boolean for code: ", code)
        }
      }
    }
  }

  as.data.table(result)
}

# ────────────────────────────────────────────────────────────────────
# Run parsing
# ────────────────────────────────────────────────────────────────────
suffix  <- cfg$file_suffix
pattern <- sprintf("^[0-9]{5,6}_.*%s.*\\.json$", suffix)
json_files <- list.files(llm_dir, pattern = pattern, full.names = TRUE)
if (length(json_files) == 0) stop("No JSON files found in: ", llm_dir)

dt <- rbindlist(lapply(json_files, parse_one), fill = TRUE)

# Deduplicate: drop rows where primary field is NA
primary_col <- pcfg$primary_field

if (!is.null(pcfg$dedup_condition) && pcfg$dedup_condition == "both") {
  primary2_col <- pcfg$primary_field2
  dt <- unique(dt[!is.na(get(primary_col)) & !is.na(get(primary2_col))])
} else {
  dt <- unique(dt[!is.na(get(primary_col))])
}

if (nrow(dt) == 0L) stop("No parsed results. Check filename pattern and model outputs.")

# ────────────────────────────────────────────────────────────────────
# Post-processing (indicator-specific rules from config)
# ────────────────────────────────────────────────────────────────────
if (!is.null(pcfg$post_process)) {
  for (rule in pcfg$post_process) {
    if (!is.null(rule$warn_if)) {
      # Evaluate warn condition
      n_bad <- tryCatch({
        expr_str <- rule$warn_if
        # For 3tg: conflict_mineral==TRUE and specific_mineral==none or NA
        if (grepl("conflict_mineral == TRUE", expr_str)) {
          nrow(dt[conflict_mineral == TRUE & (is.na(specific_mineral) | specific_mineral == "none")])
        } else {
          0L
        }
      }, error = function(e) 0L)
      if (n_bad > 0) {
        warning(sprintf(rule$message, n_bad))
      }
    }
    if (!is.null(rule$rule)) {
      rule_str <- rule$rule
      if (grepl("conflict_mineral == FALSE", rule_str)) {
        dt[conflict_mineral == FALSE & (is.na(specific_mineral) | specific_mineral != "none"),
           specific_mineral := "none"]
      }
    }
  }
}

# 3tg-specific summary message
if (indicator == "3tg" && "conflict_mineral" %in% names(dt)) {
  message(sprintf("Parsed %d rows: %d conflict minerals, %d non-conflict.",
                  nrow(dt),
                  sum(dt$conflict_mineral, na.rm = TRUE),
                  sum(!dt$conflict_mineral, na.rm = TRUE)))
}

# ────────────────────────────────────────────────────────────────────
# Save outputs
# ────────────────────────────────────────────────────────────────────
out_dir <- file.path("output", "indicators")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

today       <- format(Sys.Date(), "%Y%m%d")
safe_model  <- gsub("[^A-Za-z0-9]+", "-", model)
safe_run_id <- gsub("[^A-Za-z0-9]+", "-", run_id)
prefix      <- pcfg$output_prefix

outfile <- file.path(
  out_dir,
  sprintf("%s_hs6_%s_%s_%s.csv", prefix, today, safe_model, safe_run_id)
)

fwrite(dt, outfile)
message("Saved: ", outfile)
