#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# call_llm.R
# -------------------------------------------------------------------
# Calls OpenRouter chat completion API for any indicator pipeline.
# Reads a prompts CSV, sends each prompt to the LLM, and writes
# one JSON response per (code, description) pair.
#
# Usage:
#   Rscript code/common/call_llm.R MODEL RUN_ID --indicator rauch --prompts FILE
#   Rscript code/common/call_llm.R openai/gpt-5 deduped-20260328 --indicator bec
#   Rscript code/common/call_llm.R --indicator rauch   # uses defaults + latest prompts
#
# The --indicator flag determines:
#   - system prompt
#   - output directory (temp/llm_{indicator}/)
#   - JSON filename suffix ({code}_{hash}_{indicator}.json)
#   - Gemini JSON schema hint
#   - token limits
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, jsonlite, httr, stringr, optparse, digest, yaml)

# ────────────────────────────────────────────────────────────────────
# Args and defaults
# ────────────────────────────────────────────────────────────────────
default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-5"
default_runid <- format(Sys.time(), "%Y%m%d_%H%M%S")

option_list <- list(
  make_option(c("-m","--model"),     type = "character", default = default_model,
              help = "OpenRouter model id. Can also be given positionally as 1st arg."),
  make_option(c("-i","--run_id"),    type = "character", default = default_runid,
              help = "Run identifier. Can also be given positionally as 2nd arg."),
  make_option(c("-I","--indicator"), type = "character", default = NA_character_,
              help = "Indicator name (must have code/create_hs_{name}/config.yaml). [required]"),
  make_option(c("-p","--prompts"),   type = "character", default = NA_character_,
              help = "Path to prompts CSV. Default: newest matching file in temp/prompts."),
  make_option(c("-n","--sample_n"),  type = "integer",   default = 0L,
              help = "If >0, randomly sample this many prompts.")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog [options] [MODEL] [RUN_ID]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

posargs <- parsed$args
model      <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
run_id     <- if (length(posargs) >= 2) posargs[[2]] else parsed$options$run_id
indicator  <- parsed$options$indicator
prompts_override <- parsed$options$prompts
sample_n   <- parsed$options$sample_n

if (is.na(indicator)) stop("--indicator is required (e.g., rauch, bec, hazmat, ...)")

# Load indicator-specific config from pipeline directory
config_file <- file.path("code", paste0("create_hs_", indicator), "config.yaml")
if (!file.exists(config_file)) stop("Config not found: ", config_file,
  "\n  Expected at code/create_hs_", indicator, "/config.yaml")
cfg <- yaml::read_yaml(config_file)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (identical(api_key, "")) stop("Please set OPENROUTER_API_KEY in your environment.")

message("Using model: ", model)
message("Run ID: ", run_id)
message("Indicator: ", indicator)

# ────────────────────────────────────────────────────────────────────
# Prompts
# ────────────────────────────────────────────────────────────────────
if (!is.na(prompts_override)) {
  prompts_file <- prompts_override
  if (!file.exists(prompts_file)) stop("Prompts file not found: ", prompts_file)
} else {
  cand <- list.files(file.path("temp", "prompts"), pattern = cfg$prompts_pattern, full.names = TRUE)
  if (length(cand) == 0) stop("No prompts file found matching: ", cfg$prompts_pattern)
  prompts_file <- cand[which.max(file.info(cand)$mtime)]
}

message("Prompts file: ", prompts_file)
prompts <- fread(prompts_file, colClasses = "character")

if (sample_n > 0 && nrow(prompts) > sample_n) {
  set.seed(1995)
  prompts <- prompts[sample(.N)][1:sample_n]
  message("Sampling ", nrow(prompts), " prompts (requested sample_n = ", sample_n, ").")
}

# ────────────────────────────────────────────────────────────────────
# Output directory
# ────────────────────────────────────────────────────────────────────
llm_dir <- file.path("temp", cfg$llm_dir_name, gsub("/", "_", model), run_id)
dir.create(llm_dir, recursive = TRUE, showWarnings = FALSE)

# ────────────────────────────────────────────────────────────────────
# API call helper
# ────────────────────────────────────────────────────────────────────
call_openrouter <- function(prompt_text, model, code, retries = 3) {
  is_gemini <- grepl("gemini-2.5-pro", model)

  if (is_gemini) {
    user_text <- paste0(
      prompt_text,
      "\n\nYour reply MUST be ONLY a single valid JSON object, with no text before or after it.\n",
      "Use exactly these keys:\n",
      cfg$gemini_keys, "\n",
      "Do NOT add markdown or any extra text."
    )
    max_toks  <- cfg$max_toks_gemini
    timeout_s <- 180
  } else {
    user_text <- prompt_text
    max_toks  <- cfg$max_toks
    timeout_s <- 60
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system",
           content = list(list(type = "text", text = cfg$system_prompt))),
      list(role = "user",
           content = list(list(type = "text", text = user_text)))
    ),
    temperature     = 0,
    max_tokens      = max_toks,
    response_format = list(type = "json_object")
  )

  if (grepl("gpt-5", model)) {
    body$reasoning <- list(effort = "low")
    body$metadata  <- list(prefer_short_reasoning = TRUE)
  }

  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type"  = "application/json",
    "HTTP-Referer"  = "https://example.org",
    "X-Title"       = paste0("hs-", cfg$file_suffix, "-batch")
  )

  attempt <- 1
  repeat {
    res <- httr::POST(
      url    = "https://openrouter.ai/api/v1/chat/completions",
      headers,
      body   = jsonlite::toJSON(body, auto_unbox = TRUE),
      encode = "json",
      httr::timeout(timeout_s)
    )

    if (res$status_code < 300) {
      return(httr::content(res, as = "parsed", type = "application/json"))
    }

    if (attempt >= retries || !(res$status_code %in% c(429, 500, 502, 503, 504))) {
      body_txt <- httr::content(res, as = "text", encoding = "UTF-8")
      warning(sprintf("API %s for code %s -> %s", res$status_code, code, substr(body_txt, 1, 200)))
      return(NULL)
    }

    Sys.sleep(1.5 * attempt)
    attempt <- attempt + 1
  }
}

# ────────────────────────────────────────────────────────────────────
# Iterate over prompts
# ────────────────────────────────────────────────────────────────────
for (i in seq_len(nrow(prompts))) {
  if (i %% 50 == 0) message(sprintf("Processed %d / %d prompts...", i, nrow(prompts)))
  row <- prompts[i]

  # Use description hash in filename to disambiguate same code with different descriptions
  desc_hash <- if ("description" %in% names(row) && !is.na(row$description) && nzchar(row$description)) {
    substr(digest::digest(row$description, algo = "md5"), 1, 8)
  } else { "0" }
  out_file <- file.path(llm_dir, sprintf("%s_%s_%s.json", row$code, desc_hash, cfg$file_suffix))
  if (file.exists(out_file)) next

  result <- tryCatch(call_openrouter(row$prompt, model, row$code), error = function(e) NULL)
  if (is.null(result)) {
    warning("API call failed: ", row$code)
    next
  }

  write_json(result, out_file, pretty = TRUE, auto_unbox = TRUE)
  Sys.sleep(0.1)
}

file.create(file.path(llm_dir, ".done"))
message("LLM responses written to ", llm_dir)
