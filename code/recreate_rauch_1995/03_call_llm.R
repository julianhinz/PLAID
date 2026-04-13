#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 03_call_llms.R
# -------------------------------------------------------------------
# Calls OpenRouter chat completion API with configurable model.
# Priority for model selection:
#   1) --model flag
#   2) 1st positional arg
#   3) $OPENROUTER_MODEL
#   4) "openai/gpt-5"
# Saves JSON responses per SITC code for the Rauch classification
# Usage:
#   Rscript 03_call_llms.R --model openai/gpt-5 --run_id RUN123
#   Rscript 03_call_llms.R openai/gpt-5 RUN123
#   Rscript 03_call_llms.R            # uses env or default
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, jsonlite, httr, stringr, optparse)

# ────────────────────────────────────────────────────────────────────
# 1  Arguments and API key
# ────────────────────────────────────────────────────────────────────

default_model <- if (nzchar(Sys.getenv("OPENROUTER_MODEL"))) Sys.getenv("OPENROUTER_MODEL") else "openai/gpt-5"
default_runid <- format(Sys.time(), "%Y%m%d_%H%M%S")

option_list <- list(
  make_option(c("-m","--model"),  type = "character", default = default_model,
              help = "OpenRouter model id (e.g., openai/gpt-5). Can also be given positionally as the 1st arg. [default: %default]"),
  make_option(c("-i","--run_id"), type = "character", default = default_runid,
              help = "Run identifier to name the output folder. Can also be given positionally as the 2nd arg. [default: timestamp]")
)

opt_parser <- OptionParser(option_list = option_list,
                           usage = "usage: %prog [options] [MODEL] [RUN_ID]")
parsed <- parse_args(opt_parser, positional_arguments = TRUE)

# Backward-compatible positional fallbacks
posargs <- parsed$args
model   <- if (length(posargs) >= 1) posargs[[1]] else parsed$options$model
run_id  <- if (length(posargs) >= 2) posargs[[2]] else parsed$options$run_id

# Prefer: export OPENROUTER_API_KEY in your shell instead of hardcoding here
api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (identical(api_key, "")) stop("Please set OPENROUTER_API_KEY in your environment.")

message("Using model: ", model)
message("Run ID: ", run_id)

# ────────────────────────────────────────────────────────────────────
# 2  Load prompt data (sample of 100)
# ────────────────────────────────────────────────────────────────────
prompts_file <- file.path("temp", "prompts", "prompts_sitc2_rauch_full.csv")
if (!file.exists(prompts_file)) stop("Missing file: ", prompts_file)
prompts <- fread(prompts_file, colClasses = "character")

# Example of splitting/shuffling if needed:
# set.seed(124)
# prompts <- prompts[sample(.N)][1:25]
# prompts[, part := ceiling(.I / (nrow(prompts) / 20))]
# prompts <- prompts[part == 2]

is_gemini <- grepl("gemini-2.5-pro", model)

if (is_gemini) {
  sample_n <- 100L  # change this number if you want a larger/smaller sample
  if (nrow(prompts) > sample_n) {
    set.seed(1995)  # fixed seed so the sample is reproducible
    prompts <- prompts[sample(.N)][1:sample_n]
  }
  message("Gemini run restricted to ", nrow(prompts), " prompts (random sample).")
}


# ────────────────────────────────────────────────────────────────────
# 3  Output directory
# ────────────────────────────────────────────────────────────────────

llm_dir <- file.path("temp", "llm", gsub("/", "_", model), run_id)
dir.create(llm_dir, recursive = TRUE, showWarnings = FALSE)

# ────────────────────────────────────────────────────────────────────
# 4  API call function  (forces JSON into message$content)
# ────────────────────────────────────────────────────────────────────

call_openrouter <- function(prompt_text, model, code, retries = 3) {
  # Special handling only for Gemini to avoid truncation and long reasoning
  is_gemini <- grepl("gemini-2.5-pro", model)
  
  if (is_gemini) {
    # For Gemini: wrap prompt to enforce compact JSON + short reasoning
    user_text <- paste0(
      prompt_text,
      "\n\n",
      "Your reply MUST be ONLY a single valid JSON object, with no text before or after it.\n",
      "Use exactly these keys:\n",
      "  - \"code_system\" (string, always \"SITC\")\n",
      "  - \"code\" (string, the SITC code)\n",
      "  - \"short_description\" (string, <= 20 words)\n",
      "  - \"rauch_category\" (one of \"w\", \"r\", or \"n\")\n",
      "  - \"reasoning\" (ONE short sentence that directly justifies the category; no step-by-step reasoning, no headings, no self-talk)\n",
      "  - \"confidence\" (number in [0,1])\n",
      "Do NOT describe how you are thinking.\n",
      "Do NOT include multi-step reasoning, section titles, bullet points, or meta commentary.\n",
      "Do NOT include any other keys.\n",
      "Do NOT add markdown or explanations outside the JSON object."
    )
    max_toks   <- 2048   # give Gemini more room so the JSON isn't cut off
    timeout_s  <- 180    # allow more time for a longer completion
  } else {
    # For all non-Gemini models (including openai/gpt-5): keep original behavior
    user_text <- prompt_text
    max_toks  <- 768
    timeout_s <- 60
  }
  
  body <- list(
    model = model,
    messages = list(
      list(role = "system",
           content = list(list(type = "text",
                               text = "You are an expert trade economist."))),
      list(role = "user",
           content = list(list(type = "text", text = user_text)))
    ),
    temperature     = 0,
    max_tokens      = max_toks,
    response_format = list(type = "json_object")
  )
  
  # special case for reasoning models (unchanged): ONLY for gpt-5
  if (grepl("gpt-5", model)) {
    body$reasoning <- list(effort = "low")
    body$metadata  <- list(prefer_short_reasoning = TRUE)
  }
  
  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type"  = "application/json",
    "HTTP-Referer"  = "https://example.org",
    "X-Title"       = "rauch-sitc-batch"
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
      out <- httr::content(res, as = "parsed", type = "application/json")
      # Optional: if you want, you can warn when Gemini still hits length
      if (is_gemini) {
        fr <- tryCatch(out$choices[[1]]$finish_reason, error = function(e) NA_character_)
        if (identical(fr, "length")) {
          warning(sprintf("Truncated response (finish_reason=length) for code %s (model: %s)", code, model))
        }
      }
      return(out)
    }
    
    if (attempt >= retries || !(res$status_code %in% c(429, 500, 502, 503, 504))) {
      body_txt <- httr::content(res, as = "text", encoding = "UTF-8")
      warning(sprintf("API %s for code %s → %s", res$status_code, code, substr(body_txt, 1, 200)))
      return(NULL)
    }
    
    Sys.sleep(1.5 * attempt)
    attempt <- attempt + 1
  }
}

# ────────────────────────────────────────────────────────────────────
# 5  Iterate over prompts (sample only)
# ────────────────────────────────────────────────────────────────────

for (i in seq_len(nrow(prompts))) {
  if (i %% 25 == 0) {
    message(sprintf("Processed %d / %d prompts...", i, nrow(prompts)))
  }
  row <- prompts[i]
  
  out_file <- file.path(llm_dir, sprintf("%s_rauch.json", row$code))
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

