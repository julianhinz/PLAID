#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# 99_consistency_checks.R
# -------------------------------------------------------------------
# Cross-model consistency checks for all 6 indicators.
# Loads the latest deduped indicator CSVs for each model, computes
# pairwise agreement, Cohen's kappa, Spearman correlations, and
# confusion matrices.
#
# Usage:
#   Rscript code/99_consistency_checks.R
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, digest)

# ────────────────────────────────────────────────────────────────────
# Configuration
# ────────────────────────────────────────────────────────────────────
models <- c(
  "mistralai-mistral-small-2603",
  "openai-gpt-5-4-mini",
  "anthropic-claude-haiku-4-5",
  "google-gemini-2-5-flash"
)

model_short <- c(
  "mistralai-mistral-small-2603" = "Mistral",
  "openai-gpt-5-4-mini"         = "OpenAI",
  "anthropic-claude-haiku-4-5"  = "Haiku",
  "google-gemini-2-5-flash"     = "Gemini"
)

indicators <- list(
  rauch = list(
    field    = "rauch_category",
    type     = "categorical",
    levels   = c("w", "r", "n"),
    labels   = c(w = "organized exchange", r = "reference priced", n = "differentiated")
  ),
  perishability = list(
    field    = "perishability_class",
    type     = "ordinal",
    levels   = as.character(1:5),
    labels   = c("1" = "ultra-perishable", "2" = "highly perishable",
                 "3" = "moderately perishable", "4" = "low perishability",
                 "5" = "non-perishable")
  ),
  bec = list(
    field    = "bec",
    type     = "categorical",
    levels   = c("capital", "intermediate", "consumption")
  ),
  hazmat = list(
    field    = "hazardous",
    type     = "boolean"
  ),
  microchip = list(
    field    = "microchip_content",
    type     = "boolean"
  ),
  conflict_mineral = list(
    field    = "conflict_mineral",
    type     = "boolean",
    csv_indicator = "3tg"
  )
)

out_dir <- file.path("output", "metrics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ────────────────────────────────────────────────────────────────────
# Helper: find latest deduped CSV for a model/indicator
# ────────────────────────────────────────────────────────────────────
find_csv <- function(model, indicator_name) {
  csv_ind <- if (!is.null(indicators[[indicator_name]]$csv_indicator)) {
    indicators[[indicator_name]]$csv_indicator
  } else {
    indicator_name
  }
  pattern <- sprintf("^%s_hs6_.*%s.*deduped.*\\.csv$", csv_ind, model)
  candidates <- list.files("output/indicators", pattern = pattern, full.names = TRUE)
  if (length(candidates) == 0) return(NULL)
  candidates[which.max(file.info(candidates)$mtime)]
}

# ────────────────────────────────────────────────────────────────────
# Helper: Cohen's kappa
# ────────────────────────────────────────────────────────────────────
cohens_kappa <- function(x, y) {
  valid <- !is.na(x) & !is.na(y)
  x <- x[valid]; y <- y[valid]
  if (length(x) == 0) return(NA_real_)
  lvls <- sort(unique(c(x, y)))
  tab <- table(factor(x, levels = lvls), factor(y, levels = lvls))
  n <- sum(tab)
  po <- sum(diag(tab)) / n
  pe <- sum(rowSums(tab) * colSums(tab)) / n^2
  if (pe == 1) return(1)
  (po - pe) / (1 - pe)
}

# ────────────────────────────────────────────────────────────────────
# Load all data
# ────────────────────────────────────────────────────────────────────
message("Loading indicator CSVs...")
all_data <- list()
for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]
  field <- ind$field
  for (mod in models) {
    f <- find_csv(mod, ind_name)
    if (is.null(f)) { message("  Missing: ", mod, "/", ind_name); next }
    dt <- fread(f, colClasses = c(code = "character", desc_hash = "character"))
    # Standardise the field to character for consistency
    if (field %in% names(dt)) {
      dt[, value := as.character(get(field))]
    } else {
      message("  Field '", field, "' not found in ", f)
      next
    }
    key <- paste(ind_name, mod, sep = "::")
    all_data[[key]] <- dt[, .(code, desc_hash, value, confidence)]
  }
}

# ────────────────────────────────────────────────────────────────────
# Compute pairwise statistics
# ────────────────────────────────────────────────────────────────────
message("\nComputing pairwise statistics...")

results <- list()

for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]

  for (i in 1:(length(models) - 1)) {
    for (j in (i + 1):length(models)) {
      m1 <- models[i]; m2 <- models[j]
      k1 <- paste(ind_name, m1, sep = "::")
      k2 <- paste(ind_name, m2, sep = "::")

      if (is.null(all_data[[k1]]) || is.null(all_data[[k2]])) next

      # Join on (code, desc_hash)
      merged <- merge(
        all_data[[k1]][, .(code, desc_hash, v1 = value, c1 = confidence)],
        all_data[[k2]][, .(code, desc_hash, v2 = value, c2 = confidence)],
        by = c("code", "desc_hash")
      )

      valid <- !is.na(merged$v1) & !is.na(merged$v2)
      n <- sum(valid)
      if (n == 0) next

      v1 <- merged$v1[valid]
      v2 <- merged$v2[valid]

      # Exact agreement
      agree <- sum(v1 == v2)
      agree_pct <- 100 * agree / n

      # Cohen's kappa
      kappa <- cohens_kappa(v1, v2)

      # Type-specific metrics
      spearman <- within1 <- NA_real_
      if (ind$type == "ordinal") {
        n1 <- as.numeric(v1); n2 <- as.numeric(v2)
        spearman <- cor(n1, n2, method = "spearman", use = "complete.obs")
        within1 <- 100 * sum(abs(n1 - n2) <= 1) / n
      }

      results[[length(results) + 1]] <- data.table(
        indicator   = ind_name,
        model1      = model_short[m1],
        model2      = model_short[m2],
        n           = n,
        agree_pct   = round(agree_pct, 1),
        kappa       = round(kappa, 3),
        spearman    = if (!is.na(spearman)) round(spearman, 3) else NA_real_,
        within1_pct = if (!is.na(within1)) round(within1, 1) else NA_real_
      )
    }
  }
}

pairwise <- rbindlist(results)

# ────────────────────────────────────────────────────────────────────
# Compute unanimous agreement (all 4 models)
# ────────────────────────────────────────────────────────────────────
message("Computing unanimous agreement...")

unanimous <- list()
for (ind_name in names(indicators)) {
  keys <- paste(ind_name, models, sep = "::")
  dts <- all_data[keys[keys %in% names(all_data)]]
  if (length(dts) < 2) next

  # Merge all models
  merged <- dts[[1]][, .(code, desc_hash, v1 = value)]
  for (k in seq_along(dts)[-1]) {
    setnames_new <- paste0("v", k)
    merged <- merge(merged, dts[[k]][, .(code, desc_hash, value)],
                    by = c("code", "desc_hash"), all = FALSE)
    setnames(merged, "value", setnames_new)
  }

  vcols <- paste0("v", seq_along(dts))
  n_total <- nrow(merged)
  n_unanimous <- sum(apply(merged[, ..vcols], 1, function(r) {
    vals <- na.omit(r)
    length(vals) >= 2 && length(unique(vals)) == 1
  }))

  unanimous[[length(unanimous) + 1]] <- data.table(
    indicator     = ind_name,
    n_models      = length(dts),
    n_codes       = n_total,
    n_unanimous   = n_unanimous,
    unanimous_pct = round(100 * n_unanimous / n_total, 1)
  )
}

unan <- rbindlist(unanimous)

# ────────────────────────────────────────────────────────────────────
# Distribution per model
# ────────────────────────────────────────────────────────────────────
message("Computing distributions...")

distributions <- list()
for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]
  for (mod in models) {
    key <- paste(ind_name, mod, sep = "::")
    if (is.null(all_data[[key]])) next
    dt <- all_data[[key]]
    tab <- dt[!is.na(value), .N, by = value][order(value)]
    total <- sum(tab$N)
    tab[, pct := round(100 * N / total, 1)]
    tab[, model := model_short[mod]]
    tab[, indicator := ind_name]
    distributions[[length(distributions) + 1]] <- tab
  }
}

distrib <- rbindlist(distributions)

# ────────────────────────────────────────────────────────────────────
# Confusion matrices (for categorical/boolean indicators)
# ────────────────────────────────────────────────────────────────────
message("Computing confusion matrices...")

confusion_tables <- list()
for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]
  if (ind$type == "ordinal") next  # skip for ordinal

  for (i in 1:(length(models) - 1)) {
    for (j in (i + 1):length(models)) {
      m1 <- models[i]; m2 <- models[j]
      k1 <- paste(ind_name, m1, sep = "::")
      k2 <- paste(ind_name, m2, sep = "::")
      if (is.null(all_data[[k1]]) || is.null(all_data[[k2]])) next

      merged <- merge(
        all_data[[k1]][, .(code, desc_hash, v1 = value)],
        all_data[[k2]][, .(code, desc_hash, v2 = value)],
        by = c("code", "desc_hash")
      )
      valid <- !is.na(merged$v1) & !is.na(merged$v2)
      if (sum(valid) == 0) next

      tab <- merged[valid, .N, by = .(v1, v2)]
      tab[, model1 := model_short[m1]]
      tab[, model2 := model_short[m2]]
      tab[, indicator := ind_name]
      confusion_tables[[length(confusion_tables) + 1]] <- tab
    }
  }
}

confusions <- rbindlist(confusion_tables, fill = TRUE)

# ────────────────────────────────────────────────────────────────────
# Mean confidence per model
# ────────────────────────────────────────────────────────────────────
message("Computing confidence stats...")

conf_stats <- list()
for (ind_name in names(indicators)) {
  for (mod in models) {
    key <- paste(ind_name, mod, sep = "::")
    if (is.null(all_data[[key]])) next
    dt <- all_data[[key]]
    conf <- suppressWarnings(as.numeric(dt$confidence))
    conf <- conf[!is.na(conf)]
    if (length(conf) > 0) {
      conf_stats[[length(conf_stats) + 1]] <- data.table(
        indicator = ind_name,
        model     = model_short[mod],
        mean_conf = round(mean(conf), 3),
        sd_conf   = round(sd(conf), 3),
        min_conf  = round(min(conf), 3),
        median_conf = round(median(conf), 3)
      )
    }
  }
}

conf_dt <- rbindlist(conf_stats)

# ────────────────────────────────────────────────────────────────────
# Print report
# ────────────────────────────────────────────────────────────────────

cat("\n")
cat("================================================================\n")
cat("  PLAID Consistency Report — Cross-Model Agreement\n")
cat("================================================================\n\n")

# --- Pairwise agreement table ---
cat("── Pairwise Agreement ──────────────────────────────────────────\n\n")

for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]
  sub <- pairwise[indicator == ind_name]
  if (nrow(sub) == 0) next

  type_label <- switch(ind$type,
    categorical = "(categorical)",
    ordinal     = "(ordinal 1-5)",
    boolean     = "(boolean)"
  )

  cat(sprintf("  %s %s\n", toupper(ind_name), type_label))

  if (ind$type == "ordinal") {
    cat(sprintf("  %-18s %6s %7s %8s %9s %6s\n",
                "pair", "N", "exact%", "kappa", "within1%", "rho"))
    for (r in seq_len(nrow(sub))) {
      cat(sprintf("  %-8s–%-8s %6d %6.1f%% %7.3f %8.1f%% %6.3f\n",
                  sub$model1[r], sub$model2[r], sub$n[r],
                  sub$agree_pct[r], sub$kappa[r],
                  sub$within1_pct[r], sub$spearman[r]))
    }
  } else {
    cat(sprintf("  %-18s %6s %7s %8s\n", "pair", "N", "agree%", "kappa"))
    for (r in seq_len(nrow(sub))) {
      cat(sprintf("  %-8s–%-8s %6d %6.1f%% %7.3f\n",
                  sub$model1[r], sub$model2[r], sub$n[r],
                  sub$agree_pct[r], sub$kappa[r]))
    }
  }
  cat("\n")
}

# --- Unanimous agreement ---
cat("── Unanimous Agreement (all models) ───────────────────────────\n\n")
cat(sprintf("  %-18s %6s %6s %9s\n", "indicator", "models", "N", "unanimous"))
for (r in seq_len(nrow(unan))) {
  cat(sprintf("  %-18s %6d %6d %8.1f%%\n",
              unan$indicator[r], unan$n_models[r],
              unan$n_codes[r], unan$unanimous_pct[r]))
}

# --- Distribution ---
cat("\n── Classification Distribution (%) ─────────────────────────────\n\n")

for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]
  sub <- distrib[indicator == ind_name]
  if (nrow(sub) == 0) next

  cat(sprintf("  %s\n", toupper(ind_name)))

  vals <- sort(unique(sub$value))
  mods <- unique(sub$model)

  header <- sprintf("  %-10s", "model")
  for (v in vals) header <- paste0(header, sprintf(" %8s", v))
  cat(header, "\n")

  for (mod in mods) {
    row <- sprintf("  %-10s", mod)
    for (v in vals) {
      pct <- sub[model == mod & value == v, pct]
      if (length(pct) == 0) pct <- 0
      row <- paste0(row, sprintf(" %7.1f%%", pct))
    }
    cat(row, "\n")
  }
  cat("\n")
}

# --- Confidence ---
cat("── Mean Confidence ─────────────────────────────────────────────\n\n")
cat(sprintf("  %-18s %8s %8s %8s %8s\n", "indicator", "Mistral", "OpenAI", "Haiku", "Gemini"))
for (ind_name in names(indicators)) {
  vals <- sapply(c("Mistral", "OpenAI", "Haiku", "Gemini"), function(m) {
    v <- conf_dt[indicator == ind_name & model == m, mean_conf]
    if (length(v) == 0) NA_real_ else v
  })
  cat(sprintf("  %-18s %8.3f %8.3f %8.3f %8.3f\n",
              ind_name, vals[1], vals[2], vals[3], vals[4]))
}

# --- Confusion matrices for Rauch ---
cat("\n── Confusion Matrices (Rauch w/r/n) ────────────────────────────\n\n")

rauch_conf <- confusions[indicator == "rauch"]
if (nrow(rauch_conf) > 0) {
  pairs <- unique(rauch_conf[, .(model1, model2)])
  for (p in seq_len(nrow(pairs))) {
    m1 <- pairs$model1[p]; m2 <- pairs$model2[p]
    sub <- rauch_conf[model1 == m1 & model2 == m2]
    cat(sprintf("  %s (rows) vs %s (cols)\n", m1, m2))

    lvls <- c("w", "r", "n")
    cat(sprintf("  %5s", ""))
    for (l in lvls) cat(sprintf(" %6s", l))
    cat("\n")

    for (r in lvls) {
      cat(sprintf("  %5s", r))
      for (c_val in lvls) {
        n <- sub[v1 == r & v2 == c_val, N]
        if (length(n) == 0) n <- 0
        cat(sprintf(" %6d", n))
      }
      cat("\n")
    }
    cat("\n")
  }
}

# ────────────────────────────────────────────────────────────────────
# Save report to file
# ────────────────────────────────────────────────────────────────────
report_file <- file.path(out_dir, sprintf("consistency_report_%s.txt",
                                          format(Sys.time(), "%Y%m%d-%H%M%S")))

# Re-run with sink to capture output
sink(report_file)

cat("================================================================\n")
cat("  PLAID Consistency Report — Cross-Model Agreement\n")
cat(sprintf("  Generated: %s\n", Sys.time()))
cat(sprintf("  Models: %s\n", paste(models, collapse = ", ")))
cat("================================================================\n\n")

cat("── Pairwise Agreement ──────────────────────────────────────────\n\n")
for (ind_name in names(indicators)) {
  ind <- indicators[[ind_name]]
  sub <- pairwise[indicator == ind_name]
  if (nrow(sub) == 0) next
  type_label <- switch(ind$type,
    categorical = "(categorical)", ordinal = "(ordinal 1-5)", boolean = "(boolean)")
  cat(sprintf("  %s %s\n", toupper(ind_name), type_label))
  if (ind$type == "ordinal") {
    cat(sprintf("  %-18s %6s %7s %8s %9s %6s\n", "pair", "N", "exact%", "kappa", "within1%", "rho"))
    for (r in seq_len(nrow(sub))) {
      cat(sprintf("  %-8s-%-8s %6d %6.1f%% %7.3f %8.1f%% %6.3f\n",
                  sub$model1[r], sub$model2[r], sub$n[r], sub$agree_pct[r],
                  sub$kappa[r], sub$within1_pct[r], sub$spearman[r]))
    }
  } else {
    cat(sprintf("  %-18s %6s %7s %8s\n", "pair", "N", "agree%", "kappa"))
    for (r in seq_len(nrow(sub))) {
      cat(sprintf("  %-8s-%-8s %6d %6.1f%% %7.3f\n",
                  sub$model1[r], sub$model2[r], sub$n[r], sub$agree_pct[r], sub$kappa[r]))
    }
  }
  cat("\n")
}

cat("── Unanimous Agreement ─────────────────────────────────────────\n\n")
cat(sprintf("  %-18s %6s %6s %9s\n", "indicator", "models", "N", "unanimous"))
for (r in seq_len(nrow(unan))) {
  cat(sprintf("  %-18s %6d %6d %8.1f%%\n",
              unan$indicator[r], unan$n_models[r], unan$n_codes[r], unan$unanimous_pct[r]))
}

cat("\n── Classification Distribution (%) ─────────────────────────────\n\n")
for (ind_name in names(indicators)) {
  sub <- distrib[indicator == ind_name]
  if (nrow(sub) == 0) next
  cat(sprintf("  %s\n", toupper(ind_name)))
  vals <- sort(unique(sub$value))
  mods <- unique(sub$model)
  header <- sprintf("  %-10s", "model")
  for (v in vals) header <- paste0(header, sprintf(" %8s", v))
  cat(header, "\n")
  for (mod in mods) {
    row <- sprintf("  %-10s", mod)
    for (v in vals) {
      pct <- sub[model == mod & value == v, pct]
      if (length(pct) == 0) pct <- 0
      row <- paste0(row, sprintf(" %7.1f%%", pct))
    }
    cat(row, "\n")
  }
  cat("\n")
}

cat("── Mean Confidence ─────────────────────────────────────────────\n\n")
cat(sprintf("  %-18s %8s %8s %8s %8s\n", "indicator", "Mistral", "OpenAI", "Haiku", "Gemini"))
for (ind_name in names(indicators)) {
  vals <- sapply(c("Mistral", "OpenAI", "Haiku", "Gemini"), function(m) {
    v <- conf_dt[indicator == ind_name & model == m, mean_conf]
    if (length(v) == 0) NA_real_ else v
  })
  cat(sprintf("  %-18s %8.3f %8.3f %8.3f %8.3f\n",
              ind_name, vals[1], vals[2], vals[3], vals[4]))
}

cat("\n── Confusion Matrices (Rauch w/r/n) ────────────────────────────\n\n")
rauch_conf <- confusions[indicator == "rauch"]
if (nrow(rauch_conf) > 0) {
  pairs <- unique(rauch_conf[, .(model1, model2)])
  for (p in seq_len(nrow(pairs))) {
    m1 <- pairs$model1[p]; m2 <- pairs$model2[p]
    sub <- rauch_conf[model1 == m1 & model2 == m2]
    cat(sprintf("  %s (rows) vs %s (cols)\n", m1, m2))
    lvls <- c("w", "r", "n")
    cat(sprintf("  %5s", ""))
    for (l in lvls) cat(sprintf(" %6s", l))
    cat("\n")
    for (r in lvls) {
      cat(sprintf("  %5s", r))
      for (c_val in lvls) {
        n <- sub[v1 == r & v2 == c_val, N]
        if (length(n) == 0) n <- 0
        cat(sprintf(" %6d", n))
      }
      cat("\n")
    }
    cat("\n")
  }
}

sink()

message("\nReport saved to: ", report_file)
message("Done.")
