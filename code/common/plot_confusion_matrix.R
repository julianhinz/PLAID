# code/common/plot_confusion_matrix.R
# -------------------------------------------------------------------
# Standardized confusion-matrix ggplot helper with PLAID Warm Tartan palette.
#
# Usage:
#   source("code/common/plot_confusion_matrix.R")
#   df <- data.table(reference = c("w","w","r","n"), predicted = c("w","r","r","n"), n = c(90,10,80,120))
#   p <- plot_confusion_matrix(df, indicator = "rauch", labels = c("w","r","n"))
#   save_confusion_matrix(p, "output/analysis/figures/rauch_confusion_matrix.pdf", n_levels = 3)
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, scales)

PLAID_BG_CREAM <- "#f5f0eb"
PLAID_COLORS <- c(
  rauch         = "#c4553a",
  bec           = "#2c5f7c",
  perish        = "#d4a843",
  perishability = "#d4a843",
  hazmat        = "#5a8a6a",
  micro         = "#8b5e3c",
  microchip     = "#8b5e3c",
  tg            = "#6b4c7a",
  `3tg`         = "#6b4c7a"
)

#' Build a confusion-matrix ggplot with row-normalized fills.
#'
#' @param df         data.frame with columns `reference`, `predicted`, `n`
#' @param indicator  one of names(PLAID_COLORS)
#' @param labels     ordered vector of category labels (factor levels)
#' @param title      optional plot title (NULL for none)
#' @return           ggplot object
plot_confusion_matrix <- function(df, indicator, labels, title = NULL) {
  stopifnot(indicator %in% names(PLAID_COLORS))
  stopifnot(all(c("reference", "predicted", "n") %in% names(df)))

  accent <- PLAID_COLORS[[indicator]]
  dt <- as.data.table(df)

  # Ensure every (reference, predicted) cell exists (fill zeros)
  grid <- CJ(reference = labels, predicted = labels)
  dt <- merge(grid, dt, by = c("reference", "predicted"), all.x = TRUE)
  dt[is.na(n), n := 0L]

  # Row-normalize
  dt[, row_total := sum(n), by = reference]
  dt[, share := ifelse(row_total > 0, n / row_total, 0)]
  dt[, label := ifelse(n == 0, "0",
                       sprintf("%d\n%.1f%%", n, 100 * share))]
  dt[, text_color := ifelse(share > 0.5, "white", "#2c1810")]

  dt[, reference := factor(reference, levels = rev(labels))]
  dt[, predicted := factor(predicted, levels = labels)]

  ggplot(dt, aes(x = predicted, y = reference, fill = share)) +
    geom_tile(color = "#ddd3c6", linewidth = 0.4) +
    geom_text(aes(label = label, color = text_color), size = 3.6,
              family = "serif", lineheight = 0.9) +
    scale_fill_gradient(low = PLAID_BG_CREAM, high = accent,
                        limits = c(0, 1), labels = percent_format(accuracy = 1),
                        name = "row share") +
    scale_color_identity() +
    labs(x = "LLM consensus prediction",
         y = "Benchmark reference",
         title = title) +
    coord_fixed() +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0, face = "bold"),
          plot.background = element_rect(fill = "white", color = NA))
}

#' Save a confusion-matrix plot with PLAID's standard dimensions.
#'
#' @param p        ggplot from plot_confusion_matrix()
#' @param path     output path (PDF)
#' @param n_levels number of categories (2 or 3+); controls dimensions
save_confusion_matrix <- function(p, path, n_levels) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  side <- if (n_levels <= 2) 4 else 5
  ggsave(path, plot = p, device = cairo_pdf,
         width = side, height = side, dpi = 300)
  message("Wrote: ", path)
}
