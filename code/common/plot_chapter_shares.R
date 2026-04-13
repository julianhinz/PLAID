# code/common/plot_chapter_shares.R
# -------------------------------------------------------------------
# Standardized stacked-bar helper for chapter face-validity figures.
# Shares visual identity with plot_confusion_matrix.R.
#
# Usage:
#   source("code/common/plot_chapter_shares.R")
#   df <- data.table(chapter_group = ..., category = ..., share = ...)
#   p <- plot_chapter_shares(df, indicator = "hazmat",
#                            category_levels = c("non-hazardous", "hazardous"))
#   save_chapter_shares(p, "output/analysis/figures/hazmat_by_chapter.pdf")
#
# Binary chapter plots across indicators share a single neutral palette
# (slate blue + cream) so they look consistent side-by-side in the paper.
# Pass palette = "indicator" to opt back into the per-indicator accent.
# -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, scales)

# Palette is shared via plot_confusion_matrix.R (source first if not already)
if (!exists("PLAID_COLORS")) source("code/common/plot_confusion_matrix.R")

# Shared palette for chapter face-validity plots. Slate blue is visually
# distinct from any per-indicator confusion-matrix accent and works as a
# neutral "positive" color across hazardous / microchip / 3TG flags.
PLAID_SHARED_POSITIVE <- "#2c5f7c"

# ── HS chapter names ──────────────────────────────────────────────
# 2-digit HS chapter → short human-readable name. Only chapters that
# appear in face-validity plots need entries; fallback shows "Chapter NN".
HS_CHAPTER_NAMES <- c(
  "26" = "Ores & slag",
  "27" = "Mineral fuels",
  "28" = "Inorganic chemicals",
  "29" = "Organic chemicals",
  "31" = "Fertilizers",
  "36" = "Explosives",
  "38" = "Misc. chemicals",
  "41" = "Raw hides & leather",
  "42" = "Leather articles",
  "43" = "Furskins",
  "44" = "Wood",
  "45" = "Cork",
  "46" = "Straw & basketware",
  "61" = "Knitted apparel",
  "62" = "Non-knitted apparel",
  "63" = "Other textiles",
  "64" = "Footwear",
  "65" = "Headgear",
  "66" = "Umbrellas",
  "67" = "Feathers & flowers",
  "71" = "Pearls & precious metals",
  "80" = "Tin",
  "81" = "Other base metals",
  "84" = "Machinery",
  "85" = "Electrical machinery",
  "90" = "Optical & medical",
  "94" = "Furniture & lighting",
  "95" = "Toys & sports"
)

#' Pretty label for an HS chapter: "Short name [NN]".
#' @param chapter  character vector of 2-digit chapter codes
#' @return         character vector of labels
hs_chapter_label <- function(chapter) {
  chapter <- as.character(chapter)
  nm <- HS_CHAPTER_NAMES[chapter]
  ifelse(is.na(nm),
         sprintf("Chapter %s", chapter),
         sprintf("%s [%s]", nm, chapter))
}

#' Stacked-bar chart of category shares by HS chapter group.
#'
#' @param df               data.frame with `chapter_group`, `category`, `share`
#' @param indicator        one of names(PLAID_COLORS); only used when
#'                         `palette = "indicator"`
#' @param category_levels  ordered vector of categories for stacking
#' @param title            optional plot title
#' @param palette          "shared" (default) = neutral slate across all
#'                         chapter plots; "indicator" = per-indicator accent
#' @return                 ggplot object
plot_chapter_shares <- function(df, indicator, category_levels, title = NULL,
                                palette = c("shared", "indicator")) {
  palette <- match.arg(palette)
  stopifnot(all(c("chapter_group", "category", "share") %in% names(df)))

  dt <- as.data.table(df)
  dt[, category := factor(category, levels = category_levels)]

  accent <- if (palette == "shared") {
    PLAID_SHARED_POSITIVE
  } else {
    stopifnot(indicator %in% names(PLAID_COLORS))
    PLAID_COLORS[[indicator]]
  }
  n_cat <- length(category_levels)

  # Palette: lighten/darken the accent across categories
  if (n_cat == 2) {
    fill_palette <- c(PLAID_BG_CREAM, accent)
  } else {
    fill_palette <- colorRampPalette(c(PLAID_BG_CREAM, accent, "#2c1810"))(n_cat)
  }

  ggplot(dt, aes(x = share, y = chapter_group, fill = category)) +
    geom_col(width = 0.75, color = "#ddd3c6", linewidth = 0.2) +
    scale_fill_manual(values = setNames(fill_palette, category_levels)) +
    scale_x_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.02)),
                       limits = c(0, 1)) +
    labs(x = NULL, y = NULL, fill = NULL, title = title) +
    theme_minimal(base_family = "serif", base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0, face = "bold"),
          plot.background = element_rect(fill = "white", color = NA))
}

save_chapter_shares <- function(p, path, width = 6, height = 4) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggsave(path, plot = p, device = cairo_pdf,
         width = width, height = height, dpi = 300)
  message("Wrote: ", path)
}
