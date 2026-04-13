###
# 09 - Rauch Uncertainty Screen Figure
# 260413
###

# Shows that agreement with the Rauch (1999) benchmark declines
# monotonically with ensemble disagreement (uncertainty u_k).

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, stringr, ggplot2)

# PLAID Rauch H0
db = fread("output/database/PLAID_v0.1_rauch_H0.csv.gz", colClasses = list(character = "hs6_code"))
db[, hs6_code := str_pad(hs6_code, 6, pad = "0")]
db[, max_share := pmax(rauch_share_w, rauch_share_r, rauch_share_n, na.rm = TRUE)]
db[, u_k := 1 - max_share]

# Concordance HS92 -> SITC2
conc_zip = "input/concordance/Concordance_H0_to_S2.zip"
conc_csv = unzip(conc_zip, list = TRUE)$Name[1]
conc = fread(cmd = sprintf("unzip -p '%s' '%s'", conc_zip, conc_csv), showProgress = FALSE)
hs_col = grep("HS.*Code", names(conc), value = TRUE)[1]
sitc_col = grep("SITC.*Code", names(conc), value = TRUE)[1]
conc = conc[, .(hs6 = str_pad(as.character(get(hs_col)), 6, pad = "0"),
                sitc4 = str_sub(str_pad(as.character(get(sitc_col)), 5, pad = "0"), 1, 4))]
conc = unique(conc)

# Rauch benchmark (conservative)
rauch = fread("temp/rauch/Rauch_classification_revised.csv", colClasses = list(character = "sitc4"))
rauch[, sitc4 := str_pad(sitc4, 4, pad = "0")]
rauch = rauch[con %in% c("w","r","n"), .(sitc4, rauch_true = con)]

# Map PLAID -> SITC -> Rauch
mapped = merge(db, conc, by.x = "hs6_code", by.y = "hs6", allow.cartesian = TRUE)
mapped = merge(mapped, rauch, by = "sitc4")
mapped[, correct := rauch == rauch_true]

# Agreement by ensemble agreement level
by_share = mapped[, .(n_codes = .N, agreement = mean(correct)), by = max_share][order(max_share)]

message("Agreement by majority share:")
print(by_share)
message(sprintf("\nMean u_k — correct: %.3f, misclassified: %.3f",
  mapped[correct == TRUE, mean(u_k)], mapped[correct == FALSE, mean(u_k)]))

# Figure
p = ggplot(by_share, aes(x = factor(max_share), y = agreement)) +
  geom_col(fill = "#8B6F5C", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", agreement * 100)), vjust = -0.5, size = 3.5) +
  geom_text(aes(label = sprintf("n = %s", format(n_codes, big.mark = ","))),
            y = 0.02, size = 3, color = "white") +
  scale_x_discrete(labels = c("0.5" = "50%\n(2 vs 2)", "0.75" = "75%\n(3 of 4)", "1" = "100%\n(unanimous)")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.05), expand = c(0, 0)) +
  labs(x = "Ensemble majority share", y = "Agreement with Rauch benchmark") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 15)
  )

dir.create("output/analysis/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/analysis/figures/rauch_uncertainty_screen.pdf", p, width = 5.5, height = 3.5)
message("Saved: output/analysis/figures/rauch_uncertainty_screen.pdf")
