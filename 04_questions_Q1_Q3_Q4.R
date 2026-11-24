# =====================================================================
# STAGE 3 — ANALYTICAL QUESTIONS Q1, Q3, Q4  (layer: quantity & quality)
# Input : data/clean/es_clean.rds
# Output: output/figures/Q1_*.png, Q3_*.png, Q4_*.png
#         output/tables/Q*.csv
#         output/results_Q1_Q3_Q4.txt   <- upload this file
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(forcats); library(readr)
})

es <- readRDS("data/clean/es_clean.rds")
for (d in c("output/figures", "output/tables")) dir.create(d, recursive = TRUE, showWarnings = FALSE)
sink("output/results_Q1_Q3_Q4.txt", split = TRUE)

# ---- Shared tools ----------------------------------------------------
theme_project <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "bottom")
col_ur <- c(Urban = "#3B6EA8", Rural = "#C9772B")
src <- "Source: Data ES (Ministère des Sports), data.gouv.fr — own calculations"

# Cramér's V: effect size for chi-square (0 = no association, 1 = perfect)
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
}

# Cochran–Mantel–Haenszel: Rural vs Urban odds of a binary outcome,
# compared WITHIN each stratum (facility type), then pooled.
# OR > 1 = outcome more frequent in rural facilities of the same type.
cmh_rural_urban <- function(data, outcome, strata = "type", label = outcome) {
  d <- data |>
    filter(!is.na(.data[[outcome]]), !is.na(urban_rural), !is.na(.data[[strata]])) |>
    group_by(.data[[strata]]) |>
    filter(n_distinct(urban_rural) == 2, n_distinct(.data[[outcome]]) == 2) |>
    ungroup()
  arr <- table(factor(d$urban_rural, levels = c("Rural", "Urban")),
               factor(d[[outcome]], levels = c(TRUE, FALSE)),
               d[[strata]])
  storage.mode(arr) <- "double"   # avoids integer overflow with n > 100,000
  test <- mantelhaen.test(arr)
  # Crude (unadjusted) OR, to show how much the facility mix distorts things
  crude <- table(factor(d$urban_rural, levels = c("Rural", "Urban")),
                 factor(d[[outcome]], levels = c(TRUE, FALSE)))
  storage.mode(crude) <- "double"
  crude_or <- (crude[1, 1] * crude[2, 2]) / (crude[1, 2] * crude[2, 1])
  y <- d[[outcome]]
  pct_r <- round(100 * mean(y[d$urban_rural == "Rural"]), 1)
  pct_u <- round(100 * mean(y[d$urban_rural == "Urban"]), 1)
  tibble(outcome = label, n = nrow(d), n_strata = dim(arr)[3],
         pct_rural = pct_r, pct_urban = pct_u,
         crude_OR = round(crude_or, 3),
         adjusted_OR = round(unname(test$estimate), 3),
         ci_low = round(test$conf.int[1], 3), ci_high = round(test$conf.int[2], 3),
         p_value = signif(test$p.value, 3))
}

# =====================================================================
# Q1 — Does the MIX of facility types differ between urban and rural areas?
# H0: facility family is independent of urban/rural location
# H1: the distribution of families differs
# =====================================================================
cat("\n================ Q1 ================\n")
q1 <- es |> filter(!is.na(urban_rural))
tab_q1 <- table(q1$family, q1$urban_rural)
chi_q1 <- chisq.test(tab_q1)
v_q1 <- cramers_v(tab_q1)
print(chi_q1)
cat(sprintf("Cramér's V = %.3f  (2 columns -> 0.1 small, 0.3 medium, 0.5 large)\n", v_q1))

shares_q1 <- q1 |>
  count(urban_rural, family) |>
  group_by(urban_rural) |> mutate(pct = 100 * n / sum(n)) |> ungroup() |>
  select(-n) |>
  pivot_wider(names_from = urban_rural, values_from = pct, values_fill = 0) |>
  mutate(diff_pts = Rural - Urban) |>
  arrange(desc(abs(diff_pts)))

# Standardised residuals: |res| > 2 = family clearly over/under-represented
res_q1 <- as.data.frame(as.table(chi_q1$stdres)) |>
  setNames(c("family", "urban_rural", "std_residual")) |>
  filter(urban_rural == "Rural") |> select(-urban_rural)
shares_q1 <- shares_q1 |> left_join(res_q1, by = "family")
write_csv(shares_q1, "output/tables/Q1_family_shares.csv")
print(shares_q1, n = 29)

p_q1 <- shares_q1 |>
  slice_head(n = 15) |>
  mutate(family = fct_reorder(family, diff_pts)) |>
  ggplot(aes(y = family)) +
  geom_segment(aes(x = Urban, xend = Rural, yend = family), colour = "grey75", linewidth = 1) +
  geom_point(aes(x = Urban, colour = "Urban"), size = 3) +
  geom_point(aes(x = Rural, colour = "Rural"), size = 3) +
  scale_colour_manual(values = col_ur, name = NULL) +
  labs(title = "Q1. Urban and rural France do not have the same kind of sports facilities",
       subtitle = sprintf("Share of each facility family in the local stock — 15 largest gaps | Cramér's V = %.2f", v_q1),
       x = "Share of all facilities in the area (%)", y = NULL, caption = src) +
  theme_project
ggsave("output/figures/Q1_family_mix.png", p_q1, width = 10, height = 6.5, dpi = 300)

# =====================================================================
# Q3 — Is the age of facilities' current configuration associated with density?
# H0: construction period is independent of the density level
# H1 (project H1): more rural areas have OLDER configurations
# =====================================================================
cat("\n================ Q3 ================\n")
q3 <- es |>
  filter(!is.na(service_period), !is.na(density)) |>
  mutate(pre_1985 = service_period <= "1975-1984")

tab_q3 <- table(q3$density, q3$service_period)
chi_q3 <- chisq.test(tab_q3)
v_q3 <- cramers_v(tab_q3)
print(chi_q3)
cat(sprintf("Cramér's V = %.3f  (7x7 table -> 0.04 small, 0.12 medium, 0.20 large)\n", v_q3))

# Both variables are ORDINAL: Spearman uses the order, which chi-square ignores.
# density: 1 = large urban centre ... 7 = very dispersed rural
# period : 1 = before 1945 ... 7 = from 2005
# H1 predicts a NEGATIVE rho (more rural -> older)
sp_q3 <- cor.test(as.integer(q3$density), as.integer(q3$service_period),
                  method = "spearman", exact = FALSE)
print(sp_q3)

# Non-parametric alternative to one-way ANOVA (period rank across 7 density levels)
kw_q3 <- kruskal.test(as.integer(service_period) ~ density, data = q3)
eps2_q3 <- unname(kw_q3$statistic) / (nrow(q3) - 1)
print(kw_q3)
cat(sprintf("Epsilon-squared = %.4f (0.01 small, 0.08 medium, 0.26 large)\n", eps2_q3))

# Composition check: same comparison WITHIN facility type (CMH)
cat("\nPre-1985 configuration, rural vs urban, adjusted for facility type:\n")
cmh_q3 <- cmh_rural_urban(q3, "pre_1985", label = "Configured before 1985")
print(cmh_q3, width = Inf)

# Within the 8 largest families, for the figure
top_fam <- q3 |> count(family, sort = TRUE) |> slice_head(n = 8) |> pull(family)
q3_fam <- q3 |>
  filter(family %in% top_fam, !is.na(urban_rural)) |>
  group_by(family, urban_rural) |>
  summarise(pct_pre_1985 = 100 * mean(pre_1985), n = n(), .groups = "drop")
write_csv(q3_fam, "output/tables/Q3_pre1985_by_family.csv")
print(q3_fam, n = Inf)

p_q3a <- q3 |>
  count(density, service_period) |>
  group_by(density) |> mutate(pct = 100 * n / sum(n)) |> ungroup() |>
  ggplot(aes(x = pct, y = fct_rev(density), fill = service_period)) +
  geom_col(width = 0.75) +
  scale_fill_viridis_d(option = "cividis", name = "Period of current configuration") +
  labs(title = "Q3. Age of facilities' current configuration by territorial density",
       subtitle = sprintf("Spearman rho = %.3f | Cramér's V = %.3f", unname(sp_q3$estimate), v_q3),
       x = "Share of facilities (%)", y = NULL, caption = src) +
  theme_project + guides(fill = guide_legend(nrow = 2))
ggsave("output/figures/Q3_period_by_density.png", p_q3a, width = 10, height = 6, dpi = 300)

p_q3b <- q3_fam |>
  ggplot(aes(x = pct_pre_1985, y = fct_reorder(family, pct_pre_1985), colour = urban_rural)) +
  geom_point(size = 3) +
  scale_colour_manual(values = col_ur, name = NULL) +
  labs(title = "Q3 (control). Same facility type, different age?",
       subtitle = sprintf("Share configured before 1985 | CMH-adjusted OR (rural vs urban) = %.2f [%.2f–%.2f]",
                          cmh_q3$adjusted_OR, cmh_q3$ci_low, cmh_q3$ci_high),
       x = "Configured before 1985 (%)", y = NULL, caption = src) +
  theme_project
ggsave("output/figures/Q3_pre1985_within_family.png", p_q3b, width = 10, height = 5.5, dpi = 300)

# =====================================================================
# Q4 — Within the same facility type, are rural facilities less equipped?
# H0: conditional on facility type, the odds of having the amenity are
#     equal in rural and urban areas (common OR = 1)
# H1: rural facilities have lower odds (OR < 1)
# Changing rooms is the most reliable amenity: 0 is an explicit zero in the
# manual, whereas showers/toilets/lighting are checkboxes (FALSE may = unknown).
# =====================================================================
cat("\n================ Q4 ================\n")
amenity_families <- c("Terrain de grands jeux", "Salle multisports", "Court de tennis",
                      "Equipement d'athlétisme", "Salle ou terrain spécialisé",
                      "Salle de combat", "Bassin de natation")
q4 <- es |>
  filter(family %in% amenity_families) |>
  mutate(has_changing_rooms = changing_rooms_nb > 0)

q4_main <- bind_rows(
  cmh_rural_urban(q4, "has_changing_rooms", label = "Changing rooms (reliable)"),
  cmh_rural_urban(q4, "showers",  label = "Showers (checkbox)"),
  cmh_rural_urban(q4, "toilets",  label = "Toilets (checkbox)"),
  cmh_rural_urban(q4 |> filter(nature != "Intérieur"), "lighting", label = "Lighting, outdoor only (checkbox)")
) |> mutate(sample = "All facilities")

# Robustness: restrict to well-documented records (accessibility documented).
# If the ORs barely move, checkbox under-reporting is unlikely to drive the result.
q4_doc <- q4 |> filter(acc_mob_documented)
q4_robust <- bind_rows(
  cmh_rural_urban(q4_doc, "has_changing_rooms", label = "Changing rooms (reliable)"),
  cmh_rural_urban(q4_doc, "showers",  label = "Showers (checkbox)"),
  cmh_rural_urban(q4_doc, "toilets",  label = "Toilets (checkbox)"),
  cmh_rural_urban(q4_doc |> filter(nature != "Intérieur"), "lighting", label = "Lighting, outdoor only (checkbox)")
) |> mutate(sample = "Well-documented records only")

q4_results <- bind_rows(q4_main, q4_robust)
write_csv(q4_results, "output/tables/Q4_amenities_cmh.csv")
print(q4_results, width = Inf)

p_q4 <- q4_results |>
  ggplot(aes(x = adjusted_OR, y = outcome, colour = sample)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.5)) +
  scale_x_log10() +
  scale_colour_manual(values = c("All facilities" = "#3B6EA8", "Well-documented records only" = "#8C8C8C"),
                      name = NULL) +
  labs(title = "Q4. Rural vs urban facilities of the same type: who is better equipped?",
       subtitle = "Odds ratio (rural vs urban), adjusted for facility type (Cochran–Mantel–Haenszel)\nOR < 1 = rural facilities less often equipped",
       x = "Adjusted odds ratio (log scale, 95% CI)", y = NULL, caption = src) +
  theme_project
ggsave("output/figures/Q4_amenities_forest.png", p_q4, width = 10, height = 5, dpi = 300)

sink()
cat("Done. Upload output/results_Q1_Q3_Q4.txt (figures are in output/figures/)\n")
