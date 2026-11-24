# =====================================================================
# STAGE 3d — Q2 (supply per inhabitant + map), Quantity–Quality Gap indicator,
#            Q10 (multiple linear regression, 96 metropolitan departments)
# Inputs : data/clean/es_clean.rds, data/clean/dep_level.rds (from script 06)
#          data/raw/insee/insee_population_departements_2023.csv
#          data/raw/insee/DS_FILOSOFI_SAGE_LOG_TP_NIVVIE_2023_data.csv
#          data/raw/geo/departements-version-simplifiee.geojson (auto-downloaded)
# Output : output/figures/Q2_*.png, Q10_*.png, QQG_*.png
#          output/tables/Q2_*.csv, Q10_*.csv, QQG_*.csv
#          data/clean/dep_final.rds
#          output/results_Q2_Q10.txt   <- upload this file
#
# Supplementary sources
#  - Population: INSEE, Populations de référence 2023 (in force 01/01/2026),
#    population municipale; 100 departments (Mayotte not published).
#  - Income: INSEE Filosofi 2023, median standard of living (MED_SL) and
#    poverty rate (PR_MD60); 96 metropolitan departments + La Réunion.
#  - Boundaries: IGN Admin Express COG, simplified GeoJSON (france-geojson,
#    Licence Ouverte); 96 metropolitan departments.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(forcats)
  library(readr); library(sf)
})

for (d in c("output/figures", "output/tables", "data/raw/geo")) dir.create(d, recursive = TRUE, showWarnings = FALSE)
sink("output/results_Q2_Q10.txt", split = TRUE)

theme_project <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "bottom")
theme_map <- theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        legend.position = "right")
src <- "Sources: Data ES (Ministère des Sports); INSEE RP 2023 & Filosofi 2023; IGN Admin Express — own calculations"

es  <- readRDS("data/clean/es_clean.rds")
dep_base <- readRDS("data/clean/dep_level.rds")

# ---------------------------------------------------------------------
# 1. SUPPLEMENTARY DATA + COMPATIBILITY CHECKS
# ---------------------------------------------------------------------
pop <- read_csv("data/raw/insee/insee_population_departements_2023.csv",
                col_types = cols(dep_code = col_character(), dep_nom_insee = col_character(),
                                 population_municipale_2023 = col_double()))
stopifnot(nrow(pop) == 100, sum(pop$population_municipale_2023) == 68094280)

filo <- read_delim("data/raw/insee/DS_FILOSOFI_SAGE_LOG_TP_NIVVIE_2023_data.csv",
                   delim = ";", col_types = cols(.default = col_character())) |>
  # GEO codes overlap between regions and departments ("53" = Bretagne AND Mayenne):
  # keeping GEO_OBJECT == "DEP" is essential. Totals only (all ages, sexes, tenures).
  filter(GEO_OBJECT == "DEP", AGE_GP == "_T", SEX == "_T", TSH == "_T") |>
  transmute(dep_code = GEO, measure = FILOSOFI_MEASURE, value = as.numeric(OBS_VALUE)) |>
  pivot_wider(names_from = measure, values_from = value) |>
  rename(median_living_std = MED_SL, poverty_rate = PR_MD60)
cat("Filosofi departments:", nrow(filo), "\n")

geo_file <- "data/raw/geo/departements-version-simplifiee.geojson"
if (!file.exists(geo_file)) {
  download.file("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson",
                geo_file, mode = "wb")
}
geo <- st_read(geo_file, quiet = TRUE) |> rename(dep_code = code)
geo$area_km2 <- as.numeric(st_area(geo)) / 1e6
cat("Boundary polygons:", nrow(geo), "\n")

# Facility counts and composition-adjusted equipment index
amenity_families <- c("Terrain de grands jeux", "Salle multisports", "Court de tennis",
                      "Equipement d'athlétisme", "Salle ou terrain spécialisé",
                      "Salle de combat", "Bassin de natation")
counts <- es |>
  group_by(dep_code) |>
  summarise(n_sites = n_distinct(inst_id),
            n_core = sum(family != "Divers équipements Sports de nature"),
            .groups = "drop")

# Indirect standardisation: expected = sum of NATIONAL rates of each facility's type.
# Index = 100 * observed / expected  (100 = national level for that facility mix)
amen <- es |> filter(family %in% amenity_families, !is.na(changing_rooms_nb)) |>
  mutate(has_cr = changing_rooms_nb > 0)
nat_rates <- amen |> group_by(type) |> summarise(type_rate = mean(has_cr), .groups = "drop")
quality <- amen |> left_join(nat_rates, by = "type") |>
  group_by(dep_code) |>
  summarise(n_amenity = n(), observed = sum(has_cr), expected = sum(type_rate),
            quality_index = 100 * observed / expected, .groups = "drop")

dep <- dep_base |>
  left_join(counts, by = "dep_code") |>
  left_join(quality, by = "dep_code") |>
  left_join(pop, by = "dep_code") |>
  left_join(filo, by = "dep_code") |>
  left_join(st_drop_geometry(geo) |> select(dep_code, area_km2), by = "dep_code") |>
  mutate(metro = !dep_code %in% c("971", "972", "973", "974", "976"),
         facilities_per_10k = 10000 * n_facilities / population_municipale_2023,
         core_per_10k       = 10000 * n_core / population_municipale_2023,
         sites_per_10k      = 10000 * n_sites / population_municipale_2023,
         density            = population_municipale_2023 / area_km2)

cat("\nMerge checks (codes in Data ES without a match):\n")
cat("  no population:", paste(dep$dep_code[is.na(dep$population_municipale_2023)], collapse = ", "), "\n")
cat("  no income    :", paste(dep$dep_code[is.na(dep$median_living_std)], collapse = ", "), "\n")
cat("  no boundary  :", paste(dep$dep_code[is.na(dep$area_km2)], collapse = ", "), "\n")
saveRDS(dep, "data/clean/dep_final.rds")

# =====================================================================
# Q2 — Where are there the most facilities per inhabitant, and is high
#      supply a low-density phenomenon?   (100 departments with population)
# H0: rho = 0     H1: rho < 0 (lower density -> more facilities per inhabitant)
# =====================================================================
cat("\n================ Q2 ================\n")
q2 <- dep |> filter(!is.na(facilities_per_10k))
print(summary(q2 |> select(facilities_per_10k, sites_per_10k, core_per_10k)))

cat("\nTop 10 / bottom 10 departments (facilities per 10,000 inhabitants):\n")
print(q2 |> arrange(desc(facilities_per_10k)) |> select(dep_code, dep_nom, facilities_per_10k, sites_per_10k) |> slice_head(n = 10))
print(q2 |> arrange(facilities_per_10k) |> select(dep_code, dep_nom, facilities_per_10k, sites_per_10k) |> slice_head(n = 10))
write_csv(q2 |> select(dep_code, dep_nom, population_municipale_2023, n_facilities, n_sites,
                       facilities_per_10k, sites_per_10k, core_per_10k, density, quality_index),
          "output/tables/Q2_supply_by_department.csv")

q2m <- q2 |> filter(metro)
cat("\nSpearman: facilities per 10k vs population density (96 metropolitan):\n")
sp_density <- cor.test(q2m$facilities_per_10k, q2m$density, method = "spearman", exact = FALSE)
print(sp_density)
cat("Same, excluding nature sites (core facilities):\n")
print(cor.test(q2m$core_per_10k, q2m$density, method = "spearman", exact = FALSE))
cat("\nSpearman: facilities per 10k vs equipment quality index (quantity vs quality):\n")
sp_qq <- cor.test(q2m$facilities_per_10k, q2m$quality_index, method = "spearman", exact = FALSE)
print(sp_qq)

map_data <- geo |> left_join(st_drop_geometry(dep) |> select(-area_km2), by = "dep_code")

p_q2_map <- ggplot(map_data) +
  geom_sf(aes(fill = facilities_per_10k), colour = "white", linewidth = 0.15) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "Facilities\nper 10,000\ninhabitants") +
  labs(title = "Q2. Sports facilities per 10,000 inhabitants",
       subtitle = "Metropolitan France, by department", caption = src) +
  theme_map
ggsave("output/figures/Q2_map_supply.png", p_q2_map, width = 8, height = 8, dpi = 300)

p_q2_sc <- ggplot(q2m, aes(x = density, y = facilities_per_10k)) +
  geom_point(colour = "#3B6EA8", alpha = 0.8) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#B2182B") +
  scale_x_log10() +
  labs(title = "Q2. The emptier the territory, the more facilities per inhabitant?",
       subtitle = sprintf("96 metropolitan departments | Spearman rho = %.2f", unname(sp_density$estimate)),
       x = "Population density (inhabitants per km², log scale)",
       y = "Facilities per 10,000 inhabitants", caption = src) +
  theme_project
ggsave("output/figures/Q2_supply_vs_density.png", p_q2_sc, width = 9, height = 6, dpi = 300)

# =====================================================================
# QUANTITY–QUALITY GAP (QQG) indicator — 96 metropolitan departments
#   QQG = z(log facilities per 10k) - z(quality index)
#   > 0 : more supply than quality ("many but bare")
#   < 0 : fewer facilities, better equipped than expected for their mix
# Equal weights: no theoretical reason to favour one dimension; z-scores put
# both on the same scale. Log of supply reduces the influence of extremes.
# =====================================================================
cat("\n================ Quantity–Quality Gap ================\n")
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
qqg <- dep |>
  filter(metro, !is.na(quality_index), !is.na(facilities_per_10k)) |>
  mutate(z_supply = z(log(facilities_per_10k)), z_quality = z(quality_index),
         QQG = z_supply - z_quality)
print(summary(qqg$QQG))
cat("\nLargest positive gaps (many but bare):\n")
print(qqg |> arrange(desc(QQG)) |> select(dep_nom, facilities_per_10k, quality_index, QQG) |> slice_head(n = 8))
cat("\nLargest negative gaps (few but well equipped):\n")
print(qqg |> arrange(QQG) |> select(dep_nom, facilities_per_10k, quality_index, QQG) |> slice_head(n = 8))
cat("\nSpearman: QQG vs density:\n")
print(cor.test(qqg$QQG, qqg$density, method = "spearman", exact = FALSE))
write_csv(qqg |> select(dep_code, dep_nom, facilities_per_10k, quality_index, z_supply, z_quality, QQG, density),
          "output/tables/QQG_indicator.csv")

lim <- max(abs(qqg$QQG))
p_qqg <- geo |> left_join(qqg |> select(dep_code, QQG), by = "dep_code") |>
  ggplot() +
  geom_sf(aes(fill = QQG), colour = "white", linewidth = 0.15) +
  scale_fill_gradient2(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B", midpoint = 0,
                       limits = c(-lim, lim), name = "QQG") +
  labs(title = "Quantity–Quality Gap: where supply outpaces equipment",
       subtitle = "Red = many facilities but poorly equipped for their type | Blue = fewer but better equipped",
       caption = src) +
  theme_map
ggsave("output/figures/QQG_map.png", p_qqg, width = 8, height = 8, dpi = 300)

# =====================================================================
# Q10 — MULTIPLE LINEAR REGRESSION, 96 metropolitan departments
#   Model A (H4): documentation rate (%)
#   Model B     : equipment quality index (100 = national level for the mix)
#   Predictors  : log facilities per 10k, log density, median standard of
#                 living (k€), share of communally owned facilities (%)
#   Poverty rate is excluded: near-mirror of income (multicollinearity).
# =====================================================================
cat("\n================ Q10 ================\n")
q10 <- dep |>
  filter(metro) |>
  transmute(dep_code, dep_nom, pct_documented, quality_index,
            log_supply = log(facilities_per_10k), log_density = log(density),
            income_k = median_living_std / 1000, pct_commune) |>
  na.omit()
cat("Departments in the models:", nrow(q10), "\n")
cat("\nCorrelation matrix of predictors:\n")
print(round(cor(q10 |> select(log_supply, log_density, income_k, pct_commune)), 2))

predictors <- c("log_supply", "log_density", "income_k", "pct_commune")
vif_manual <- function(df, vars) {
  sapply(vars, function(v) 1 / (1 - summary(lm(reformulate(setdiff(vars, v), response = v), data = df))$r.squared))
}
cat("\nVIF (> 5 = problematic collinearity):\n"); print(round(vif_manual(q10, predictors), 2))

boot_ci <- function(formula, data, B = 2000) {
  set.seed(42)
  bs <- replicate(B, coef(lm(formula, data = data[sample(nrow(data), replace = TRUE), ])))
  t(apply(bs, 1, quantile, c(0.025, 0.975)))
}

fit_and_check <- function(dv, label) {
  cat("\n------", label, "------\n")
  f <- reformulate(predictors, response = dv)
  m <- lm(f, data = q10)
  print(summary(m))
  # Standardised betas: change in DV (in SD) per 1 SD of the predictor
  q10_std <- q10 |> mutate(across(all_of(c(dv, predictors)), ~ as.numeric(scale(.x))))
  std_beta <- coef(lm(f, data = q10_std))[-1]
  ci  <- confint(m)
  bci <- boot_ci(f, q10)
  tab <- tibble(model = label, term = names(coef(m)), estimate = coef(m),
                ols_low = ci[, 1], ols_high = ci[, 2],
                boot_low = bci[, 1], boot_high = bci[, 2],
                p_value = summary(m)$coefficients[, 4],
                std_beta = c(NA, std_beta))
  print(tab |> mutate(across(where(is.numeric), ~ signif(.x, 3))), width = Inf)
  cat("\nShapiro-Wilk on residuals:\n"); print(shapiro.test(resid(m)))
  aux <- lm(reformulate(predictors, response = "r2"), data = mutate(q10, r2 = resid(m)^2))
  bp <- nrow(q10) * summary(aux)$r.squared
  cat(sprintf("Breusch-Pagan: LM = %.2f, df = %d, p = %.4f\n", bp, length(predictors),
              pchisq(bp, df = length(predictors), lower.tail = FALSE)))
  cd <- cooks.distance(m)
  cat("Influential departments (Cook's D > 4/n):",
      paste(q10$dep_nom[cd > 4 / nrow(q10)], collapse = ", "), "\n")
  list(model = m, table = tab,
       diag = tibble(model = label, fitted = fitted(m), residual = resid(m)))
}

res_A <- fit_and_check("pct_documented", "Model A: documentation rate")
res_B <- fit_and_check("quality_index",  "Model B: equipment quality index")
q10_tab <- bind_rows(res_A$table, res_B$table)
write_csv(q10_tab, "output/tables/Q10_regressions.csv")

term_labels <- c(log_supply = "Facilities per 10k (log)", log_density = "Population density (log)",
                 income_k = "Median standard of living (k€)", pct_commune = "Share owned by communes (%)")
p_q10 <- q10_tab |>
  filter(term != "(Intercept)") |>
  mutate(term = term_labels[term]) |>
  ggplot(aes(x = std_beta, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_col(fill = "#3B6EA8", width = 0.6) +
  facet_wrap(~ model) +
  labs(title = "Q10. What explains departments' documentation and equipment levels?",
       subtitle = "Multiple linear regression, 96 metropolitan departments — standardised coefficients",
       x = "Standardised beta (SD change in outcome per 1 SD of predictor)", y = NULL, caption = src) +
  theme_project
ggsave("output/figures/Q10_std_betas.png", p_q10, width = 11, height = 5, dpi = 300)

p_q10_res <- bind_rows(res_A$diag, res_B$diag) |>
  ggplot(aes(x = fitted, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(colour = "#3B6EA8") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#B2182B") +
  facet_wrap(~ model, scales = "free") +
  labs(title = "Q10 diagnostics. Residuals vs fitted values", x = "Fitted value", y = "Residual") +
  theme_project
ggsave("output/figures/Q10_residuals.png", p_q10_res, width = 11, height = 5, dpi = 300)

sink()
cat("Done. Upload output/results_Q2_Q10.txt and the Q2 / QQG / Q10 figures.\n")
