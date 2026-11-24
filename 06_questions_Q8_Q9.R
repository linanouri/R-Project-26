# =====================================================================
# STAGE 3c — Q8 (who gets documented?) and Q9 (simple linear regression)
# Input : data/clean/es_clean.rds
# Output: output/figures/Q8_*.png, Q9_*.png
#         output/tables/Q8_*.csv, Q9_*.csv
#         data/clean/dep_level.rds      (department dataset, reused in Q10)
#         output/results_Q8_Q9.txt      <- upload this file
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(forcats); library(readr)
})

es <- readRDS("data/clean/es_clean.rds")
for (d in c("output/figures", "output/tables")) dir.create(d, recursive = TRUE, showWarnings = FALSE)
sink("output/results_Q8_Q9.txt", split = TRUE)

theme_project <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "bottom")
src <- "Source: Data ES (Ministère des Sports), data.gouv.fr — own calculations"

dens_levels <- c("Grands centres urbains", "Centres urbains intermédiaires",
                 "Ceintures urbaines", "Petites villes", "Bourgs ruraux",
                 "Rural à habitat dispersé", "Rural à habitat très dispersé")

# =====================================================================
# Q8 — Is accessibility DOCUMENTATION itself unequal?
# Logistic regression: P(documented) ~ density + QPV + owner + period + family
# H0: each OR = 1 (documentation unrelated to the characteristic)
# Undated facilities are kept as their own category: dropping them would
# remove exactly the poorly documented records we want to study.
# =====================================================================
cat("\n================ Q8 ================\n")
q8 <- es |>
  filter(!is.na(density), !is.na(owner_group)) |>
  mutate(
    density = factor(as.character(density), levels = dens_levels),   # unordered for glm
    qpv_zone = factor(qpv_zone, levels = c("Elsewhere", "Within 200 m", "Inside QPV")),
    owner_group = relevel(factor(owner_group), ref = "Commune"),
    period = factor(if_else(is.na(service_period), "Unknown", as.character(service_period)),
                    levels = c("1985-1994", "Avant 1945", "1945-1964", "1965-1974",
                               "1975-1984", "1995-2004", "A partir de 2005", "Unknown")),
    family = relevel(factor(family), ref = "Terrain de grands jeux")
  )
cat("n =", nrow(q8), "| documented:", round(100 * mean(q8$acc_mob_documented), 1), "%\n")

m_q8 <- glm(acc_mob_documented ~ density + qpv_zone + owner_group + period + family,
            family = binomial, data = q8)
co <- summary(m_q8)$coefficients
keep <- grep("^(density|qpv_zone|owner_group|period)", rownames(co))
q8_or <- tibble(term = rownames(co)[keep],
                OR = exp(co[keep, 1]),
                ci_low = exp(co[keep, 1] - 1.96 * co[keep, 2]),
                ci_high = exp(co[keep, 1] + 1.96 * co[keep, 2]),
                p_value = signif(co[keep, 4], 3)) |>
  mutate(block = case_when(grepl("^density", term) ~ "Density (ref: large urban centres)",
                           grepl("^qpv", term) ~ "QPV (ref: elsewhere)",
                           grepl("^owner", term) ~ "Owner (ref: commune)",
                           TRUE ~ "Period (ref: 1985-1994)"),
         label = sub("^(density|qpv_zone|owner_group|period)", "", term))
print(q8_or |> mutate(across(c(OR, ci_low, ci_high), ~ round(.x, 3))) |> select(-term), n = Inf)
cat(sprintf("AIC = %.0f | McFadden pseudo-R2 = %.3f\n", AIC(m_q8),
            1 - m_q8$deviance / m_q8$null.deviance))
write_csv(q8_or, "output/tables/Q8_documentation_logit.csv")

p_q8 <- q8_or |>
  ggplot(aes(x = OR, y = fct_rev(fct_inorder(label)))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), colour = "#3B6EA8", size = 0.3) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y") +
  scale_x_log10() +
  labs(title = "Q8. Which facilities have their accessibility documented?",
       subtitle = "Logistic regression, odds ratios with 95% CI (also adjusted for facility family)\nOR > 1 = more often documented",
       x = "Odds ratio (log scale)", y = NULL, caption = src) +
  theme_project + theme(strip.text.y = element_text(angle = 0, hjust = 0))
ggsave("output/figures/Q8_documentation_forest.png", p_q8, width = 11, height = 8, dpi = 300)

# =====================================================================
# Q9 — How much of the variation in departments' APPARENT accessibility
#      is explained by documentation alone?  (simple linear regression)
# Apparent rate = what a naive reading gives (undocumented counted as "no").
# H0: slope = 0     H1: slope > 0
# =====================================================================
cat("\n================ Q9 ================\n")
dep <- es |>
  group_by(dep_code, dep_nom) |>
  summarise(n_facilities = n(),
            pct_documented   = 100 * mean(acc_mob_documented),
            pct_apparent_acc = 100 * mean(acc_mob_aire %in% TRUE),
            pct_acc_if_doc   = 100 * mean(acc_mob_aire, na.rm = TRUE),
            pct_rural        = 100 * mean(urban_rural == "Rural", na.rm = TRUE),
            pct_pre_1985     = 100 * mean(service_period <= "1975-1984", na.rm = TRUE),
            pct_commune      = 100 * mean(owner_group == "Commune", na.rm = TRUE),
            pct_changing_rooms = 100 * mean(changing_rooms_nb > 0, na.rm = TRUE),
            .groups = "drop")
saveRDS(dep, "data/clean/dep_level.rds")
cat("Departments:", nrow(dep), "\n")
print(summary(dep |> select(pct_documented, pct_apparent_acc, pct_acc_if_doc)))

m_q9 <- lm(pct_apparent_acc ~ pct_documented, data = dep)
print(summary(m_q9))
print(confint(m_q9))

# ---- Assumption checks ----
cat("\nNormality of residuals (Shapiro-Wilk):\n"); print(shapiro.test(resid(m_q9)))
# Breusch-Pagan (Koenker version) by hand: n * R2 of squared residuals on X
aux <- lm(resid(m_q9)^2 ~ pct_documented, data = dep)
bp_stat <- nrow(dep) * summary(aux)$r.squared
cat(sprintf("Breusch-Pagan: LM = %.3f, df = 1, p = %.4f (H0: constant variance)\n",
            bp_stat, pchisq(bp_stat, df = 1, lower.tail = FALSE)))
dep <- dep |> mutate(fitted = fitted(m_q9), residual = resid(m_q9),
                     cooks_d = cooks.distance(m_q9))
cat("\nInfluential departments (Cook's D > 4/n):\n")
print(dep |> filter(cooks_d > 4 / nrow(dep)) |>
        select(dep_code, dep_nom, pct_documented, pct_apparent_acc, cooks_d) |>
        arrange(desc(cooks_d)))

# ---- Exact variance decomposition ----
# apparent/100 = (documented/100) * (accessible-if-documented/100), so on the
# log scale: log(apparent) = log(documented) + log(acc_if_doc) - log(100).
# Var(log apparent) = Var(log doc) + Var(log acc) + 2 Cov(...)  -- exactly.
l_app <- log(dep$pct_apparent_acc); l_doc <- log(dep$pct_documented); l_acc <- log(dep$pct_acc_if_doc)
decomp <- tibble(
  component = c("Documentation", "Accessibility among documented", "Covariance term"),
  variance  = c(var(l_doc), var(l_acc), 2 * cov(l_doc, l_acc))
) |> mutate(share_pct = round(100 * variance / var(l_app), 1))
cat("\nVariance decomposition of log(apparent accessibility) across departments:\n")
print(decomp)
cat(sprintf("Check: sum of components = %.6f vs Var(log apparent) = %.6f\n",
            sum(decomp$variance), var(l_app)))
write_csv(decomp, "output/tables/Q9_variance_decomposition.csv")
write_csv(dep, "output/tables/Q9_department_table.csv")

# ---- Figures ----
lab_deps <- dep |> arrange(desc(abs(residual))) |> slice_head(n = 5) |>
  bind_rows(dep |> arrange(pct_documented) |> slice_head(n = 2),
            dep |> arrange(desc(pct_documented)) |> slice_head(n = 2)) |>
  distinct(dep_code, .keep_all = TRUE)

p_q9 <- ggplot(dep, aes(x = pct_documented, y = pct_apparent_acc)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#B2182B", fill = "#F4A582") +
  geom_point(colour = "#3B6EA8", alpha = 0.8) +
  geom_text(data = lab_deps, aes(label = dep_nom), size = 3, vjust = -0.8, colour = "grey25") +
  labs(title = "Q9. Departments' apparent accessibility mostly reflects how well they are documented",
       subtitle = sprintf("Simple linear regression, 101 departments | R² = %.2f | slope = %.2f",
                          summary(m_q9)$r.squared, coef(m_q9)[2]),
       x = "Facilities with accessibility documented (%)",
       y = "Facilities with an accessible playing area,\nnaive reading (%)", caption = src) +
  theme_project
ggsave("output/figures/Q9_regression.png", p_q9, width = 10, height = 6.5, dpi = 300)

p_q9_res <- ggplot(dep, aes(x = fitted, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(colour = "#3B6EA8") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#B2182B") +
  labs(title = "Q9 diagnostics. Residuals vs fitted values",
       subtitle = "No pattern = linearity and constant variance plausible",
       x = "Fitted value (%)", y = "Residual (percentage points)") +
  theme_project
ggsave("output/figures/Q9_residuals.png", p_q9_res, width = 8, height = 5, dpi = 300)

p_q9_qq <- ggplot(dep, aes(sample = residual)) +
  stat_qq(colour = "#3B6EA8") + stat_qq_line(colour = "#B2182B") +
  labs(title = "Q9 diagnostics. Normal Q-Q plot of residuals",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_project
ggsave("output/figures/Q9_qqplot.png", p_q9_qq, width = 6, height = 5, dpi = 300)

sink()
cat("Done. Upload output/results_Q8_Q9.txt and the Q8/Q9 figures.\n")
