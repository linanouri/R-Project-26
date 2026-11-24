# =====================================================================
# STAGE 3b — Q3 ROBUSTNESS + Q5 (QPV), Q6 (ownership), Q7 (2005 law)
# Input : data/clean/es_clean.rds
# Output: output/figures/Q5_*.png, Q6_*.png, Q7_*.png
#         output/tables/Q*.csv
#         output/results_Q5_Q6_Q7.txt   <- upload this file
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(forcats); library(readr)
})

es <- readRDS("data/clean/es_clean.rds")
for (d in c("output/figures", "output/tables")) dir.create(d, recursive = TRUE, showWarnings = FALSE)
sink("output/results_Q5_Q6_Q7.txt", split = TRUE)

theme_project <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "bottom")
src <- "Source: Data ES (Ministère des Sports), data.gouv.fr — own calculations"

cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
}

# Generic CMH comparison: group a vs group b, within strata (facility type).
# OR > 1 = outcome more frequent in group a than in group b, same type.
cmh_compare <- function(data, outcome, group, a, b, strata = "type", label = outcome) {
  d <- data |>
    filter(as.character(.data[[group]]) %in% c(a, b),
           !is.na(.data[[outcome]]), !is.na(.data[[strata]])) |>
    mutate(.g = factor(as.character(.data[[group]]), levels = c(a, b))) |>
    group_by(.data[[strata]]) |>
    filter(n_distinct(.g) == 2, n_distinct(.data[[outcome]]) == 2) |>
    ungroup()
  if (n_distinct(d[[strata]]) < 2) return(NULL)
  arr <- table(d$.g, factor(d[[outcome]], levels = c(TRUE, FALSE)), d[[strata]])
  storage.mode(arr) <- "double"
  test <- mantelhaen.test(arr)
  crude <- table(d$.g, factor(d[[outcome]], levels = c(TRUE, FALSE)))
  storage.mode(crude) <- "double"
  y <- d[[outcome]]
  tibble(outcome = label, group_a = a, group_b = b, n = nrow(d), n_strata = dim(arr)[3],
         pct_a = round(100 * mean(y[d$.g == a]), 1),
         pct_b = round(100 * mean(y[d$.g == b]), 1),
         crude_OR = round((crude[1, 1] * crude[2, 2]) / (crude[1, 2] * crude[2, 1]), 3),
         adjusted_OR = round(unname(test$estimate), 3),
         ci_low = round(test$conf.int[1], 3), ci_high = round(test$conf.int[2], 3),
         p_value = signif(test$p.value, 3))
}

amenity_families <- c("Terrain de grands jeux", "Salle multisports", "Court de tennis",
                      "Equipement d'athlétisme", "Salle ou terrain spécialisé",
                      "Salle de combat", "Bassin de natation")

es <- es |>
  mutate(pre_1985 = service_period <= "1975-1984",
         has_changing_rooms = changing_rooms_nb > 0,
         lighting_outdoor = if_else(nature != "Intérieur", lighting, NA))

# =====================================================================
# Q3 ROBUSTNESS — could undated facilities make the rural stock LOOK younger?
# =====================================================================
cat("\n================ Q3 robustness ================\n")
cat("Share of facilities with no construction period:\n")
print(es |> filter(!is.na(urban_rural)) |> group_by(urban_rural) |>
        summarise(n = n(), pct_missing_period = round(100 * mean(is.na(service_period)), 1)))

es <- es |> mutate(missing_period = is.na(service_period),
                   pre_1985_worst = if_else(is.na(service_period), TRUE, pre_1985))

q3r <- bind_rows(
  cmh_compare(es, "missing_period", "urban_rural", "Rural", "Urban",
              label = "Period missing (rural vs urban)"),
  cmh_compare(es, "pre_1985", "urban_rural", "Rural", "Urban",
              label = "Pre-1985, observed data"),
  cmh_compare(es, "pre_1985_worst", "urban_rural", "Rural", "Urban",
              label = "Pre-1985, worst case (all undated = old)")
)
print(q3r, width = Inf)
write_csv(q3r, "output/tables/Q3_robustness.csv")

# =====================================================================
# Q5 — Do facilities in / near priority neighbourhoods (QPV) differ from
#      comparable URBAN facilities of the same type? (project H3)
# H0: common OR = 1 (no difference within type)   H1: OR != 1
# Restricted to urban areas: QPVs are urban, so comparing with rural
# facilities would confound "QPV" with "urban".
# =====================================================================
cat("\n================ Q5 ================\n")
q5_data <- es |> filter(urban_rural == "Urban")
cat("Urban facilities by QPV zone:\n"); print(table(q5_data$qpv_zone))

q5_outcomes <- list(
  list(var = "pre_1985",           lab = "Configured before 1985", fam = NULL),
  list(var = "has_changing_rooms", lab = "Changing rooms",         fam = amenity_families),
  list(var = "showers",            lab = "Showers",                fam = amenity_families),
  list(var = "toilets",            lab = "Toilets",                fam = amenity_families),
  list(var = "lighting_outdoor",   lab = "Lighting (outdoor)",     fam = amenity_families)
)

q5_results <- bind_rows(lapply(q5_outcomes, function(o) {
  dat <- if (is.null(o$fam)) q5_data else filter(q5_data, family %in% o$fam)
  bind_rows(
    cmh_compare(dat, o$var, "qpv_zone", "Inside QPV",   "Elsewhere", label = o$lab),
    cmh_compare(dat, o$var, "qpv_zone", "Within 200 m", "Elsewhere", label = o$lab)
  )
}))
write_csv(q5_results, "output/tables/Q5_qpv_cmh.csv")
print(q5_results, width = Inf)

p_q5 <- q5_results |>
  mutate(group_a = factor(group_a, levels = c("Within 200 m", "Inside QPV"))) |>
  ggplot(aes(x = adjusted_OR, y = fct_rev(factor(outcome, levels = unique(q5_results$outcome))),
             colour = group_a)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(width = 0.5)) +
  scale_x_log10() +
  scale_colour_manual(values = c("Inside QPV" = "#B2182B", "Within 200 m" = "#EF8A62"), name = NULL) +
  labs(title = "Q5. Facilities in priority neighbourhoods vs other urban facilities of the same type",
       subtitle = "Odds ratio vs urban facilities elsewhere, adjusted for facility type (CMH)\nOR > 1 = more frequent in / near QPV",
       x = "Adjusted odds ratio (log scale, 95% CI)", y = NULL, caption = src) +
  theme_project
ggsave("output/figures/Q5_qpv_forest.png", p_q5, width = 10, height = 5.5, dpi = 300)

# =====================================================================
# Q6 — Does the type of OWNER relate to facility age and equipment?
# H0: owner group independent of outcome (overall: chi-square;
#     within type: common OR = 1 vs communes)
# =====================================================================
cat("\n================ Q6 ================\n")
q6 <- es |> filter(!is.na(owner_group))

tab_q6_age <- table(q6$owner_group, q6$pre_1985)
print(chisq.test(tab_q6_age))
cat(sprintf("Cramér's V (owner x pre-1985) = %.3f\n", cramers_v(tab_q6_age)))

q6_amen <- q6 |> filter(family %in% amenity_families)
tab_q6_cr <- table(q6_amen$owner_group, q6_amen$has_changing_rooms)
print(chisq.test(tab_q6_cr))
cat(sprintf("Cramér's V (owner x changing rooms) = %.3f\n", cramers_v(tab_q6_cr)))

cat("\nDescriptive profile by owner group:\n")
q6_profile <- q6 |>
  group_by(owner_group) |>
  summarise(n = n(),
            pct_pre_1985 = round(100 * mean(pre_1985, na.rm = TRUE), 1),
            pct_rural = round(100 * mean(urban_rural == "Rural", na.rm = TRUE), 1),
            pct_free_access = round(100 * mean(free_access), 1),
            .groups = "drop") |>
  arrange(desc(n))
print(q6_profile)
write_csv(q6_profile, "output/tables/Q6_owner_profile.csv")

other_owners <- setdiff(unique(q6$owner_group), "Commune")
q6_results <- bind_rows(lapply(other_owners, function(g) bind_rows(
  cmh_compare(q6, "pre_1985", "owner_group", g, "Commune", label = "Configured before 1985"),
  cmh_compare(q6_amen, "has_changing_rooms", "owner_group", g, "Commune", label = "Changing rooms")
)))
write_csv(q6_results, "output/tables/Q6_owner_cmh.csv")
print(q6_results, width = Inf)

p_q6 <- q6_results |>
  ggplot(aes(x = adjusted_OR, y = fct_reorder(group_a, adjusted_OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), colour = "#3B6EA8") +
  facet_wrap(~ outcome) +
  scale_x_log10() +
  labs(title = "Q6. Who owns the facility: does it matter for age and equipment?",
       subtitle = "Odds ratio vs facilities owned by communes, adjusted for facility type (CMH)",
       x = "Adjusted odds ratio (log scale, 95% CI)", y = NULL, caption = src) +
  theme_project
ggsave("output/figures/Q6_owner_forest.png", p_q6, width = 10, height = 5, dpi = 300)

# =====================================================================
# Q7 — Are facilities configured since the 2005 disability law more often
#      accessible? (project H2) — documented facilities only
# H0: OR = 1        H1: OR > 1
# Caution: documentation itself may vary by period (selection bias).
# =====================================================================
cat("\n================ Q7 ================\n")
q7 <- es |> filter(acc_mob_documented, !is.na(post_2005), !is.na(acc_mob_aire))

# (a) Crude 2x2
tab_q7 <- table(post_2005 = q7$post_2005, accessible = q7$acc_mob_aire)
print(tab_q7)
print(chisq.test(tab_q7))
storage.mode(tab_q7) <- "double"
or_q7 <- (tab_q7["TRUE", "TRUE"] * tab_q7["FALSE", "FALSE"]) /
         (tab_q7["TRUE", "FALSE"] * tab_q7["FALSE", "TRUE"])
cat(sprintf("Crude OR (post-2005 vs before) = %.3f\n", or_q7))

# (b) Within facility type
q7_cmh <- cmh_compare(q7 |> mutate(period_grp = if_else(post_2005, "From 2005", "Before 2005")),
                      "acc_mob_aire", "period_grp", "From 2005", "Before 2005",
                      label = "Accessible playing area")
print(q7_cmh, width = Inf)

# (c) Logistic regression with controls (family, rurality, owner)
q7_model_data <- q7 |>
  filter(!is.na(urban_rural), !is.na(owner_group)) |>
  mutate(family = relevel(factor(family), ref = "Terrain de grands jeux"),
         owner_group = relevel(factor(owner_group), ref = "Commune"),
         urban_rural = relevel(factor(urban_rural), ref = "Urban"))
m_q7 <- glm(acc_mob_aire ~ post_2005 + urban_rural + owner_group + family,
            family = binomial, data = q7_model_data)
co <- summary(m_q7)$coefficients
keep <- grep("^(post_2005|urban_rural|owner_group)", rownames(co))
q7_logit <- tibble(term = rownames(co)[keep],
                   OR = round(exp(co[keep, 1]), 3),
                   ci_low = round(exp(co[keep, 1] - 1.96 * co[keep, 2]), 3),
                   ci_high = round(exp(co[keep, 1] + 1.96 * co[keep, 2]), 3),
                   p_value = signif(co[keep, 4], 3))
cat("\nLogistic regression (family dummies included but not shown):\n")
print(q7_logit, n = Inf)
cat(sprintf("n = %d | AIC = %.0f\n", nrow(q7_model_data), AIC(m_q7)))
write_csv(q7_logit, "output/tables/Q7_logit.csv")

# (d) Profile over ALL periods: a jump at 2005, or a steady trend?
q7_profile <- es |>
  filter(!is.na(service_period)) |>
  group_by(service_period) |>
  summarise(n = n(),
            pct_documented = 100 * mean(acc_mob_documented),
            n_doc = sum(acc_mob_documented),
            pct_accessible = 100 * mean(acc_mob_aire, na.rm = TRUE),
            .groups = "drop") |>
  mutate(se = sqrt(pct_accessible * (100 - pct_accessible) / n_doc),
         ci_low = pct_accessible - 1.96 * se, ci_high = pct_accessible + 1.96 * se)
print(q7_profile)
write_csv(q7_profile, "output/tables/Q7_profile_by_period.csv")

p_q7 <- q7_profile |>
  select(service_period, `Accessible playing area (documented facilities)` = pct_accessible,
         `Accessibility documented (all facilities)` = pct_documented) |>
  pivot_longer(-service_period, names_to = "measure", values_to = "pct") |>
  ggplot(aes(x = service_period, y = pct, group = measure, colour = measure)) +
  geom_line(linewidth = 1) + geom_point(size = 2.5) +
  # discrete layers first, so the x axis is set as categorical before the marker line
  geom_vline(xintercept = 6.5, linetype = "dotted", colour = "grey40") +
  annotate("text", x = 6.55, y = 20, label = "2005 disability law", hjust = 0, size = 3.5, colour = "grey30") +
  scale_colour_manual(values = c("Accessible playing area (documented facilities)" = "#3B6EA8",
                                 "Accessibility documented (all facilities)" = "#8C8C8C"), name = NULL) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Q7. Accessibility by period of current configuration",
       subtitle = "A jump at 2005 would be consistent with a law effect; a steady rise would not",
       x = "Period of current configuration", y = "%", caption = src) +
  theme_project +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  guides(colour = guide_legend(nrow = 2))
ggsave("output/figures/Q7_accessibility_by_period.png", p_q7, width = 10, height = 6, dpi = 300)

sink()
cat("Done. Upload output/results_Q5_Q6_Q7.txt and the new figures.\n")
