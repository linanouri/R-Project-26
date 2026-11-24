# =====================================================================
# STAGE 4 — COURSE ALIGNMENT
# Adds, in the course's own syntax, every method from the three course
# documents that the project had not yet used explicitly:
#   Data Preprocessing : any/sum/colSums/rowSums(is.na), complete.cases,
#                        drop columns with many NA, getmode imputation,
#                        mean vs median, duplicated(), IQR outliers + boxplot,
#                        scale() standardisation, min-max normalisation
#   Chapter 2          : geom_histogram, geom_boxplot, geom_col + reorder +
#                        coord_flip, anti_join, slice_head
#   Chapter 3          : Pearson cor.test, t.test, aov (ANOVA), simple lm,
#                        plot(model, which = 1), factor + relevel, anova()
#                        nested models, predict() with confidence and
#                        prediction intervals, train/test split + RMSE
# Inputs : data/raw/es_raw.rds, data/clean/es_clean.rds, data/clean/dep_final.rds
# Output : output/results_course_alignment.txt  <- upload this file
#          output/figures/C_*.png
# =====================================================================

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, ggplot2, tidyr, forcats, readr (as in Chapter 2)
})

for (d in c("output/figures", "output/tables")) dir.create(d, recursive = TRUE, showWarnings = FALSE)
sink("output/results_course_alignment.txt", split = TRUE)

theme_project <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        panel.grid.minor = element_blank(), legend.position = "bottom")
src <- "Source: Data ES (Ministère des Sports), INSEE — own calculations"

# =====================================================================
# PART 1 — DATA PREPROCESSING (course: "Data Preprocessing" slides)
# =====================================================================
cat("\n================ PART 1: PREPROCESSING (course syntax) ================\n")
data <- as.data.frame(readRDS("data/raw/es_raw.rds"))

# 1. Any missing values in the whole dataset?
cat("any(is.na(data)):", any(is.na(data)), "\n")
# 2. Total number of missing values
cat("sum(is.na(data)):", sum(is.na(data)), "of", nrow(data) * ncol(data), "cells\n")
# 3. Missing values by column (the 10 worst)
na_by_col <- colSums(is.na(data))
cat("\ncolSums(is.na(data)) — 10 columns with most NA:\n")
print(head(sort(na_by_col, decreasing = TRUE), 10))
# 4. Missing values by row
na_by_row <- rowSums(is.na(data))
cat("\nrowSums(is.na(data)) — distribution of NA per facility:\n"); print(summary(na_by_row))
# 5-6. Complete cases: why "drop every row with an NA" is impossible here
cat("\nRows with NO missing value at all (complete.cases):", sum(complete.cases(data)), "\n")
cat("=> deleting incomplete rows would delete the whole dataset: NA must be handled column by column.\n")

# a) Remove columns with a lot of missing values (course example: 70% threshold)
pct_na <- 100 * colMeans(is.na(data))
cols_to_drop <- names(pct_na)[pct_na > 70]
cat("\nColumns with > 70% missing (dropped from the analysis):", length(cols_to_drop), "\n")
print(round(sort(pct_na[cols_to_drop], decreasing = TRUE), 1))
data_reduced <- data[, !names(data) %in% cols_to_drop]
cat("Columns before:", ncol(data), "| after:", ncol(data_reduced), "\n")

# Duplicates (course: duplicated / sum(duplicated))
cat("\nsum(duplicated(data)) — exact duplicate rows:", sum(duplicated(data)), "\n")
cat("sum(duplicated(data$equip_numero)) — duplicated IDs:", sum(duplicated(data$equip_numero)), "\n")
# Near-duplicates: same site, same type, same name (possible double entries)
near_dup <- sum(duplicated(data[, c("inst_numero", "equip_type_name", "equip_nom")]))
cat("Same site + same type + same name:", near_dup,
    "(not removed: e.g. 6 identical tennis courts on one site are 6 real facilities)\n")
rm(data, data_reduced); invisible(gc())

es <- readRDS("data/clean/es_clean.rds")

# c) Imputation — Case 2: mode of a categorical variable (course getmode function)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
cat("\nImputation of 'nature' (indoor/outdoor, ", sum(is.na(es$nature)),
    " NA) with the MODE of the same facility type:\n", sep = "")
es <- es |>
  group_by(type) |>
  mutate(nature_imputed = if_else(is.na(nature), getmode(nature[!is.na(nature)]), nature)) |>
  ungroup()
cat("NA before:", sum(is.na(es$nature)), "| after:", sum(is.na(es$nature_imputed)), "\n")

# c) Imputation — Case 1: why the MEAN is the wrong choice for surface
cat("\nSurface (m²): mean =", round(mean(es$surface_m2, na.rm = TRUE)),
    "| median =", round(median(es$surface_m2, na.rm = TRUE)), "\n")
cat("Mean imputation would give a missing tennis court the surface of the national mean,\n",
    "inflated by ski areas and nature sites. Surface is also missing NOT at random\n",
    "(98.9% missing for pools). Decision: no imputation; surface is analysed within family only.\n")
es |> filter(!is.na(surface_m2)) |> group_by(family) |>
  summarise(n = n(), mean_m2 = round(mean(surface_m2)), median_m2 = round(median(surface_m2)), .groups = "drop") |>
  arrange(desc(n)) |> slice_head(n = 8) |> print()

# Outliers — course IQR method, applied WITHIN one family (gymnasiums)
cat("\n---- Outliers: IQR rule (course method) on gymnasium surfaces ----\n")
gym <- es |> filter(family == "Salle multisports", !is.na(surface_m2))
IQR_value   <- IQR(gym$surface_m2)
lower_bound <- quantile(gym$surface_m2, 0.25) - 1.5 * IQR_value
upper_bound <- quantile(gym$surface_m2, 0.75) + 1.5 * IQR_value
gym_clean   <- gym[gym$surface_m2 >= lower_bound & gym$surface_m2 <= upper_bound, ]
cat(sprintf("Q1 = %.0f, Q3 = %.0f, IQR = %.0f, bounds = [%.0f ; %.0f]\n",
            quantile(gym$surface_m2, 0.25), quantile(gym$surface_m2, 0.75),
            IQR_value, lower_bound, upper_bound))
cat("Gymnasiums:", nrow(gym), "| outside the bounds:", nrow(gym) - nrow(gym_clean), "\n")

p_box <- bind_rows(mutate(gym, step = "Before removing outliers"),
                   mutate(gym_clean, step = "After removing outliers")) |>
  mutate(step = fct_inorder(step)) |>
  ggplot(aes(x = step, y = surface_m2)) +
  geom_boxplot(fill = "#C6DBEF") +
  labs(title = "Outliers in gymnasium surfaces (IQR rule, 1.5 × IQR)",
       x = NULL, y = "Surface of the playing area (m²)", caption = src) +
  theme_project
ggsave("output/figures/C_boxplot_iqr_gym.png", p_box, width = 8, height = 5, dpi = 300)

# IQR rule applied to ALL families at once vs within family: why grouping matters
iqr_flag <- function(x) {
  b <- quantile(x, c(.25, .75), na.rm = TRUE); i <- b[2] - b[1]
  x < b[1] - 1.5 * i | x > b[2] + 1.5 * i
}
cat("\nIQR outliers if applied to ALL facilities together:",
    sum(iqr_flag(es$surface_m2), na.rm = TRUE), "\n")
cat("IQR outliers if applied WITHIN each family:",
    sum(es |> group_by(family) |> mutate(f = iqr_flag(surface_m2)) |> pull(f), na.rm = TRUE), "\n")
cat("Decision kept in the project: flag, do not delete (a 60 km² ski area is real).\n")

# =====================================================================
# PART 2 — CHAPTER 2 CHARTS (histogram, geom_col + reorder + coord_flip)
# =====================================================================
cat("\n================ PART 2: CHAPTER 2 CHARTS ================\n")
dep <- readRDS("data/clean/dep_final.rds")
regions <- es |> distinct(dep_code, reg_nom) |> filter(!is.na(reg_nom)) |>
  group_by(dep_code) |> slice_head(n = 1) |> ungroup()
dep <- dep |> left_join(regions, by = "dep_code")

# anti_join: departments without population (course: shows rows with no match)
cat("Departments with no population (anti_join):\n")
print(dep |> select(dep_code, dep_nom) |>
        anti_join(dep |> filter(!is.na(population_municipale_2023)), by = "dep_code"))

# Histogram: why facilities per 10k is log-transformed in the regressions
p_hist <- dep |> filter(!is.na(facilities_per_10k)) |>
  transmute(`Raw scale` = facilities_per_10k, `Log scale` = log(facilities_per_10k)) |>
  pivot_longer(everything()) |>
  mutate(name = fct_inorder(name)) |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 25, fill = "steelblue", colour = "white") +
  facet_wrap(~ name, scales = "free_x") +
  labs(title = "Facilities per 10,000 inhabitants: skewed, hence the log in the models",
       x = "Value", y = "Number of departments", caption = src) +
  theme_project
ggsave("output/figures/C_histogram_supply.png", p_hist, width = 10, height = 5, dpi = 300)

# geom_col + reorder + coord_flip: the 15 most common facility families
p_col <- es |> count(family) |> slice_max(n, n = 15) |>
  ggplot(aes(x = reorder(family, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "The 15 most common families of sports facilities in France",
       x = NULL, y = "Number of facilities", caption = src) +
  theme_project
ggsave("output/figures/C_bar_families.png", p_col, width = 9, height = 6, dpi = 300)

# Department-level outliers (IQR) on supply — boxplot
b <- quantile(dep$facilities_per_10k, c(.25, .75), na.rm = TRUE)
cat("\nIQR outliers, facilities per 10k (departments):\n")
print(dep |> filter(facilities_per_10k > b[2] + 1.5 * diff(b)) |>
        select(dep_nom, facilities_per_10k) |> arrange(desc(facilities_per_10k)))

# =====================================================================
# PART 3 — CHAPTER 3 TESTS (cor.test Pearson, t.test, aov)
# =====================================================================
cat("\n================ PART 3: CHAPTER 3 TESTS ================\n")
m96 <- dep |> filter(metro) |>
  mutate(log_supply = log(facilities_per_10k), log_density = log(density),
         income_k = median_living_std / 1000,
         dep_type = if_else(pct_rural > 50, "Mostly rural", "Mostly urban"),
         reg_nom = factor(reg_nom))

# Pearson (course default) next to the Spearman already reported
cat("\nPearson correlation, log supply vs log density:\n")
print(cor.test(m96$log_supply, m96$log_density))
cat("Pearson correlation, log supply vs quality index:\n")
print(cor.test(m96$log_supply, m96$quality_index))

# t.test: numeric + factor with 2 groups
cat("\nt-test: equipment quality index, mostly rural vs mostly urban departments\n")
print(table(m96$dep_type))
print(t.test(quality_index ~ dep_type, data = m96))
p_tbox <- ggplot(m96, aes(x = dep_type, y = quality_index)) +
  geom_boxplot(fill = "#C6DBEF") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey40") +
  labs(title = "Equipment quality index by type of department",
       subtitle = "100 = national level for the same facility mix",
       x = NULL, y = "Equipment quality index", caption = src) +
  theme_project
ggsave("output/figures/C_boxplot_quality_ttest.png", p_tbox, width = 7, height = 5, dpi = 300)

# ANOVA: numeric + factor with 3+ groups (13 metropolitan regions)
# Survey work is organised by region (DRAJES, manual p.4): if documentation
# differs strongly between regions, survey organisation matters.
cat("\nANOVA: documentation rate by region (13 regions)\n")
print(summary(aov(pct_documented ~ reg_nom, data = m96)))
cat("Robustness (non-parametric): Kruskal-Wallis\n")
print(kruskal.test(pct_documented ~ reg_nom, data = m96))
cat("\nANOVA: equipment quality index by region\n")
print(summary(aov(quality_index ~ reg_nom, data = m96)))

p_reg <- m96 |>
  ggplot(aes(x = reorder(reg_nom, pct_documented, FUN = median), y = pct_documented)) +
  geom_boxplot(fill = "#C6DBEF") +
  coord_flip() +
  labs(title = "Accessibility documentation rate of departments, by region",
       x = NULL, y = "Facilities with accessibility documented (%)", caption = src) +
  theme_project
ggsave("output/figures/C_boxplot_documentation_region.png", p_reg, width = 9, height = 6, dpi = 300)

# =====================================================================
# PART 4 — CHAPTER 3 REGRESSION (simple → multiple → factor → anova → predict → RMSE)
# =====================================================================
cat("\n================ PART 4: CHAPTER 3 REGRESSION ================\n")
m96 <- m96 |> filter(!is.na(quality_index), !is.na(income_k), !is.na(pct_commune))

# Simple linear regression
model1 <- lm(quality_index ~ log_supply, data = m96)
cat("\n---- model1: simple regression ----\n"); print(summary(model1))
png("output/figures/C_model1_residuals.png", width = 1600, height = 1200, res = 200)
plot(model1, which = 1)          # residuals vs fitted (course)
dev.off()

# Multiple linear regression ("all else equal")
model2 <- lm(quality_index ~ log_supply + log_density + income_k + pct_commune, data = m96)
cat("\n---- model2: multiple regression ----\n"); print(summary(model2))
print(confint(model2))
cat(sprintf("log_supply: %.2f alone -> %.2f all else equal\n",
            coef(model1)["log_supply"], coef(model2)["log_supply"]))

# Factor with reference level + nested comparison with anova()
# (found with grep so that a console encoding problem with "Î" cannot break relevel)
ref_region <- grep("de-France", levels(m96$reg_nom), value = TRUE)[1]
m96$reg_nom <- relevel(m96$reg_nom, ref = ref_region)
model3 <- lm(quality_index ~ log_supply + log_density + income_k + pct_commune + reg_nom, data = m96)
cat("\n---- model3: model2 + region (reference: Île-de-France) ----\n")
cat(sprintf("Adjusted R²: model2 = %.3f | model3 = %.3f\n",
            summary(model2)$adj.r.squared, summary(model3)$adj.r.squared))
print(anova(model2, model3))

cat("\n---- Same test for documentation (H4): does region add information? ----\n")
docA  <- lm(pct_documented ~ log_supply + log_density + income_k + pct_commune, data = m96)
docA3 <- update(docA, . ~ . + reg_nom)
cat(sprintf("Adjusted R²: without region = %.3f | with region = %.3f\n",
            summary(docA)$adj.r.squared, summary(docA3)$adj.r.squared))
print(anova(docA, docA3))

# Sensitivity of model2 without the 6 influential departments (by CODE, not name)
infl_codes <- c("05", "2B", "55", "65", "75", "93")
model2_sub <- update(model2, data = filter(m96, !dep_code %in% infl_codes))
cat("\n---- model2 without the 6 influential departments ----\n")
cat("n =", nobs(model2_sub), "\n")
print(summary(model2_sub)$coefficients["log_supply", ])

# predict(): confidence vs prediction interval
new <- data.frame(log_supply  = log(quantile(m96$facilities_per_10k, c(.25, .75))),
                  log_density = median(m96$log_density),
                  income_k    = median(m96$income_k),
                  pct_commune = median(m96$pct_commune))
cat("\n---- predict(): typical department with LOW (Q1) vs HIGH (Q3) supply ----\n")
cat("Facilities per 10k:", round(exp(new$log_supply), 1), "\n")
print(predict(model2, new, interval = "confidence"))
print(predict(model2, new, interval = "prediction"))

# Train / test split and RMSE (course: set.seed(123), sample, RMSE)
cat("\n---- Train/test evaluation (70 train / 26 test departments) ----\n")
set.seed(123)
train_id <- sample(1:nrow(m96), 70)
train <- m96[train_id, ]
test  <- m96[-train_id, ]
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
f_multi  <- quality_index ~ log_supply + log_density + income_k + pct_commune
pred_m2  <- predict(lm(f_multi, data = train), newdata = test)
pred_m1  <- predict(lm(quality_index ~ log_supply, data = train), newdata = test)
pred_0   <- rep(mean(train$quality_index), nrow(test))   # baseline: no predictor
cat(sprintf("RMSE baseline (mean only) = %.2f | simple model = %.2f | multiple model = %.2f\n",
            rmse(test$quality_index, pred_0), rmse(test$quality_index, pred_m1),
            rmse(test$quality_index, pred_m2)))

# With 96 departments one split is noisy: repeat 200 random splits
reps <- replicate(200, {
  id <- sample(1:nrow(m96), 70); tr <- m96[id, ]; te <- m96[-id, ]
  c(baseline = rmse(te$quality_index, mean(tr$quality_index)),
    simple   = rmse(te$quality_index, predict(lm(quality_index ~ log_supply, tr), te)),
    multiple = rmse(te$quality_index, predict(lm(f_multi, tr), te)))
})
cat("Mean RMSE over 200 random splits:\n"); print(round(rowMeans(reps), 2))

# =====================================================================
# PART 5 — FEATURE SCALING (standardisation vs min-max normalisation)
# =====================================================================
cat("\n================ PART 5: FEATURE SCALING ================\n")
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
qqg <- m96 |>
  mutate(z_supply = as.numeric(scale(log_supply)), z_quality = as.numeric(scale(quality_index)),
         QQG_z = z_supply - z_quality,                                   # standardisation (project)
         QQG_minmax = minmax(log_supply) - minmax(quality_index))        # normalisation [0,1]
cat("Correlation between the two versions of the indicator:",
    round(cor(qqg$QQG_z, qqg$QQG_minmax), 3), "\n")
cat("Spearman (same ranking?):",
    round(cor(qqg$QQG_z, qqg$QQG_minmax, method = "spearman"), 3), "\n")
print(qqg |> arrange(desc(QQG_minmax)) |> select(dep_nom, QQG_z, QQG_minmax) |> slice_head(n = 5))

sink()
cat("Done. Upload output/results_course_alignment.txt and the C_*.png figures.\n")
