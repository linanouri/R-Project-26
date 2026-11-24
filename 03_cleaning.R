# =====================================================================
# STAGE 2b — DATA CLEANING (Data ES)
# Input : data/raw/es_raw.rds        (created by 01_data_audit.R)
# Output: data/clean/es_clean.rds    (one row per facility, analysis-ready)
#         output/audit/07_cleaning_log.csv
#         output/audit/08_clean_dictionary.csv
#         output/audit/09_cleaning_report.txt
#
# Every decision below is justified by the Stage 2a diagnostics or by the
# Data ES methodological manual (Ministère des Sports, 04/03/2024).
# Nothing is deleted silently: each step is logged with row counts.
# =====================================================================

library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(tibble)

es_raw <- readRDS("data/raw/es_raw.rds")
sink("output/audit/09_cleaning_report.txt", split = TRUE)

cleaning_log <- tibble(step = character(), rows_before = integer(),
                       rows_after = integer(), decision = character())
log_step <- function(step, before, after, decision) {
  cleaning_log <<- add_row(cleaning_log, step = step, rows_before = before,
                           rows_after = after, decision = decision)
  cat(sprintf("[%s] %d -> %d rows | %s\n", step, before, after, decision))
}

es <- as_tibble(es_raw)

# ---------------------------------------------------------------------
# 1. SCOPE
# ---------------------------------------------------------------------
# 1a. Out-of-service installations (manual p.9: abandoned / closed / unmaintained)
n0 <- nrow(es)
es <- es |> filter(inst_hs_bool != "Oui" | is.na(inst_hs_bool))
log_step("1a_out_of_service", n0, nrow(es),
         "Removed installations flagged hors service (inst_hs_bool == 'Oui').")

# 1b. Keep the 101 departments (96 metropolitan + 5 DROM).
#     Overseas collectivities (975, 977, 978, 986, 987, 988) are not departments
#     and are not covered by the same INSEE statistics; missing dep_code cannot
#     be placed on a map or merged.
dep_metro  <- c(sprintf("%02d", c(1:19, 21:95)), "2A", "2B")
dep_drom   <- c("971", "972", "973", "974", "976")
valid_deps <- c(dep_metro, dep_drom)
stopifnot(length(valid_deps) == 101)

n0 <- nrow(es)
es <- es |> filter(dep_code %in% valid_deps)
log_step("1b_territory", n0, nrow(es),
         "Kept 101 departments; removed overseas collectivities and missing dep_code.")

# NOTE: equip_ouv_public_bool is NOT used as a filter. 64.5% 'false' is
# incompatible with 'closed to the public' in a register of public facilities;
# it most likely encodes the ERP opening order (manual p.12).

# ---------------------------------------------------------------------
# 2. BOOLEANS: "true"/"false" text -> logical
# ---------------------------------------------------------------------
bool_cols <- names(es)[vapply(es, function(x) {
  v <- unique(x[!is.na(x)])
  length(v) > 0 && all(v %in% c("true", "false"))
}, logical(1))]
es <- es |> mutate(across(all_of(bool_cols), ~ .x == "true"))
cat("\nConverted to logical:", length(bool_cols), "columns\n")

# ---------------------------------------------------------------------
# 3. ACCESSIBILITY: separate "not accessible" from "not documented"
# ---------------------------------------------------------------------
# Diagnostic D: equip_pmr_* is TRUE only when the text field lists the
# component, and is ALWAYS FALSE when the text field is empty.
# => FALSE mixes "not accessible" and "unknown". Undocumented -> NA.
pmr_cols  <- c("equip_pmr_aire", "equip_pmr_acc", "equip_pmr_chem", "equip_pmr_vest",
               "equip_pmr_douche", "equip_pmr_sanit", "equip_pmr_trib")
pshs_cols <- c("equip_pshs_aire", "equip_pshs_chem", "equip_pshs_sanit",
               "equip_pshs_sign", "equip_pshs_trib", "equip_pshs_vest")

es <- es |>
  mutate(
    acc_mob_documented  = !is.na(equip_acces_handi_mobilite),
    acc_sens_documented = !is.na(equip_acces_handi_sensoriel),
    across(all_of(pmr_cols),  ~ if_else(acc_mob_documented,  .x, NA)),
    across(all_of(pshs_cols), ~ if_else(acc_sens_documented, .x, NA)),
    # Main accessibility outcome: autonomous access to the playing area
    acc_mob_aire  = equip_pmr_aire,
    acc_sens_aire = equip_pshs_aire,
    # Installation level: derived from the type field, because
    # inst_acc_handi_bool is TRUE even when the type says "Non accessible"
    inst_acc_documented  = !is.na(inst_acc_handi_type),
    inst_acc_moteur      = if_else(inst_acc_documented,
                                   str_detect(inst_acc_handi_type, "Handicap moteur"), NA),
    inst_declared_non_acc = if_else(inst_acc_documented,
                                    str_detect(inst_acc_handi_type, "Non accessible"), NA)
  )

cat("\nAccessible playing area (mobility), documented facilities only:\n")
print(es |> filter(acc_mob_documented) |> count(acc_mob_aire) |>
        mutate(pct = round(100 * n / sum(n), 1)))

# ---------------------------------------------------------------------
# 4. AGE OF CURRENT CONFIGURATION
# ---------------------------------------------------------------------
# Manual p.13: the commissioning year is the year of the CURRENT configuration
# (a full rebuild resets it). Exact year mandatory only after 2005, so it is
# missing far more often for old facilities (diagnostic E) -> the PERIOD is the
# main age variable; the exact year is kept only for descriptive use.
period_levels <- c("Avant 1945", "1945-1964", "1965-1974", "1975-1984",
                   "1985-1994", "1995-2004", "A partir de 2005")
year_to_period <- function(y) {
  as.character(cut(y, breaks = c(-Inf, 1944, 1964, 1974, 1984, 1994, 2004, Inf),
                   labels = period_levels))
}
to_num <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))

es <- es |>
  mutate(
    service_year_raw   = to_num(equip_service_date),
    flag_year_future   = !is.na(service_year_raw) & service_year_raw > 2026,
    flag_year_very_old = !is.na(service_year_raw) & service_year_raw < 1850,
    service_year       = if_else(flag_year_future, NA_real_, service_year_raw),
    period_from_year   = year_to_period(service_year),
    period_filled      = is.na(equip_service_periode) & !is.na(period_from_year),
    service_period     = coalesce(equip_service_periode, period_from_year),
    service_period     = factor(service_period, levels = period_levels, ordered = TRUE),
    post_2005          = service_period == "A partir de 2005"
  )

cat("\nFuture years set to NA:", sum(es$flag_year_future),
    "| years < 1850 kept but flagged:", sum(es$flag_year_very_old), "\n")
cat("Periods filled from exact year:", sum(es$period_filled), "\n")
cat("Remaining missing period:", sum(is.na(es$service_period)),
    sprintf("(%.1f%%)\n", 100 * mean(is.na(es$service_period))))

# Consistency check: does the recorded period match the recorded year?
consistency <- es |>
  filter(!is.na(equip_service_periode), !is.na(period_from_year)) |>
  summarise(n = n(), pct_inconsistent = round(100 * mean(equip_service_periode != period_from_year), 2))
cat("Year/period inconsistency (both recorded):\n"); print(consistency)

# Last major works: 78% missing, and the manual gives no way to tell
# "never renovated" from "unknown" -> kept for description only.
es <- es |> mutate(works_year = to_num(equip_travaux_date),
                   works_year = if_else(works_year > 2026, NA_real_, works_year))

# ---------------------------------------------------------------------
# 5. DIMENSIONS AND COUNTS
# ---------------------------------------------------------------------
# Inspect suspicious repeated large values before deciding on sentinels
cat("\nMost frequent surface values above the 99th percentile:\n")
surf_tmp <- to_num(es$equip_surf)
print(head(sort(table(surf_tmp[surf_tmp > quantile(surf_tmp, 0.99, na.rm = TRUE)]),
                decreasing = TRUE), 10))

sentinels <- c(99999, 999999, 9999999)
es <- es |>
  mutate(
    across(c(equip_surf, equip_long, equip_larg, equip_trib_nb, equip_vest_sport,
             equip_vest_ens), to_num, .names = "{.col}_num"),
    # A playing area of 0 m or 0 m2 is impossible -> missing. Placeholder codes -> missing.
    across(c(equip_surf_num, equip_long_num, equip_larg_num),
           ~ if_else(.x <= 0 | .x %in% sentinels, NA_real_, .x)),
    # Changing rooms / seats: 0 is a REAL zero (manual p.16-17); negatives -> missing
    across(c(equip_trib_nb_num, equip_vest_sport_num, equip_vest_ens_num),
           ~ if_else(.x < 0, NA_real_, .x))
  ) |>
  rename(surface_m2 = equip_surf_num, length_m = equip_long_num, width_m = equip_larg_num,
         seats_nb = equip_trib_nb_num, changing_rooms_nb = equip_vest_sport_num,
         staff_rooms_nb = equip_vest_ens_num)

# Outliers: flagged WITHIN each facility family on the log scale (robust z with
# median/MAD), never deleted. A 60 km2 ski area is not an error; a 60 km2 gym is.
es <- es |>
  group_by(equip_type_famille) |>
  mutate(
    log_surf      = log(surface_m2),
    med_log       = median(log_surf, na.rm = TRUE),
    mad_log       = mad(log_surf, na.rm = TRUE),
    flag_surf_outlier = if_else(!is.na(log_surf) & mad_log > 0,
                                abs(log_surf - med_log) / mad_log > 3.5, FALSE)
  ) |>
  ungroup() |>
  select(-log_surf, -med_log, -mad_log)

cat("\nSurface flagged as outlier within family:", sum(es$flag_surf_outlier),
    "| changing rooms > 50 (check manually):", sum(es$changing_rooms_nb > 50, na.rm = TRUE), "\n")

# ---------------------------------------------------------------------
# 6. CATEGORIES
# ---------------------------------------------------------------------
# 6a. Floor type: 48 spellings -> official categories (manual p.14)
official_sol <- c("Surface naturelle", "Synthétique (hors gazon)", "Bitume", "Gazon naturel",
                  "Béton", "Sable", "Stabilisé/cendrée", "Parquet", "Carrelage",
                  "Gazon synthétique", "Terre battue", "Glace", "Bois", "Sciure/copeaux",
                  "Métal", "Hybride", "Terre artificielle", "Autre")
es <- es |>
  mutate(
    floor_type = case_when(
      equip_sol %in% c("Stabilisé / cendré")         ~ "Stabilisé/cendrée",
      equip_sol %in% c("Carrelage, tomettes, pavés") ~ "Carrelage",
      equip_sol %in% c("Bois (hors parquet)")        ~ "Bois",
      equip_sol %in% c("Sciure / copeaux")           ~ "Sciure/copeaux",
      equip_sol %in% c("Terre Artificielle")         ~ "Terre artificielle",
      equip_sol %in% c("Métal (acier, inox, etc.)")  ~ "Métal",
      equip_sol %in% c("Synthétique")                ~ "Synthétique (hors gazon)",
      equip_sol %in% official_sol                    ~ equip_sol,
      !is.na(equip_sol)                              ~ "Autre",   # rare free-text entries
      TRUE                                           ~ NA_character_
    )
  )

# 6b. Owner and manager: 12 types -> 5 analytical groups
group_actor <- function(x) {
  case_when(
    x == "Commune"                                       ~ "Commune",
    x %in% c("EPCI", "Département", "Région")            ~ "Other local authority",
    x %in% c("Etat", "Etablissement Public")             ~ "State / public body",
    x == "Etablissement privé commercial"                ~ "Private commercial",
    x %in% c("Association(s)", "Privé non commercial",
             "Etablissement d'enseignement privé",
             "Multi-propriétaire")                       ~ "Private non-commercial",
    TRUE                                                 ~ NA_character_   # "Autre", "other", NA
  )
}
es <- es |> mutate(owner_group = group_actor(equip_prop_type),
                   manager_group = group_actor(equip_gest_type))

# 6c. Urban/rural: INSEE 7-level density grid + INSEE's urban/rural split
dens_levels <- c("Grands centres urbains", "Centres urbains intermédiaires",
                 "Ceintures urbaines", "Petites villes", "Bourgs ruraux",
                 "Rural à habitat dispersé", "Rural à habitat très dispersé")
es <- es |>
  mutate(density = factor(dens_lib, levels = dens_levels, ordered = TRUE),
         urban_rural = case_when(dens_lib %in% dens_levels[1:4] ~ "Urban",
                                 dens_lib %in% dens_levels[5:7] ~ "Rural",
                                 TRUE ~ NA_character_))

# 6d. Priority neighbourhoods: an empty QPV code means "not in a QPV" (structural, not missing)
es <- es |> mutate(in_qpv = !is.na(equip_qpv), near_qpv_200m = !is.na(equip_qpv_200m))
cat("\nQPV cross-check (is 'within 200 m' a superset of 'inside'?):\n")
print(es |> count(in_qpv, near_qpv_200m))

# ---------------------------------------------------------------------
# 7. FINAL SELECTION
# ---------------------------------------------------------------------
es_clean <- es |>
  transmute(
    # identifiers & geography
    equip_id = equip_numero, inst_id = inst_numero, commune_code = new_code,
    commune_name = new_name, epci_code, dep_code, dep_nom, reg_code, reg_nom,
    lon = to_num(equip_x), lat = to_num(equip_y), density, urban_rural,
    in_qpv, near_qpv_200m,
    # facility type
    family = equip_type_famille, type = equip_type_name, type_code = equip_type_code,
    nature = equip_nature, floor_type, categorie,
    # age
    service_period, post_2005, service_year, period_filled,
    flag_year_very_old, works_year,
    # size and amenities
    surface_m2, length_m, width_m, flag_surf_outlier, seats_nb,
    changing_rooms_nb, staff_rooms_nb,
    lighting = equip_eclair, showers = equip_douche, toilets = equip_sanit,
    free_access = equip_acc_libre, seasonal = equip_saison, dsp = equip_dsp,
    # accessibility
    acc_mob_documented, acc_mob_aire, acc_sens_documented, acc_sens_aire,
    across(all_of(pmr_cols)), across(all_of(pshs_cols)),
    inst_acc_documented, inst_acc_moteur, inst_declared_non_acc,
    # ownership
    owner_type = equip_prop_type, owner_group, manager_type = equip_gest_type, manager_group
  )

cat("\nFinal clean dataset:", nrow(es_clean), "rows x", ncol(es_clean), "columns\n")
stopifnot(!anyDuplicated(es_clean$equip_id))

# ---------------------------------------------------------------------
# 8. SAVE + DOCUMENTATION
# ---------------------------------------------------------------------
saveRDS(es_clean, "data/clean/es_clean.rds")
write_csv(cleaning_log, "output/audit/07_cleaning_log.csv")

dictionary <- tibble(
  variable = names(es_clean),
  type = vapply(es_clean, function(x) class(x)[1], character(1)),
  pct_missing = round(100 * colMeans(is.na(es_clean)), 2)
)
write_csv(dictionary, "output/audit/08_clean_dictionary.csv")
print(dictionary, n = Inf)

sink()
cat("Done. Upload output/audit/09_cleaning_report.txt\n")
es_clean <- readRDS("data/clean/es_clean.rds") |>
  mutate(
    qpv_zone = factor(case_when(in_qpv ~ "Inside QPV", near_qpv_200m ~ "Within 200 m", TRUE ~ "Elsewhere"),
                      levels = c("Elsewhere", "Within 200 m", "Inside QPV")),
    changing_rooms_cat = cut(changing_rooms_nb, c(-Inf, 0, 2, Inf), labels = c("None", "1-2", "3+"))
  )
saveRDS(es_clean, "data/clean/es_clean.rds")
es_clean <- readRDS("data/clean/es_clean.rds")
table(es_clean$qpv_zone)
table(es_clean$changing_rooms_cat, useNA = "ifany")