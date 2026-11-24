# =====================================================================
# STAGE 2a — TARGETED DIAGNOSTICS (Data ES)
# Uses the real column names found in the Stage 1 audit.
# All output is written to output/audit/05_diagnostics.txt
# =====================================================================

library(data.table)
library(stringr)

es <- readRDS("data/raw/es_raw.rds")
setDT(es)

sink("output/audit/05_diagnostics.txt", split = TRUE)  # console + file

tab <- function(dt, by) {
  out <- dt[, .N, by = by][order(-N)]
  out[, pct := round(100 * N / sum(N), 2)]
  print(out, nrows = 300)
}

## A. Identify the three other files ------------------------------------
for (f in c("csv_068mb", "csv_174mb", "csv_100mb")) {
  cat("\n=====", f, "— column names =====\n")
  print(readLines(file.path("output/audit", paste0("01_columns_", f, ".txt"))))
}

cat("\n===== Documentation PDF check =====\n")
cat("Size (bytes):", file.size("data/raw/documentation.pdf"), "\n")
cat("First line:", readLines("data/raw/documentation.pdf", n = 1, warn = FALSE), "\n")

## B. Scope variables: service status, public opening, territories -------
cat("\n===== B. Scope =====\n")
for (v in c("inst_hs_bool", "equip_ouv_public_bool", "equip_nature",
            "categorie", "equip_dsp", "equip_acc_libre")) {
  cat("\n---", v, "---\n"); tab(es, v)
}
cat("\n--- Departments (code + name) ---\n")
tab(es, c("dep_code", "dep_nom"))

## C. Key categorical variables -----------------------------------------
cat("\n===== C. Key categories =====\n")
for (v in c("equip_type_famille", "equip_service_periode", "equip_travaux_periode",
            "equip_prop_type", "equip_gest_type", "dens_lib",
            "inst_acc_handi_type", "equip_sol")) {
  cat("\n---", v, "---\n"); tab(es, v)
}

## D. CRITICAL: are accessibility booleans real zeros or hidden NAs? ----
cat("\n===== D. Boolean coding test =====\n")
# If 'false' appears whenever the detailed text field is empty, then
# 'false' mixes "not accessible" with "not filled in".
es[, txt_mob_missing  := is.na(equip_acces_handi_mobilite)]
es[, txt_mob_has_aire := str_detect(equip_acces_handi_mobilite, "Aire de jeu")]
cat("\n--- equip_pmr_aire vs mobility text field ---\n")
tab(es, c("txt_mob_missing", "txt_mob_has_aire", "equip_pmr_aire"))

es[, txt_sens_missing  := is.na(equip_acces_handi_sensoriel)]
es[, txt_sens_has_aire := str_detect(equip_acces_handi_sensoriel, "Aire de jeu")]
cat("\n--- equip_pshs_aire vs sensory text field ---\n")
tab(es, c("txt_sens_missing", "txt_sens_has_aire", "equip_pshs_aire"))

cat("\n--- inst_acc_handi_bool vs inst_acc_handi_type ---\n")
tab(es, c("inst_acc_handi_bool", "inst_acc_handi_type"))

cat("\n--- Equipment booleans ---\n")
for (v in c("equip_eclair", "equip_douche", "equip_sanit")) {
  cat("\n", v, "\n"); tab(es, v)
}

## E. Is missing construction year related to age? ----------------------
cat("\n===== E. Construction year vs period =====\n")
es[, has_year   := !is.na(equip_service_date)]
es[, has_period := !is.na(equip_service_periode)]
tab(es, c("has_year", "has_period"))

cat("\n--- % of exact years missing, by construction period ---\n")
# If older periods lack exact years more often, missingness is NOT random.
print(es[has_period == TRUE,
         .(n = .N, pct_year_missing = round(100 * mean(!has_year), 1)),
         by = equip_service_periode][order(equip_service_periode)])

## F. Does missingness vary by department? -------------------------------
cat("\n===== F. Missingness by department =====\n")
miss_dep <- es[, .(n            = .N,
                   miss_year    = round(100 * mean(is.na(equip_service_date)), 1),
                   miss_period  = round(100 * mean(is.na(equip_service_periode)), 1),
                   miss_mob_txt = round(100 * mean(is.na(equip_acces_handi_mobilite)), 1),
                   miss_surf    = round(100 * mean(is.na(equip_surf)), 1),
                   pct_pmr_aire = round(100 * mean(equip_pmr_aire == "true"), 1)),
               by = dep_code]
print(summary(miss_dep[, !"dep_code"]))
cat("\nCorrelation: missing mobility text vs % 'accessible' (by department):\n")
print(cor.test(miss_dep$miss_mob_txt, miss_dep$pct_pmr_aire, method = "spearman"))
fwrite(miss_dep, "output/audit/06_missing_by_department.csv")

## G. Data freshness -------------------------------------------------------
cat("\n===== G. Last update year =====\n")
es[, maj_year := substr(equip_maj_date, 1, 4)]
print(es[, .N, by = maj_year][order(maj_year)])

## H. Numeric sanity checks ------------------------------------------------
cat("\n===== H. Numeric variables =====\n")
to_num <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))
for (v in c("equip_surf", "equip_long", "equip_larg", "equip_service_date",
            "equip_travaux_date", "equip_trib_nb", "equip_vest_sport")) {
  x <- to_num(es[[v]])
  cat("\n---", v, "---\n")
  print(summary(x))
  print(quantile(x, c(0.01, 0.99, 0.999), na.rm = TRUE))
  cat("Zero:", sum(x == 0, na.rm = TRUE),
      "| Negative:", sum(x < 0, na.rm = TRUE),
      "| Non-numeric text:", sum(is.na(x) & !is.na(es[[v]])), "\n")
}
yr <- to_num(es$equip_service_date)
cat("\nConstruction years < 1850 or > 2026:", sum(yr < 1850 | yr > 2026, na.rm = TRUE), "\n")

## I. Surface by facility family (comparability check) ---------------------
cat("\n===== I. Median surface by facility family =====\n")
es[, surf_num := to_num(equip_surf)]
print(es[, .(n = .N,
             pct_surf_missing = round(100 * mean(is.na(surf_num)), 1),
             median_surf = median(surf_num, na.rm = TRUE)),
         by = equip_type_famille][order(-n)], nrows = 50)

sink()
cat("Done: please upload output/audit/05_diagnostics.txt\n")
