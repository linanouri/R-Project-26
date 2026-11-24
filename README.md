# Beyond the Playing Field: Sports Infrastructure in France

An R analysis of France's national census of sports facilities (**Data ES**, Ministère chargé des Sports). It looks at **how many** facilities there are, **how well equipped** they are and **how accessible** they are across territories: urban vs rural areas, priority neighbourhoods (QPV), owner types and departments.

---

## Data

### Main dataset: Data ES (sports facilities census)

| | |
|---|---|
| Source | Ministère chargé des Sports, via [data.gouv.fr](https://www.data.gouv.fr/datasets/recensement-des-equipements-sportifs-espaces-et-sites-de-pratiques) |
| Licence | Licence Ouverte / Open Licence 2.0 |
| Main file | `data/raw/csv_main_335mb.csv`: 333,585 rows × 115 columns (≈ 351 MB) |
| Downloaded on | 2026-11-23 |
| MD5 | `8ca466591655945a2a3064e44fe69a88` |

> ⚠️ **The raw CSV is not stored in this repository** because it is over GitHub's 100 MB file limit.
>
> 📥 **Download the exact version used in this project here:**
> **[Data ES raw CSV (Google Drive)](https://drive.google.com/file/d/1NxuBu3N26rHWjt1d9Hh8p_XlgCh2ACZa/view?usp=sharing)**
>
> Save it as `data/raw/csv_main_335mb.csv`.

The source is updated **daily**, so a fresh download from data.gouv.fr will not exactly match the version analysed here. Use the Drive copy to reproduce the results exactly. You can check that your copy is the same file with the MD5 above:

```r
tools::md5sum("data/raw/csv_main_335mb.csv")
```

**You only need the raw CSV to re-run script 01.** Its fast-loading copy, `data/raw/es_raw.rds`, is already in the repo, so scripts 02 to 08 run without it.

### Supplementary sources (included in the repo)

| File | Source | Use |
|---|---|---|
| `data/raw/insee/insee_population_departements_2023.csv` | INSEE, Populations de référence 2023 (population municipale, 100 departments) | Facilities per inhabitant |
| `data/raw/insee/DS_FILOSOFI_SAGE_LOG_TP_NIVVIE_2023_*.csv` | INSEE Filosofi 2023: median standard of living and poverty rate | Income controls (Q10) |
| `data/raw/geo/departements-version-simplifiee.geojson` | IGN Admin Express COG, simplified (france-geojson, Licence Ouverte) | Maps |
| `2024_DATAES_Manuel_methodologique_arial.pdf` | Data ES methodological manual (Ministère des Sports, 04/03/2024) | Variable definitions and cleaning rules |
| `Populations des départements en 2023 _ Insee.pdf`, `Indicateurs sur les revenus et la pauvreté … _ Insee.pdf` | INSEE documentation | Reference for the supplementary data |

---

## Research questions

| # | Question | Method |
|---|---|---|
| **Q1** | Does the *mix* of facility types differ between urban and rural areas? | Shares by density level |
| **Q2** | Where are there the most facilities per inhabitant, and is high supply a low-density phenomenon? | Choropleth map, correlation |
| **Q3** | Is the age of facilities' current configuration associated with density? (+ robustness check on undated facilities) | Cross-tabulation, within-type comparison |
| **Q4** | Within the same facility type, are rural facilities less equipped? | Cochran–Mantel–Haenszel (common OR) |
| **Q5** | Do facilities in or near priority neighbourhoods (QPV) differ from comparable urban facilities of the same type? *(H3)* | CMH |
| **Q6** | Does the type of owner relate to facility age and equipment? | CMH vs communes |
| **Q7** | Are facilities configured since the 2005 disability law more often accessible? *(H2)* | Logistic regression |
| **Q8** | Is accessibility *documentation* itself unequal? | Logistic regression |
| **Q9** | How much of the variation in departments' apparent accessibility is explained by documentation alone? | Simple linear regression + variance decomposition |
| **QQG** | Quantity–Quality Gap: where does supply outpace equipment? `QQG = z(log facilities per 10k) − z(quality index)` | Composite indicator + map |
| **Q10** | What explains departments' documentation rate *(H4)* and equipment quality? | Multiple linear regression, 96 metropolitan departments |

---

## Project structure

```
.
├── 01_data_audit.R              # Stage 1: download, provenance, variable profile
├── 02_diagnostics.R             # Stage 2a: targeted diagnostics
├── 03_cleaning.R                # Stage 2b: logged cleaning -> es_clean.rds
├── 04_questions_Q1_Q3_Q4.R      # Stage 3a
├── 05_questions_Q5_Q6_Q7.R      # Stage 3b (+ Q3 robustness)
├── 06_questions_Q8_Q9.R         # Stage 3c -> dep_level.rds
├── 07_questions_Q2_Q10.R        # Stage 3d: maps, QQG, Q10 -> dep_final.rds
├── 08_course_alignment.R        # Stage 4: course methods (t-test, ANOVA, lm, RMSE…)
├── data/
│   ├── raw/                     # raw CSV (Drive link above), es_raw.rds, INSEE, geo
│   └── clean/                   # es_clean.rds, dep_level.rds, dep_final.rds
└── output/
    ├── audit/                   # provenance, variable profile, cleaning log, dictionary
    ├── figures/                 # all PNG charts and maps
    ├── tables/                  # result tables (CSV)
    └── results_*.txt            # full console output of each analysis script
```

---

## How to reproduce

1. **Clone** the repository and open the folder in RStudio. Set the working directory to the project root, because all paths are relative.
2. **Raw data:** only needed for script 01. Download it from the [Drive link](#main-dataset-data-es-sports-facilities-census) into `data/raw/`.
3. **Install packages:**
   ```r
   install.packages(c("data.table", "dplyr", "tidyr", "tibble", "readr", "stringr",
                      "janitor", "ggplot2", "forcats", "sf", "tidyverse"))
   ```
4. **Run the scripts in order:**

   | Script | Reads | Writes |
   |---|---|---|
   | `01_data_audit.R` | raw CSV | `data/raw/es_raw.rds`, `output/audit/00–04_*` |
   | `02_diagnostics.R` | `es_raw.rds` | `output/audit/05–06_*` |
   | `03_cleaning.R` | `es_raw.rds` | `data/clean/es_clean.rds`, `output/audit/07–09_*` |
   | `04_questions_Q1_Q3_Q4.R` | `es_clean.rds` | Q1, Q3, Q4 figures/tables |
   | `05_questions_Q5_Q6_Q7.R` | `es_clean.rds` | Q3 robustness, Q5–Q7 figures/tables |
   | `06_questions_Q8_Q9.R` | `es_clean.rds` | `dep_level.rds`, Q8–Q9 figures/tables |
   | `07_questions_Q2_Q10.R` | `es_clean.rds`, `dep_level.rds`, INSEE, geo | `dep_final.rds`, Q2, QQG, Q10 |
   | `08_course_alignment.R` | `es_raw.rds`, `es_clean.rds`, `dep_final.rds` | `C_*` figures |

   You can start at **script 02**, because `es_raw.rds` is already provided.

> Note: script 01 also downloads three secondary Data ES CSVs (≈ 360 MB in total) from data.gouv.fr for the audit step if they are not already present. Those files are not used by the later scripts.

---

## Key outputs

- **Audit trail:** `output/audit/07_cleaning_log.csv` logs every cleaning step with row counts before and after; nothing is deleted silently.
- **Data dictionary:** `output/audit/08_clean_dictionary.csv`
- **Maps:** `output/figures/Q2_map_supply.png`, `output/figures/QQG_map.png`
- **Full statistical results:** `output/results_*.txt`

---

## Sources & licence

- Data ES: Ministère chargé des Sports, data.gouv.fr, Licence Ouverte 2.0
- INSEE: Populations de référence 2023; Filosofi 2023
- IGN: Admin Express COG (simplified GeoJSON), Licence Ouverte
- All analyses, figures and indicators are our own calculations.
