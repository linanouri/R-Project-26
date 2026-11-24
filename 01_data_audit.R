# =====================================================================
# STAGE 1 — DATA AUDIT
# Project : Beyond the Playing Field — sports infrastructure in France
# Source  : Data ES, Ministère chargé des Sports, data.gouv.fr
#           https://www.data.gouv.fr/datasets/recensement-des-equipements-sportifs-espaces-et-sites-de-pratiques
# Licence : Licence Ouverte / Open Licence 2.0
#
# Goal: inspect the REAL files before choosing questions or variables.
# Nothing here assumes a column name: everything is generic, on purpose.
# =====================================================================

## 0. Packages ---------------------------------------------------------
pkgs <- c("data.table", "dplyr", "tibble", "janitor", "readr")
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## 1. Folders, download, provenance -------------------------------------
for (d in c("data/raw", "data/clean", "output/audit")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

base_api <- "https://www.data.gouv.fr/api/1/datasets/r/"
resources <- tibble::tribble(
  ~short_name,      ~resource_id,                           ~ext,
  "csv_main_335mb", "ea4f5879-af40-4e3e-949d-812d6eeb5e02", "csv",
  "csv_068mb",      "d8234a0a-4791-4582-bd9e-6ac795dcc71d", "csv",
  "csv_174mb",      "82bae93a-cbe1-4f17-84a5-c5a53a8206ef", "csv",
  "csv_100mb",      "17beab6a-d588-43d1-a207-2e516655916d", "csv",
  "documentation",  "ad749b6c-5f6f-435f-8914-9e2734557ae6", "pdf"
)
resources$path <- file.path("data/raw", paste0(resources$short_name, ".", resources$ext))

options(timeout = 3600)  # large files: avoid the 60-second default timeout
for (i in seq_len(nrow(resources))) {
  if (!file.exists(resources$path[i])) {
    message("Downloading ", resources$short_name, " ...")
    download.file(paste0(base_api, resources$resource_id[i]),
                  resources$path[i], mode = "wb")
  }
}

# The source is updated daily: the download date is part of the method.
resources$downloaded_at <- format(file.info(resources$path)$mtime, "%Y-%m-%d %H:%M")
resources$size_mb       <- round(file.size(resources$path) / 1e6, 1)
resources$md5           <- unname(tools::md5sum(resources$path))
readr::write_csv(resources, "output/audit/00_provenance.csv")
print(resources[, c("short_name", "size_mb", "downloaded_at")])

## 2. Light inspection of every CSV: rows, columns, names ---------------
csv_paths <- resources$path[resources$ext == "csv"]

peek <- lapply(csv_paths, function(f) {
  head_dt <- fread(f, nrows = 200, colClasses = "character", encoding = "UTF-8")
  n_rows  <- nrow(fread(f, select = 1L, colClasses = "character", encoding = "UTF-8"))
  list(file = basename(f), n_rows = n_rows, n_cols = ncol(head_dt), cols = names(head_dt))
})

peek_summary <- rbindlist(lapply(peek, function(p) {
  data.table(file = p$file, n_rows = p$n_rows, n_cols = p$n_cols)
}))
print(peek_summary)
fwrite(peek_summary, "output/audit/01_files_summary.csv")

for (p in peek) {
  writeLines(p$cols, file.path("output/audit",
                               paste0("01_columns_", sub("\\.csv$", "", p$file), ".txt")))
}

# Columns shared with the main file = candidate join keys between files
main_cols <- peek[[1]]$cols
for (k in seq_along(peek)[-1]) {
  cat("\nColumns shared between main file and", peek[[k]]$file, ":\n")
  print(intersect(main_cols, peek[[k]]$cols))
}

## 3. Full load of the main file ----------------------------------------
# Everything read as text first: automatic type guessing on 330k rows can
# silently turn codes like "01" or "2A" into numbers/NA. Types are fixed
# deliberately in Stage 2, once we know what each column means.
es_raw <- fread(resources$path[1], colClasses = "character",
                na.strings = c("", "NA", "NULL"), encoding = "UTF-8")

name_map <- tibble(original = names(es_raw),
                   clean    = janitor::make_clean_names(names(es_raw)))
setnames(es_raw, name_map$clean)
readr::write_csv(name_map, "output/audit/02_name_map.csv")

cat("\nMain file:", nrow(es_raw), "rows x", ncol(es_raw), "columns\n")

## 4. Variable profile: missingness, cardinality, examples, numeric-ness --
looks_numeric <- function(x) {
  v <- x[!is.na(x)]
  if (length(v) == 0) return(NA_real_)
  round(mean(grepl("^-?[0-9]+([.,][0-9]+)?$", v)), 3)  # accepts French decimal comma
}

profile <- tibble(
  variable       = names(es_raw),
  n_missing      = vapply(es_raw, function(x) sum(is.na(x)), numeric(1)),
  pct_missing    = round(100 * n_missing / nrow(es_raw), 2),
  n_distinct     = vapply(es_raw, function(x) uniqueN(x, na.rm = TRUE), numeric(1)),
  share_numeric  = vapply(es_raw, looks_numeric, numeric(1)),
  example_values = vapply(es_raw, function(x) {
    paste(head(unique(x[!is.na(x)]), 3), collapse = " | ")
  }, character(1))
) |>
  arrange(desc(pct_missing))

readr::write_csv(profile, "output/audit/03_variable_profile.csv")
print(profile, n = Inf, width = Inf)

## 5. Duplicates and identifiers ----------------------------------------
n_exact_dup <- sum(duplicated(es_raw))
id_candidates <- profile$variable[profile$n_distinct == nrow(es_raw) & profile$n_missing == 0]
cat("\nExact duplicate rows:", n_exact_dup, "\n")
cat("Columns unique on every row (candidate IDs):\n"); print(id_candidates)

## 6. Modalities of categorical-looking variables -----------------------
# Columns with 2–40 distinct values: categories, yes/no flags, etc.
# This is where inconsistent spellings and odd codes show up.
cat_vars <- profile$variable[profile$n_distinct >= 2 & profile$n_distinct <= 40]

modalities <- rbindlist(lapply(cat_vars, function(v) {
  tab <- es_raw[, .N, by = v]
  setnames(tab, v, "modality")
  tab[, variable := v]
  tab[, pct := round(100 * N / sum(N), 2)]
  tab[order(-N)]
}), use.names = TRUE)
setcolorder(modalities, c("variable", "modality", "N", "pct"))
fwrite(modalities, "output/audit/04_modalities.csv")

## 7. Save a fast-loading copy for the next stages -----------------------
saveRDS(es_raw, "data/raw/es_raw.rds")

cat("\nDone. Send back the files in output/audit/ and the PDF in data/raw/.\n")
