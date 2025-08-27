# Load necessary package
library(haven)
library(dplyr)
library(readr)
library(jsonlite)
library(ggplot2)

eua_adsl_file <- 'eua_data/xpt_data/FDA-CBER-2021-5683-1495707-1496004_27034_S1_M5_c4591001-safety-fa-eff-A-adva.xpt'

bla_adsl_file <- 'xpt_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.xpt'

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

out_dir <- "analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Backward-compatible XPT reader (handles older haven without name_repair)
read_xpt_compat <- function(path) {
  tryCatch(
    haven::read_xpt(path, name_repair = "minimal"),
    error = function(e) haven::read_xpt(path)
  )
}

# Robust PARAM normalization (trim, collapse spaces, replace en/em dashes with ASCII hyphen)
normalize_param <- function(x) {
  x <- haven::zap_labels(x)
  x <- as.character(x)
  x <- gsub("\u2013|\u2014", "-", x)      # en/em dash -> hyphen
  x <- gsub("\\s+", " ", x)               # collapse multiple spaces
  trimws(x)
}

# Read the two ADVA datasets
eua <- read_xpt_compat(eua_adsl_file)
bla <- read_xpt_compat(bla_adsl_file)

# Filter: keep ONLY the target PARAM
target_param <- "N-binding antibody - N-binding Antibody Assay"
target_param_norm <- normalize_param(target_param)

eua <- eua %>%
  mutate(PARAM = normalize_param(PARAM)) %>%
  filter(PARAM == target_param_norm)

bla <- bla %>%
  mutate(PARAM = normalize_param(PARAM)) %>%
  filter(PARAM == target_param_norm)

# Required composite key
key_cols <- c("SUBJID", "VISIT", "PARAM")
missing_keys_eua <- setdiff(key_cols, names(eua))
missing_keys_bla <- setdiff(key_cols, names(bla))
if (length(missing_keys_eua)) stop("EUA ADVA missing key columns: ", paste(missing_keys_eua, collapse = ", "))
if (length(missing_keys_bla)) stop("BLA ADVA missing key columns: ", paste(missing_keys_bla, collapse = ", "))

# Normalize keys for safe matching (character + trimmed)
normalize_key <- function(df) {
  df %>%
    mutate(
      SUBJID = trimws(as.character(SUBJID)),
      VISIT  = trimws(as.character(VISIT)),
      PARAM  = trimws(as.character(PARAM))  # already normalized to ASCII hyphen
    )
}

eua_k <- normalize_key(eua)
bla_k <- normalize_key(bla)

# (Optional) duplicate-key sanity check AFTER filtering to target PARAM
dup_eua <- eua_k %>% count(across(all_of(key_cols)), name = "n") %>% filter(n > 1) %>% mutate(dataset = "EUA")
dup_bla <- bla_k %>% count(across(all_of(key_cols)), name = "n") %>% filter(n > 1) %>% mutate(dataset = "BLA")
dup_both <- bind_rows(dup_eua, dup_bla)
if (nrow(dup_both) > 0) write_csv(dup_both, file.path(out_dir, "adva_duplicate_keys.csv"))

# Value normalization for fair comparisons
normalize_vec <- function(x) {
  x <- haven::zap_labels(x)
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (inherits(x, "Date"))   return(as.character(x))
  if (is.numeric(x))         return(format(x, trim = TRUE, scientific = FALSE, digits = 15))
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x
}

# Columns to compare
priority_set <- c("ARM", "AVISIT", "ISDTC", "AVALC")
common_cols  <- intersect(names(eua_k), names(bla_k))
priority_cols <- intersect(priority_set, common_cols)
other_cols <- setdiff(common_cols, c(key_cols, priority_cols))

# Generic comparer for a set of columns (only rows common to BOTH files)
compare_cols <- function(cols_to_compare) {
  if (length(cols_to_compare) == 0) {
    return(tibble(SUBJID = character(), VISIT = character(), PARAM = character(),
                  variable = character(), value_eua = character(), value_bla = character()))
  }

  eua_sel <- eua_k %>% select(all_of(key_cols), all_of(cols_to_compare))
  bla_sel <- bla_k %>% select(all_of(key_cols), all_of(cols_to_compare))

  joined <- inner_join(eua_sel, bla_sel, by = key_cols, suffix = c("_eua", "_bla"))

  diffs <- list()
  for (col in cols_to_compare) {
    e_col <- normalize_vec(joined[[paste0(col, "_eua")]])
    b_col <- normalize_vec(joined[[paste0(col, "_bla")]])

    different <- xor(is.na(e_col), is.na(b_col)) | (!is.na(e_col) & !is.na(b_col) & e_col != b_col)
    if (any(different)) {
      idx <- which(different)
      diffs[[col]] <- tibble(
        SUBJID = joined$SUBJID[idx],
        VISIT  = joined$VISIT[idx],
        PARAM  = joined$PARAM[idx],
        variable = col,
        value_eua = e_col[idx],
        value_bla = b_col[idx]
      )
    }
  }

  if (length(diffs)) bind_rows(diffs) else tibble(
    SUBJID = character(), VISIT = character(), PARAM = character(),
    variable = character(), value_eua = character(), value_bla = character()
  )
}

# Run the two comparison passes
priority_diff <- compare_cols(priority_cols)
other_diff    <- compare_cols(other_cols)

# Write detailed diff tables
write_csv(priority_diff, file.path(out_dir, "adva_value_differences_priority.csv"))
write_csv(other_diff,    file.path(out_dir, "adva_value_differences_other.csv"))

# Per-variable summaries
priority_by_var <- priority_diff %>% count(variable, name = "n_differences") %>% arrange(desc(n_differences))
other_by_var    <- other_diff    %>% count(variable, name = "n_differences") %>% arrange(desc(n_differences))

write_csv(priority_by_var, file.path(out_dir, "adva_value_differences_priority_by_variable.csv"))
write_csv(other_by_var,    file.path(out_dir, "adva_value_differences_other_by_variable.csv"))

# JSON summary
summary_list <- list(
  keys = list(
    columns = key_cols,
    duplicates_reported = nrow(dup_both) > 0
  ),
  filter = list(
    PARAM_equals = target_param
  ),
  compared = list(
    priority = priority_cols,
    other = other_cols
  ),
  results = list(
    priority = list(
      rows_with_differences = nrow(priority_diff),
      by_variable = setNames(as.list(priority_by_var$n_differences), priority_by_var$variable)
    ),
    other = list(
      rows_with_differences = nrow(other_diff),
      by_variable = setNames(as.list(other_by_var$n_differences), other_by_var$variable)
    )
  ),
  notes = list(
    scope = "Only rows present in BOTH files after filtering (inner join on SUBJID+VISIT+PARAM). No insert/delete detection.",
    comparison = "Exact match after normalization (trim strings; empty->NA; dates to ISO; numerics formatted, non-scientific)."
  )
)

write_json(summary_list, file.path(out_dir, "adva_summary.json"), pretty = TRUE, auto_unbox = TRUE)

cat("\nADVA comparison (filtered to PARAM == '", target_param, "') reports written to '", out_dir, "':\n", sep = "")
cat(" - adva_value_differences_priority.csv\n")
cat(" - adva_value_differences_other.csv\n")
cat(" - adva_value_differences_priority_by_variable.csv\n")
cat(" - adva_value_differences_other_by_variable.csv\n")
if (nrow(dup_both) > 0) cat(" - adva_duplicate_keys.csv (duplicate key rows found)\n")
cat(" - adva_summary.json\n\n")
