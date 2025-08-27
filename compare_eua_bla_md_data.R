# Load necessary package
library(haven)
library(dplyr)
library(readr)
library(jsonlite)
library(ggplot2)

eua_adsl_file <- 'eua_data/xpt_data/FDA-CBER-2021-5683-1226624-1227706_27034_S1_M5_c4591001-ia efficacy-A-adsl.xpt'

bla_adsl_file <- 'xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt'

# Ensure needed packages are available
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

# Output folder
out_dir <- "analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_xpt_compat <- function(path) {
  tryCatch(
    haven::read_xpt(path, name_repair = "minimal"),
    error = function(e) haven::read_xpt(path)
  )
}

eua <- read_xpt_compat(eua_adsl_file)
bla <- read_xpt_compat(bla_adsl_file)


# Basic checks
if (!("SUBJID" %in% names(eua))) stop("SUBJID not found in EUA dataset.")
if (!("SUBJID" %in% names(bla))) stop("SUBJID not found in BLA dataset.")

# Always compare SUBJID as character
eua <- eua %>% mutate(SUBJID = as.character(SUBJID))
bla <- bla %>% mutate(SUBJID = as.character(SUBJID))

# Helper to normalize values for fair comparison
normalize_vec <- function(x) {
  x <- haven::zap_labels(x)              # drop labelled class, keep underlying values
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.character(x))              # ISO yyyy-mm-dd
  }
  if (is.numeric(x)) {
    return(format(x, trim = TRUE, scientific = FALSE, digits = 15))
  }
  # default: character/factor/logical/etc.
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x
}

# 1) Columns added/removed ------------------------------------------------------
cols_eua <- names(eua)
cols_bla <- names(bla)

all_cols <- union(cols_eua, cols_bla)
col_report <- tibble::tibble(
  column = all_cols,
  in_eua = column %in% cols_eua,
  in_bla = column %in% cols_bla
) %>%
  mutate(status = dplyr::case_when(
    in_eua & in_bla ~ "common",
    in_eua & !in_bla ~ "only_in_eua",
    !in_eua & in_bla ~ "only_in_bla",
    TRUE ~ "unknown"
  )) %>%
  arrange(match(status, c("only_in_eua","only_in_bla","common")), column)

write_csv(col_report, file.path(out_dir, "adsl_eua_vs_bla_columns.csv"))

# 2) SUBJIDs inserted/deleted ---------------------------------------------------
subj_eua <- unique(eua$SUBJID)
subj_bla <- unique(bla$SUBJID)

subj_only_in_eua <- setdiff(subj_eua, subj_bla)
subj_only_in_bla <- setdiff(subj_bla, subj_eua)
subj_common      <- intersect(subj_eua, subj_bla)

subj_changes <- tibble::tibble(
  SUBJID = c(subj_only_in_eua, subj_only_in_bla),
  status = c(
    rep("only_in_eua", length(subj_only_in_eua)),
    rep("only_in_bla", length(subj_only_in_bla))
  )
) %>% arrange(status, SUBJID)

write_csv(subj_changes, file.path(out_dir, "adsl_eua_vs_bla_subjid_changes.csv"))

# Also flag any duplicate SUBJIDs in either file (useful sanity check)
dup_eua <- eua %>% count(SUBJID, name = "n") %>% filter(n > 1) %>% mutate(dataset = "EUA")
dup_bla <- bla %>% count(SUBJID, name = "n") %>% filter(n > 1) %>% mutate(dataset = "BLA")
dup_both <- bind_rows(dup_eua, dup_bla) %>% select(dataset, SUBJID, n)
if (nrow(dup_both) > 0) {
  write_csv(dup_both, file.path(out_dir, "adsl_eua_vs_bla_duplicate_subjid.csv"))
}

# 3) Value comparisons for common SUBJIDs & columns ----------------------------
common_cols <- intersect(cols_eua, cols_bla)
common_cols_no_id <- setdiff(common_cols, "SUBJID")

# Keep only common SUBJIDs and common columns
eua_common <- eua %>%
  filter(SUBJID %in% subj_common) %>%
  select(SUBJID, dplyr::all_of(common_cols_no_id))

bla_common <- bla %>%
  filter(SUBJID %in% subj_common) %>%
  select(SUBJID, dplyr::all_of(common_cols_no_id))

# Join and compare
joined <- inner_join(
  eua_common, bla_common,
  by = "SUBJID", suffix = c("_eua", "_bla")
)

# Build a long table of differences
diff_rows <- list()

for (col in common_cols_no_id) {
  e_col <- normalize_vec(joined[[paste0(col, "_eua")]])
  b_col <- normalize_vec(joined[[paste0(col, "_bla")]])
  
  different <- xor(is.na(e_col), is.na(b_col)) | (!is.na(e_col) & !is.na(b_col) & e_col != b_col)
  
  if (any(different)) {
    idx <- which(different)
    if (length(idx) > 0) {
      diff_rows[[col]] <- tibble::tibble(
        SUBJID = joined$SUBJID[idx],
        variable = col,
        value_eua = e_col[idx],
        value_bla = b_col[idx]
      )
    }
  }
}

diff_table <- if (length(diff_rows)) bind_rows(diff_rows) else tibble::tibble(
  SUBJID = character(), variable = character(), value_eua = character(), value_bla = character()
)

write_csv(diff_table, file.path(out_dir, "adsl_eua_vs_bla_value_differences.csv"))

# Per-variable summary (handy overview)
by_var <- diff_table %>%
  count(variable, name = "n_differences") %>%
  arrange(desc(n_differences))

write_csv(by_var, file.path(out_dir, "adsl_eua_vs_bla_value_differences_by_variable.csv"))

# JSON summary ------------------------------------------------------------------
summary_list <- list(
  columns = list(
    n_eua = length(cols_eua),
    n_bla = length(cols_bla),
    n_common = length(common_cols),
    only_in_eua = sort(setdiff(cols_eua, cols_bla)),
    only_in_bla = sort(setdiff(cols_bla, cols_eua))
  ),
  subjid = list(
    n_eua = length(subj_eua),
    n_bla = length(subj_bla),
    n_common = length(subj_common),
    only_in_eua = length(subj_only_in_eua),
    only_in_bla = length(subj_only_in_bla)
  ),
  value_differences = list(
    total_rows = nrow(diff_table),
    by_variable = setNames(as.list(by_var$n_differences), by_var$variable)
  ),
  notes = list(
    comparison = "Exact match after normalization (trimmed strings; empty strings -> NA; dates to ISO; numeric formatted non-scientific with up to 15 digits)."
  )
)

write_json(summary_list, file.path(out_dir, "adsl_eua_vs_bla_summary.json"), pretty = TRUE, auto_unbox = TRUE)

# Console summary
cat("\nReports written to '", out_dir, "':\n", sep = "")
cat(" - eua_vs_bla_columns.csv\n")
cat(" - eua_vs_bla_subjid_changes.csv\n")
if (exists("dup_both") && nrow(dup_both) > 0) cat(" - eua_vs_bla_duplicate_subjid.csv\n")
cat(" - eua_vs_bla_value_differences.csv\n")
cat(" - eua_vs_bla_value_differences_by_variable.csv\n")
cat(" - eua_vs_bla_summary.json\n\n")
