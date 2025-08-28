# Loads necessary packages
library(haven)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(stats)

# -------------------------------------------------------------------------------
# Reading the input XPT files
bla_adsl_data <- read_xpt("xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt")
bla_aes_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0775805-0776791_125742_S1_M5_C4591001-S-D_ae.xpt')

# Filtering and selecting necessary columns
bla_adsl_data_filtered <- bla_adsl_data %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT, V01DT, V02DT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)
print(bla_adsl_data_filtered)

# Processing AES data
bla_aes_data_selected <- bla_aes_data[c("USUBJID", "AESPID", "AESTDTC", "AETERM", "AESER")]
print(bla_aes_data_selected)
bla_aes_data_filtered <- bla_aes_data_selected %>%
  mutate(SUBJID = substr(USUBJID, 15, 24)
         ) %>%
  select(SUBJID, AESPID, AESTDTC, AETERM, AESER)
print(bla_aes_data_filtered)

# -------------------------------------------------------------------------------
# Reading the input XPT files
eua_adsl_data <- read_xpt("eua_data/xpt_data/FDA-CBER-2021-5683-1226624-1227706_27034_S1_M5_c4591001-ia efficacy-A-adsl.xpt")
eua_aes_data <- read_xpt('eua_data/xpt_data/FDA-CBER-2021-5683-1231982-1232400_27034_S1_M5_c4591001-ia efficacy-S-ae.xpt')

# Filtering and selecting necessary columns
eua_adsl_data_filtered <- eua_adsl_data %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, RANDDT, RFICDT, V01DT, V02DT, VAX101DT, VAX102DT)
print(eua_adsl_data_filtered)

# Processing AES data
eua_aes_data_selected <- eua_aes_data[c("USUBJID", "AESPID", "AESTDTC", "AETERM", "AESER")]
print(eua_aes_data_selected)
eua_aes_data_filtered <- eua_aes_data_selected %>%
  mutate(SUBJID = substr(USUBJID, 15, 24)
         ) %>%
  select(SUBJID, AESPID, AESTDTC, AETERM, AESER)
print(eua_aes_data_filtered)

# -------------------------------------------------------------------------------
# --- 0) Derive a plain start-date string ---------------------------
eua_aes_prepped <- eua_aes_data_filtered %>%
  mutate(AE_STARTDATE = substr(AESTDTC %>% as.character(), 1, 10))

bla_aes_prepped <- bla_aes_data_filtered %>%
  mutate(AE_STARTDATE = substr(AESTDTC %>% as.character(), 1, 10))


# --- 1) List unique AE keys (SUBJID + AESPID) in each dataset ----------------
eua_ae_keys <- eua_aes_prepped %>%
  distinct(SUBJID, AESPID) %>%
  arrange(SUBJID, AESPID)

bla_ae_keys <- bla_aes_prepped %>%
  distinct(SUBJID, AESPID) %>%
  arrange(SUBJID, AESPID)

print(eua_ae_keys)
print(bla_ae_keys)

# --- 2) Check consistency *within* each dataset ------------------------------
# Are there multiple dates/terms/seriousness values for the same SUBJID+AESPID?
check_within_ae <- function(df, label) {
  df %>%
    group_by(SUBJID, AESPID) %>%
    summarise(
      n_dates = n_distinct(AE_STARTDATE, na.rm = TRUE),
      dates   = paste(sort(unique(na.omit(AE_STARTDATE))), collapse = "; "),
      n_terms = n_distinct(AETERM, na.rm = TRUE),
      terms   = paste(sort(unique(na.omit(AETERM))), collapse = " | "),
      n_ser   = n_distinct(AESER, na.rm = TRUE),
      ser     = paste(sort(unique(na.omit(AESER))), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(dataset = label) %>%
    relocate(dataset, .before = SUBJID) %>%
    arrange(desc(n_dates), desc(n_terms), desc(n_ser), SUBJID, AESPID)
}

eua_within <- check_within_ae(eua_aes_prepped, "EUA")
bla_within <- check_within_ae(bla_aes_prepped, "BLA")

# Rows with internal inconsistencies (multiple values for the same key)
eua_inconsistent <- eua_within %>% filter(n_dates > 1 | n_terms > 1 | n_ser > 1)
bla_inconsistent <- bla_within %>% filter(n_dates > 1 | n_terms > 1 | n_ser > 1)

print(eua_inconsistent)
print(bla_inconsistent)


# --- 3) Compare *across* datasets by SUBJID + AESPID -------------------------
eua_collapse <- eua_within %>%
  select(SUBJID, AESPID,
         DATES_EUA = dates, TERMS_EUA = terms, SER_EUA = ser)

bla_collapse <- bla_within %>%
  select(SUBJID, AESPID,
         DATES_BLA = dates, TERMS_BLA = terms, SER_BLA = ser)

ae_cross_comp <- full_join(eua_collapse, bla_collapse, by = c("SUBJID", "AESPID")) %>%
  mutate(
    date_match = DATES_EUA == DATES_BLA,
    term_match = TERMS_EUA == TERMS_BLA,
    ser_match  = SER_EUA   == SER_BLA
  ) %>%
  arrange(SUBJID, AESPID)

print(ae_cross_comp)

# Focused diagnostics
both_present_date_mismatch <- ae_cross_comp %>%
  filter(!is.na(DATES_EUA), !is.na(DATES_BLA), !date_match)

both_present_term_mismatch <- ae_cross_comp %>%
  filter(!is.na(TERMS_EUA), !is.na(TERMS_BLA), !term_match)

both_present_ser_mismatch <- ae_cross_comp %>%
  filter(!is.na(SER_EUA), !is.na(SER_BLA), !ser_match)

missing_in_bla <- ae_cross_comp %>%
  filter(!is.na(DATES_EUA) | !is.na(TERMS_EUA) | !is.na(SER_EUA)) %>%
  filter(is.na(DATES_BLA) & is.na(TERMS_BLA) & is.na(SER_BLA))

missing_in_eua <- ae_cross_comp %>%
  filter(!is.na(DATES_BLA) | !is.na(TERMS_BLA) | !is.na(SER_BLA)) %>%
  filter(is.na(DATES_EUA) & is.na(TERMS_EUA) & is.na(SER_EUA))

# Quick counts
cat("# date mismatches where both present:", nrow(both_present_date_mismatch), "\n")
print(both_present_date_mismatch, n=300)
cat("# term mismatches where both present:", nrow(both_present_term_mismatch), "\n")
print(both_present_term_mismatch, n=350)
cat("# seriousness mismatches where both present:", nrow(both_present_ser_mismatch), "\n")
print(both_present_ser_mismatch, n=300)
cat("# AE keys missing entirely in BLA:", nrow(missing_in_bla), "\n")
print(missing_in_bla, n=300)
cat("# AE keys missing entirely in EUA:", nrow(missing_in_eua), "\n")
print(missing_in_eua, n=300)

# --- 4) Nicely formatted HTML report (reordered; no 'missing in EUA' section) -
# --- Add BLA ADSL fields to report tables ------------------------------------
adsl_for_merge <- bla_adsl_data_filtered %>%
  transmute(
    SUBJID,
    ARM    = as.character(ARM),
    RANDDT = as.character(RANDDT),
    VAX101DT  = as.character(VAX101DT),
    VAX102DT  = as.character(VAX102DT)
  )

attach_adsl <- function(df) {
  df %>%
    left_join(adsl_for_merge, by = "SUBJID") %>%
    relocate(ARM, RANDDT, VAX101DT, VAX102DT, .after = SUBJID)
}

# Create report-ready versions (with ADSL columns placed after SUBJID)
both_present_date_mismatch_rpt <- attach_adsl(both_present_date_mismatch)
both_present_term_mismatch_rpt <- attach_adsl(both_present_term_mismatch)
both_present_ser_mismatch_rpt  <- attach_adsl(both_present_ser_mismatch)
missing_in_bla_rpt             <- attach_adsl(missing_in_bla)

suppressPackageStartupMessages({
  library(gt)
  library(htmltools)
})

dir.create("analysis", showWarnings = FALSE, recursive = TRUE)

# Helpers ----------------------------------------------------------------------
fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE)

metric_card <- function(label, value) {
  tags$div(
    class = "metric",
    tags$div(class = "label", label),
    tags$div(class = "value", fmt_int(value))
  )
}

make_gt_tbl <- function(df, title, subtitle = NULL, width_px = 1200) {
  if (is.null(df) || nrow(df) == 0) {
    df <- tibble::tibble(`No rows` = character())
  }
  gt(df) |>
    tab_header(
      title = md(title),
      subtitle = if (is.null(subtitle)) NULL else md(subtitle)
    ) |>
    opt_row_striping() |>
    tab_options(
      table.width = px(width_px),
      table.font.size = px(12),
      data_row.padding = px(2),
      heading.border.bottom.color = "black",
      column_labels.border.bottom.color = "black"
    ) |>
    fmt_missing(everything(), missing_text = "—") |>
    as_raw_html()
}

styles <- "
  :root { --ink:#111; --muted:#555; --line:#eaeaea; }
  * { box-sizing: border-box; }
  body { font-family: system-ui, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
         margin: 32px; color: var(--ink); }
  header h1 { margin: 0 0 4px; font-size: 28px; }
  .subtitle { color: var(--muted); margin: 0 0 24px; }
  section { margin-top: 28px; }
  .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 12px; margin-bottom: 8px; }
  .metric { border: 1px solid var(--line); border-radius: 12px; padding: 12px 16px;
            box-shadow: 0 1px 3px rgba(0,0,0,.05); background: white; }
  .metric .label { color: var(--muted); font-size: 12px; text-transform: uppercase; letter-spacing: .04em; }
  .metric .value { font-size: 20px; font-weight: 700; }
  hr { border: 0; border-top: 1px solid var(--line); margin: 24px 0; }
  footer { color: var(--muted); font-size: 12px; margin-top: 24px; }
"

# Convert logicals to readable flags
ae_cross_comp_print <- ae_cross_comp %>%
  mutate(across(
    c(date_match, term_match, ser_match),
    ~ case_when(
      is.na(.)       ~ NA_character_,
      . == TRUE      ~ "MATCH",
      . == FALSE     ~ "MISMATCH"
    )
  ))

# Overview metrics (dropped 'missing in EUA') ----------------------------------
title_txt <- "EUA vs BLA Adverse Events (AE) Comparison"
subtitle_txt <- paste0("Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"))

metrics <- list(
  metric_card("EUA AE keys (SUBJID+AESPID)", nrow(eua_ae_keys)),
  metric_card("BLA AE keys (SUBJID+AESPID)", nrow(bla_ae_keys)),
  metric_card("Date mismatches (both present)", nrow(both_present_date_mismatch)),
  metric_card("Term mismatches (both present)", nrow(both_present_term_mismatch)),
  metric_card("Seriousness mismatches (both present)", nrow(both_present_ser_mismatch)),
  metric_card("Keys present in EUA, missing in BLA", nrow(missing_in_bla))
  # intentionally removed: Keys present in BLA, missing in EUA
)

# --- 5) Per-ARM anomaly tests vs full ADSL randomized base --------------------

allowed_arms <- c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)")

# Normalize ARM and build the eligible base once, from ADSL only
eligible_subj_adsl <- bla_adsl_data_filtered %>%
  mutate(ARM = stringr::str_trim(as.character(ARM))) %>%
  filter(ARM %in% allowed_arms, !is.na(RANDDT)) %>%   # randomized & in target arms
  distinct(SUBJID, ARM)

# sanity: should equal your printed 46,480 (total across both arms)
message("Eligible randomized subjects (ADSL, two arms): ", nrow(eligible_subj_adsl))
print(eligible_subj_adsl %>% count(ARM), n=Inf)

fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < 1e-4, "<0.0001", sprintf("%.4f", p)))

run_arm_test_vs_adsl <- function(anom_df, label, eligible_base = eligible_subj_adsl) {
  # unique subjects with ≥1 AE that meets the anomaly
  affected_ids <- unique(anom_df$SUBJID)
  
  base <- eligible_base %>%
    mutate(affected = SUBJID %in% affected_ids)
  
  counts <- base %>%
    count(ARM, affected, name = "n") %>%
    tidyr::complete(ARM, affected, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = affected, values_from = n) %>%
    dplyr::rename(not_affected = `FALSE`, affected = `TRUE`) %>%
    mutate(total = affected + not_affected,
           affected_pct = ifelse(total > 0, round(100 * affected / total, 1), 0.0))
  
  # Build 2x2 (or r x 2) for test
  mat <- counts %>%
    select(ARM, affected, not_affected) %>%
    tibble::column_to_rownames("ARM") %>%
    as.matrix()
  
  # Choose test
  suppressWarnings({
    chi <- try(chisq.test(mat), silent = TRUE)
  })
  use_fisher <- FALSE
  use_sim    <- FALSE
  
  if (inherits(chi, "htest")) {
    exp_min <- min(chi$expected)
    if (nrow(mat) == 2 && ncol(mat) == 2 && exp_min < 5) {
      use_fisher <- TRUE
    } else if (exp_min < 5) {
      use_sim <- TRUE
    }
  } else {
    if (nrow(mat) == 2 && ncol(mat) == 2) use_fisher <- TRUE else use_sim <- TRUE
  }
  
  if (use_fisher) {
    tst <- fisher.test(mat)
    method <- "Fisher's exact test"
    pval   <- tst$p.value
    stat   <- NA
    df     <- NA
  } else if (use_sim) {
    tst <- chisq.test(mat, simulate.p.value = TRUE, B = 10000)
    method <- "Chi-square test (simulated p-value)"
    pval   <- tst$p.value
    stat   <- as.numeric(tst$statistic)
    df     <- as.numeric(tst$parameter)
  } else {
    tst <- chi
    method <- "Chi-square test"
    pval   <- tst$p.value
    stat   <- as.numeric(tst$statistic)
    df     <- as.numeric(tst$parameter)
  }
  
  # sanity: totals must equal the eligible base per ARM
  base_totals <- eligible_base %>% count(ARM, name = "eligible_total")
  counts <- counts %>%
    left_join(base_totals, by = "ARM")
  
  list(
    label   = label,
    method  = method,
    pval    = pval,
    stat    = stat,
    df      = df,
    counts  = counts
  )
}

date_test_adsl     <- run_arm_test_vs_adsl(both_present_date_mismatch, "Date mismatch (both present)")
term_test_adsl     <- run_arm_test_vs_adsl(both_present_term_mismatch, "Term mismatch (both present)")
ser_test_adsl      <- run_arm_test_vs_adsl(both_present_ser_mismatch,  "Seriousness mismatch (both present)")
miss_bla_test_adsl <- run_arm_test_vs_adsl(missing_in_bla,             "Present in EUA, missing in BLA")

# Helper to render a test table with method + p-value in the header
make_test_tbl <- function(test_res, width_px = 900) {
  hdr <- paste0(test_res$label, " — ", test_res$method, " (p = ", fmt_p(test_res$pval), ")")
  gt(test_res$counts) |>
    tab_header(title = md(hdr)) |>
    cols_label(
      ARM = "ARM",
      affected = "Affected",
      not_affected = "Not affected",
      total = "Total",
      affected_pct = "Affected (%)",
      eligible_total = "ADSL base (per ARM)"
    ) |>
    fmt_number(columns = c(affected_pct), decimals = 1) |>
    opt_row_striping() |>
    tab_options(
      table.width = px(width_px),
      table.font.size = px(12),
      data_row.padding = px(2),
      heading.border.bottom.color = "black",
      column_labels.border.bottom.color = "black"
    ) |>
    as_raw_html()
}

# -------------------------------------------------------------------------------



# Build the page ---------------------------------------------------------------
doc <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$title("EUA vs BLA AE Data Comparison"),
    tags$style(HTML(styles))
  ),
  tags$body(
    tags$header(
      tags$h1(title_txt),
      tags$p(class = "subtitle", subtitle_txt)
    ),
    
    tags$section(
      tags$h2("Overview"),
      tags$div(class = "grid", metrics)
    ),
    
    tags$section(
      tags$h2("Cross-dataset comparison (mismatch focus)"),
      HTML(make_gt_tbl(
        both_present_date_mismatch_rpt,
        "Date mismatches (EUA vs BLA)",
        "Only keys present in both, where collapsed date strings differ"
      )),
      HTML(make_gt_tbl(
        both_present_term_mismatch_rpt,
        "Term mismatches (EUA vs BLA)",
        "Only keys present in both, where collapsed AETERM strings differ"
      )),
      HTML(make_gt_tbl(
        both_present_ser_mismatch_rpt,
        "Seriousness mismatches (EUA vs BLA)",
        "Only keys present in both, where collapsed AESER strings differ"
      )),
      HTML(make_gt_tbl(
        missing_in_bla_rpt,
        "Present in EUA, missing in BLA",
        "Keys found in EUA with no corresponding key in BLA"
      ))
    ),
    
    tags$h2("Association of anomalies with ARM"),
    tags$p("Base: all randomized subjects in ADSL with ARM ∈ {Placebo, BNT162b2 Phase 2/3 (30 mcg)}. A subject is 'Affected' if they have ≥1 AE meeting the anomaly; otherwise they are 'Not affected'. Tests compare the affected proportions between ARMs."),
    HTML(make_test_tbl(date_test_adsl)),
    HTML(make_test_tbl(term_test_adsl)),
    HTML(make_test_tbl(ser_test_adsl)),
    HTML(make_test_tbl(miss_bla_test_adsl)),
    tags$footer(
      tags$hr(),
      tags$p("This HTML report is intended for scientific reporting; tables are generated via {gt}.")
    )
  )
)

htmltools::save_html(doc, file = "analysis/eua_vs_bla_aes_data.html", background = "white")
cat("Saved report to analysis/eua_vs_bla_aes_data.html\n")
# -------------------------------------------------------------------------------
