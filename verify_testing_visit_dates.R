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

# Reading the input XPT files
bla_adsl_data <- read_xpt("xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt")
bla_adva_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.xpt')
bla_mb_data   <- read_xpt('xpt_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.xpt')

# Filtering and selecting necessary columns
bla_adsl_data_filtered <- bla_adsl_data %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT, V01DT, V02DT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)
print(bla_adsl_data_filtered)

# Processing MB data
bla_mb_data_filtered <- bla_mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), !is.na(VISIT), MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2', 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(SUBJID = substr(USUBJID, 15, 24),
         TESTDATE = substr(MBDTC, 1, 10),
         TESTRESULT = MBORRES,
         TESTTYPE = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', 'PCR Local', 'PCR Central'),
         MBORRES = case_when(
           MBORRES == 'INDETERMINATE' ~ 'IND',
           MBORRES == 'POSITIVE' ~ 'POS',
           MBORRES == 'NEGATIVE' ~ 'NEG',
           TRUE ~ MBORRES
         )) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, TESTRESULT)
print(bla_mb_data_filtered)

# Processing ADVA data
bla_adva_data_filtered <- bla_adva_data %>%
  filter(PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(TESTTYPE = 'N-Binding',
         TESTRESULT = AVALC,
         TESTDATE = substr(ADT, 1, 10)) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, TESTRESULT)
print(bla_adva_data_filtered)

# Combining both datasets
bla_testing_data <- rbind(bla_mb_data_filtered, bla_adva_data_filtered)
print(bla_testing_data)

# -------------------------------------------------------------------------------

# Merging ADSL and MB data
bla_merged_mb_data <- merge(bla_adsl_data_filtered, bla_mb_data_filtered, by = "SUBJID")
print(bla_merged_mb_data)

# Merging ADSL and ADVA data
bla_merged_adva_data <- merge(bla_adsl_data_filtered, bla_adva_data_filtered, by = "SUBJID")

# Combining both datasets
bla_final_data <- rbind(bla_merged_mb_data, bla_merged_adva_data)

print(bla_final_data)

# -------------------------------------------------------------------------------

# Reading the input XPT files
eua_adsl_data <- read_xpt("eua_data/xpt_data/FDA-CBER-2021-5683-1226624-1227706_27034_S1_M5_c4591001-ia efficacy-A-adsl.xpt")
eua_adva_data <- read_xpt('eua_data/xpt_data/FDA-CBER-2021-5683-1495707-1496004_27034_S1_M5_c4591001-safety-fa-eff-A-adva.xpt')
eua_mb_data   <- read_xpt('eua_data/xpt_data/FDA-CBER-2021-5683-1559614-1561992_27034_S1_M5_c4591001-safety-fa-eff-S-mb.xpt')

# Filtering and selecting necessary columns
eua_adsl_data_filtered <- eua_adsl_data %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, RANDDT, RFICDT, V01DT, V02DT, VAX101DT, VAX102DT)
print(eua_adsl_data_filtered)

# Processing MB data
eua_mb_data_filtered <- eua_mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), !is.na(VISIT), MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2', 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(SUBJID = substr(USUBJID, 15, 24),
         TESTDATE = substr(MBDTC, 1, 10),
         TESTRESULT = MBORRES,
         TESTTYPE = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', 'PCR Local', 'PCR Central'),
         MBORRES = case_when(
           MBORRES == 'INDETERMINATE' ~ 'IND',
           MBORRES == 'POSITIVE' ~ 'POS',
           MBORRES == 'NEGATIVE' ~ 'NEG',
           TRUE ~ MBORRES
         )) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, TESTRESULT)
print(eua_mb_data_filtered)

# Processing ADVA data
eua_adva_data_filtered <- eua_adva_data %>%
  filter(PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(TESTTYPE = 'N-Binding',
         TESTRESULT = AVALC,
         TESTDATE = substr(ADT, 1, 10)) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, TESTRESULT)
print(eua_adva_data_filtered)

# Combining both datasets
eua_testing_data <- rbind(eua_mb_data_filtered, eua_adva_data_filtered)
print(eua_testing_data)

# -------------------------------------------------------------------------------

# --- 1) List unique VISIT values in each testing dataset ----------------------

eua_visits <- eua_testing_data %>% distinct(VISIT) %>% arrange(VISIT)
bla_visits <- bla_testing_data %>% distinct(VISIT) %>% arrange(VISIT)

print(eua_visits)
print(bla_visits)

# If you prefer a simple vector:
# eua_visits_vec <- eua_visits %>% pull(VISIT)
# bla_visits_vec <- bla_visits %>% pull(VISIT)


# --- 2) Check TESTDATE consistency per SUBJID & VISIT -------------------------
# (a) Within each dataset (are there multiple TESTDATEs for the same SUBJID+VISIT?)
check_within <- function(df, label) {
  df %>%
    group_by(SUBJID, VISIT) %>%
    summarise(
      n_dates = n_distinct(TESTDATE),
      dates   = paste(sort(unique(TESTDATE)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(dataset = label) %>%
    relocate(dataset, .before = SUBJID) %>%
    arrange(desc(n_dates), SUBJID, VISIT)
}

eua_within <- check_within(eua_testing_data, "EUA")
bla_within <- check_within(bla_testing_data, "BLA")

# Rows with >1 unique TESTDATE within the same SUBJID & VISIT:
eua_inconsistent <- eua_within %>% filter(n_dates > 1)
bla_inconsistent <- bla_within %>% filter(n_dates > 1)

print(eua_inconsistent)
print(bla_inconsistent)


# (b) Across datasets (does EUA have the same TESTDATE as BLA for each SUBJID+VISIT?)
# Collapse to one (sorted, unique) TESTDATE-string per SUBJID+VISIT in each dataset
eua_dates <- eua_within %>%
  select(SUBJID, VISIT, TESTDATE_EUA = dates)

bla_dates <- bla_within %>%
  select(SUBJID, VISIT, TESTDATE_BLA = dates)

# Compare
cross_comp <- full_join(eua_dates, bla_dates, by = c("SUBJID", "VISIT")) %>%
  mutate(match = TESTDATE_EUA == TESTDATE_BLA)

# Useful slices:
both_present_mismatch <- cross_comp %>% 
  filter(!is.na(TESTDATE_EUA), !is.na(TESTDATE_BLA), !match)

missing_in_bla <- cross_comp %>% 
  filter(!is.na(TESTDATE_EUA),  is.na(TESTDATE_BLA))

missing_in_eua <- cross_comp %>% 
  filter(is.na(TESTDATE_EUA),  !is.na(TESTDATE_BLA))

# Inspect results
print(cross_comp %>% arrange(SUBJID, VISIT))
cat("# mismatches where both present:", nrow(both_present_mismatch), "\n")
print(both_present_mismatch, n=300)
cat("# pairs missing in BLA:", nrow(missing_in_bla), "\n")
print(missing_in_bla, n=300)
cat("# pairs missing in EUA:", nrow(missing_in_eua), "\n")

# -------------------------------------------------------------------------------

# ===== Result comparison EUA vs BLA (by SUBJID + VISIT + TESTTYPE) ============

# Normalize test result strings lightly to reduce trivial mismatches
normalize_result <- function(x) {
  x2 <- toupper(trimws(as.character(x)))
  dplyr::case_when(
    x2 == "POSITIVE"       ~ "POS",
    x2 == "NEGATIVE"       ~ "NEG",
    x2 == "INDETERMINATE"  ~ "IND",
    TRUE ~ x2
  )
}

eua_res <- eua_testing_data %>%
  mutate(TESTRESULT_N = normalize_result(TESTRESULT)) %>%
  group_by(SUBJID, VISIT, TESTTYPE) %>%
  summarise(
    RESULT_EUA = paste(sort(unique(TESTRESULT_N)), collapse = "; "),
    DATES_EUA  = paste(sort(unique(TESTDATE)),     collapse = "; "),
    .groups = "drop"
  )

bla_res <- bla_testing_data %>%
  mutate(TESTRESULT_N = normalize_result(TESTRESULT)) %>%
  group_by(SUBJID, VISIT, TESTTYPE) %>%
  summarise(
    RESULT_BLA = paste(sort(unique(TESTRESULT_N)), collapse = "; "),
    DATES_BLA  = paste(sort(unique(TESTDATE)),     collapse = "; "),
    .groups = "drop"
  )

results_cross <- full_join(eua_res, bla_res, by = c("SUBJID","VISIT","TESTTYPE")) %>%
  mutate(
    result_match = RESULT_EUA == RESULT_BLA,
    date_match   = DATES_EUA  == DATES_BLA
  ) %>%
  arrange(SUBJID, VISIT, TESTTYPE)

# Anomalies of interest for the report
res_mismatch_both_present <- results_cross %>%
  filter(!is.na(RESULT_EUA), !is.na(RESULT_BLA), !result_match)

res_missing_in_bla <- results_cross %>%
  filter(!is.na(RESULT_EUA),  is.na(RESULT_BLA))

cat("# result mismatches where both present:", nrow(res_mismatch_both_present), "\n")
print(res_mismatch_both_present, n = 300)
cat("# results present in EUA, missing in BLA:", nrow(res_missing_in_bla), "\n")
print(res_missing_in_bla, n = 300)


# ===== Attach BLA ADSL context to the tables ==================================

adsl_for_merge <- bla_adsl_data_filtered %>%
  transmute(
    SUBJID,
    ARM    = as.character(ARM),
    RANDDT = as.character(RANDDT),
    V01DT  = as.character(V01DT),
    V02DT  = as.character(V02DT)
  )

attach_adsl <- function(df) {
  df %>%
    left_join(adsl_for_merge, by = "SUBJID") %>%
    relocate(ARM, RANDDT, V01DT, V02DT, .after = SUBJID)
}

res_mismatch_both_present_rpt <- attach_adsl(res_mismatch_both_present)
res_missing_in_bla_rpt        <- attach_adsl(res_missing_in_bla)


# ===== Per-ARM tests vs full randomized ADSL base (two arms) ==================

allowed_arms <- c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)")

eligible_subj_adsl <- bla_adsl_data_filtered %>%
  mutate(ARM = stringr::str_trim(as.character(ARM))) %>%
  filter(ARM %in% allowed_arms, !is.na(RANDDT)) %>%    # randomized & target arms
  distinct(SUBJID, ARM)

fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < 1e-4, "<0.0001", sprintf("%.4f", p)))

run_arm_test_vs_adsl <- function(anom_df, label, eligible_base = eligible_subj_adsl) {
  affected_ids <- unique(anom_df$SUBJID)

  counts <- eligible_base %>%
    mutate(affected = SUBJID %in% affected_ids) %>%
    count(ARM, affected, name = "n") %>%
    tidyr::complete(ARM, affected, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = affected, values_from = n) %>%
    rename(not_affected = `FALSE`, affected = `TRUE`) %>%
    mutate(total = affected + not_affected,
           affected_pct = ifelse(total > 0, round(100 * affected / total, 1), 0))

  mat <- counts %>%
    select(ARM, affected, not_affected) %>%
    tibble::column_to_rownames("ARM") %>%
    as.matrix()

  suppressWarnings({ chi <- try(chisq.test(mat), silent = TRUE) })
  use_fisher <- FALSE; use_sim <- FALSE
  if (inherits(chi, "htest")) {
    exp_min <- min(chi$expected)
    if (nrow(mat) == 2 && ncol(mat) == 2 && exp_min < 5) use_fisher <- TRUE
    else if (exp_min < 5) use_sim <- TRUE
  } else {
    if (nrow(mat) == 2 && ncol(mat) == 2) use_fisher <- TRUE else use_sim <- TRUE
  }

  if (use_fisher) {
    tst <- fisher.test(mat); method <- "Fisher's exact test"; pval <- tst$p.value; stat <- NA; df <- NA
  } else if (use_sim) {
    tst <- chisq.test(mat, simulate.p.value = TRUE, B = 10000)
    method <- "Chi-square test (simulated p-value)"; pval <- tst$p.value
    stat <- as.numeric(tst$statistic); df <- as.numeric(tst$parameter)
  } else {
    tst <- chi
    method <- "Chi-square test"; pval <- tst$p.value
    stat <- as.numeric(tst$statistic); df <- as.numeric(tst$parameter)
  }

  base_totals <- eligible_base %>% count(ARM, name = "ADSL_base")
  counts <- counts %>% left_join(base_totals, by = "ARM")

  list(label = label, method = method, pval = pval, stat = stat, df = df, counts = counts)
}

# Run tests for the two anomalies we report
res_change_test_adsl  <- run_arm_test_vs_adsl(res_mismatch_both_present, "Result mismatch (both present)")
miss_bla_test_adsl    <- run_arm_test_vs_adsl(res_missing_in_bla,        "Present in EUA, missing in BLA")


# ===== HTML report ============================================================

suppressPackageStartupMessages({ library(gt); library(htmltools) })
dir.create("analysis", showWarnings = FALSE, recursive = TRUE)

make_gt_tbl <- function(df, title, subtitle = NULL, width_px = 1200) {
  if (is.null(df) || nrow(df) == 0) df <- tibble::tibble(`No rows` = character())
  gt(df) |>
    tab_header(title = md(title), subtitle = if (is.null(subtitle)) NULL else md(subtitle)) |>
    opt_row_striping() |>
    tab_options(table.width = px(width_px), table.font.size = px(12),
                data_row.padding = px(2),
                heading.border.bottom.color = "black",
                column_labels.border.bottom.color = "black") |>
    fmt_missing(everything(), missing_text = "—") |>
    as_raw_html()
}

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
      ADSL_base = "ADSL base (per ARM)"
    ) |>
    fmt_number(columns = c(affected_pct), decimals = 1) |>
    opt_row_striping() |>
    tab_options(table.width = px(width_px), table.font.size = px(12),
                data_row.padding = px(2),
                heading.border.bottom.color = "black",
                column_labels.border.bottom.color = "black") |>
    as_raw_html()
}

styles <- "
  :root { --ink:#111; --muted:#555; --line:#eaeaea; }
  * { box-sizing: border-box; }
  body { font-family: system-ui, Segoe UI, Roboto, Helvetica, Arial, sans-serif; margin: 32px; color: var(--ink); }
  header h1 { margin: 0 0 4px; font-size: 28px; }
  .subtitle { color: var(--muted); margin: 0 0 24px; }
  section { margin-top: 28px; }
  .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 12px; margin-bottom: 8px; }
  .metric { border: 1px solid var(--line); border-radius: 12px; padding: 12px 16px; box-shadow: 0 1px 3px rgba(0,0,0,.05); background: white; }
  .metric .label { color: var(--muted); font-size: 12px; text-transform: uppercase; letter-spacing: .04em; }
  .metric .value { font-size: 20px; font-weight: 700; }
  hr { border: 0; border-top: 1px solid var(--line); margin: 24px 0; }
  footer { color: var(--muted); font-size: 12px; margin-top: 24px; }
"

title_txt <- "EUA vs BLA: Testing Results Comparison"
subtitle_txt <- paste0("Generated on ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"))

overview_metrics <- list(
  tags$div(class="metric", tags$div(class="label","Result mismatches (both present)"),
           tags$div(class="value", nrow(res_mismatch_both_present))),
  tags$div(class="metric", tags$div(class="label","Results present in EUA, missing in BLA"),
           tags$div(class="value", nrow(res_missing_in_bla))),
  tags$div(class="metric", tags$div(class="label","Eligible randomized subjects (ADSL, two arms)"),
           tags$div(class="value", nrow(eligible_subj_adsl)))
)

doc <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$title("EUA vs BLA Testing Results"),
    tags$style(HTML(styles))
  ),
  tags$body(
    tags$header(tags$h1(title_txt), tags$p(class = "subtitle", subtitle_txt)),

    tags$section(tags$h2("Overview"), tags$div(class="grid", overview_metrics)),

    tags$section(
      tags$h2("Cross-dataset comparison (result focus)"),
      HTML(make_gt_tbl(
        res_mismatch_both_present_rpt,
        "Result mismatches (EUA vs BLA)",
        "Keys with both EUA and BLA present where collapsed TESTRESULT strings differ"
      )),
      HTML(make_gt_tbl(
        res_missing_in_bla_rpt,
        "Present in EUA, missing in BLA",
        "Keys found in EUA with no corresponding key in BLA"
      ))
    ),

    tags$section(
      tags$h2("Association of result anomalies with ARM"),
      tags$p("Base: all randomized subjects in ADSL with ARM ∈ {Placebo, BNT162b2 Phase 2/3 (30 mcg)}. A subject is 'Affected' if they have ≥1 key with the anomaly; otherwise 'Not affected'. Tests compare affected proportions between ARMs."),
      HTML(make_test_tbl(res_change_test_adsl)),
      HTML(make_test_tbl(miss_bla_test_adsl))
    ),

    tags$footer(tags$hr(), tags$p("HTML report for scientific use; tables via {gt}."))
  )
)

htmltools::save_html(doc, file = "analysis/eua_vs_bla_testing_results.html", background = "white")
cat("Saved report to analysis/eua_vs_bla_testing_results.html\n")
