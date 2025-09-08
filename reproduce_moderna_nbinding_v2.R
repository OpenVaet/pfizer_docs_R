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
library(broom)

# Reading the input XPT files
adsl_data <- read_xpt("xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt")
adva_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.xpt')
mb_data   <- read_xpt('xpt_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.xpt')
symptoms  <- read_xpt('xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt')

# Process symptoms data
symptoms_filtered <- symptoms %>%
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE", AVALC == "Y")

# Expand symptoms to daily records
symptoms_days <- symptoms_filtered %>%
  mutate(
    ASTDT     = as.Date(ASTDT),
    AENDT     = as.Date(AENDT),
    end_date  = coalesce(AENDT, ASTDT),
    end_date  = if_else(end_date < ASTDT, ASTDT, end_date),
    n_days    = as.integer(end_date - ASTDT) + 1
  ) %>%
  filter(!is.na(ASTDT), n_days >= 1) %>%
  tidyr::uncount(n_days, .remove = FALSE, .id = "day_index") %>%
  mutate(SYMPTDATE = ASTDT + (day_index - 1)) %>%
  select(SUBJID, SYMPTDATE, PARAM)

# Get unique symptom days per subject
symptom_days_by_subject <- symptoms_days %>%
  select(SUBJID, SYMPTDATE) %>%
  distinct()

# Filtering and selecting necessary columns
adsl_data_filtered <- adsl_data %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, 
         RFICDT, V01DT, V02DT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)

# MB data (PCR tests)
mb_data_filtered <- mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), !is.na(VISIT),
         MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2',
                       'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(
    SUBJID    = substr(USUBJID, 15, 24),
    TESTDATE  = substr(MBDTC, 1, 10),
    TESTTYPE  = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2',
                       'PCR Local', 'PCR Central'),
    MBORRES   = stringr::str_trim(MBORRES),
    TESTRESULT = dplyr::recode(MBORRES,
                               'INDETERMINATE' = 'IND',
                               'POSITIVE'      = 'POS',
                               'NEGATIVE'      = 'NEG',
                               .default = MBORRES)
  ) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, TESTRESULT)

# ADVA data (N-binding antibody)
adva_data_filtered <- adva_data %>%
  filter(PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(
    TESTTYPE   = 'N-Binding',
    TESTDATE   = substr(ADT, 1, 10),
    AVALC      = stringr::str_trim(AVALC),
    TESTRESULT = dplyr::recode(AVALC,
                               'INDETERMINATE' = 'IND',
                               'POSITIVE'      = 'POS',
                               'NEGATIVE'      = 'NEG',
                               .default = AVALC)
  ) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, TESTRESULT)

# Combining both test datasets
testing_data <- rbind(mb_data_filtered, adva_data_filtered)

# --- Helpers -----------------------------------------------------------------
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  suppressWarnings(lubridate::ymd(x))
}

# Ensure dates are dates
subjects <- adsl_data_filtered %>%
  filter(ARM %in% c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)")) %>%
  mutate(across(c(UNBLNDDT, RANDDT, RFICDT, V01DT, V02DT,
                  VAX101DT, VAX102DT, VAX201DT, VAX202DT),
                to_date)) %>%
  # must have both doses
  filter(!is.na(VAX101DT), !is.na(VAX102DT))

tests_core <- testing_data %>%
  mutate(TESTDATE = ymd(TESTDATE)) %>%
  filter(TESTTYPE %in% c("PCR Central", "PCR Local", "N-Binding"))

# Join tests to subject-level (so each test row "knows" its subject's dose dates)
tj <- tests_core %>%
  inner_join(subjects, by = "SUBJID")

# --- Baseline at dose 1 (closest on or before VAX101DT) ----------------------
nb_d1 <- tj %>%
  filter(TESTTYPE == "N-Binding", TESTDATE <= VAX101DT) %>%
  group_by(SUBJID) %>%
  slice_max(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SUBJID,
            NBIND_VISIT_1_TESTDATE = TESTDATE,
            NBIND_VISIT_1_RESULT   = TESTRESULT)

pcr_d1 <- tj %>%
  filter(TESTTYPE %in% c("PCR Central", "PCR Local"), TESTDATE <= VAX101DT) %>%
  group_by(SUBJID) %>%
  slice_max(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SUBJID,
            PCR_VISIT_1_TESTDATE = TESTDATE,
            PCR_VISIT_1_RESULT   = TESTRESULT)

# --- Last tests on/before dose 2 ---------------
nb_d2 <- tj %>%
  filter(TESTTYPE == "N-Binding", TESTDATE <= VAX102DT) %>%
  group_by(SUBJID) %>%
  slice_max(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SUBJID,
            NBIND_VISIT_2_TESTDATE = TESTDATE,
            NBIND_VISIT_2_RESULT   = TESTRESULT)

pcr_d2 <- tj %>%
  filter(TESTTYPE %in% c("PCR Central", "PCR Local"), TESTDATE <= VAX102DT) %>%
  group_by(SUBJID) %>%
  slice_max(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SUBJID,
            PCR_VISIT_2_TESTDATE = TESTDATE,
            PCR_VISIT_2_RESULT   = TESTRESULT)

# --- Any evidence of infection before dose 2? --------------------------------
pre_dose2 <- tj %>%
  filter(TESTDATE <= VAX102DT, TESTTYPE %in% c("N-Binding","PCR Central")) %>%
  group_by(SUBJID) %>%
  summarise(ANY_POS_BEFORE_DOSE2 = any(TESTRESULT == "POS"), .groups = "drop")

# --- NEW: Function to check if PCR date has symptoms within ±4 days ----------
has_symptoms_near_date <- function(subjid, test_date, symptom_df, window_days = 4) {
  subj_symptoms <- symptom_df %>%
    filter(SUBJID == subjid)
  
  if (nrow(subj_symptoms) == 0) return(FALSE)
  
  any(abs(as.integer(subj_symptoms$SYMPTDATE - test_date)) <= window_days)
}

# --- First qualifying SYMPTOMATIC positive PCR (>=14 days post dose 2) -------
# First get all positive PCR tests >=14 days post dose 2
pos_pcr_candidates <- tj %>%
  mutate(DAY14 = VAX102DT + days(14)) %>%
  filter(TESTTYPE %in% c("PCR Central", "PCR Local"), 
         TESTRESULT == "POS", 
         TESTDATE >= DAY14) %>%
  select(SUBJID, TESTDATE, TESTTYPE, DAY14)

# Check each positive PCR for associated symptoms
pos_pcr_with_symptoms <- pos_pcr_candidates %>%
  rowwise() %>%
  mutate(HAS_SYMPTOMS = has_symptoms_near_date(SUBJID, TESTDATE, 
                                                 symptom_days_by_subject, 
                                                 window_days = 4)) %>%
  ungroup() %>%
  filter(HAS_SYMPTOMS)

# Get first symptomatic positive PCR per subject
first_pos <- pos_pcr_with_symptoms %>%
  group_by(SUBJID) %>%
  slice_min(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SUBJID,
            FIRST_POS_DATE     = TESTDATE,
            FIRST_POS_TESTTYPE = TESTTYPE)

# --- First N-binding >=4 days after that positive PCR ------------------------
post_pos_nb <- first_pos %>%
  inner_join(tj %>% filter(TESTTYPE == "N-Binding"),
             by = "SUBJID") %>%
  filter(TESTDATE >= FIRST_POS_DATE + days(4)) %>%
  group_by(SUBJID, FIRST_POS_DATE, FIRST_POS_TESTTYPE) %>%
  slice_min(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(SUBJID,
            POST_POS_N_BINDING_DATE   = TESTDATE,
            POST_POS_N_BINDING_RESULT = TESTRESULT)

# --- Assemble flattened dataframe -------------------------------------------
flattened <- subjects %>%
  select(SUBJID, ARM, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT,
         VAX101DT, VAX102DT, VAX201DT) %>%
  left_join(nb_d1,  by = "SUBJID") %>%
  left_join(pcr_d1, by = "SUBJID") %>%
  left_join(nb_d2,  by = "SUBJID") %>%
  left_join(pcr_d2, by = "SUBJID") %>%
  left_join(pre_dose2, by = "SUBJID") %>%
  left_join(first_pos, by = "SUBJID") %>%
  left_join(post_pos_nb, by = "SUBJID") %>%
  mutate(
    ANY_POS_BEFORE_DOSE2 = coalesce(ANY_POS_BEFORE_DOSE2, FALSE),
    NEG_AT_DOSE_1 = case_when(
      is.na(NBIND_VISIT_1_RESULT) | is.na(PCR_VISIT_1_RESULT) ~ NA,
      TRUE ~ (NBIND_VISIT_1_RESULT == "NEG" & PCR_VISIT_1_RESULT == "NEG")
    ),
    NEG_AT_DOSE_2 = case_when(
      is.na(NBIND_VISIT_2_RESULT) | is.na(PCR_VISIT_2_RESULT) ~ NA,
      TRUE ~ (NBIND_VISIT_2_RESULT == "NEG" & PCR_VISIT_2_RESULT == "NEG")
    )
  )

print(flattened)

# --- Apply the inclusion criteria ("Moderna-like") ---------------------------
eligible_subjects <- flattened %>%
  filter(
    ARM %in% c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)"),
    NEG_AT_DOSE_1 %in% TRUE,                              # baseline negative
    !ANY_POS_BEFORE_DOSE2,                                # no prior infection
    !is.na(FIRST_POS_DATE),                               # has symptomatic PCR+
    !is.na(POST_POS_N_BINDING_DATE)                       # has follow-up N-binding
  )

# Print summary of eligible subjects
cat("\n=== Eligible Subjects Summary ===\n")
cat("Total eligible subjects:", nrow(eligible_subjects), "\n")
eligible_subjects %>%
  group_by(ARM) %>%
  summarise(n = n()) %>%
  print()

# --- Analysis ----------------------------------------------------------------
anl <- eligible_subjects %>%
  mutate(
    FIRST_POS_DATE          = as.Date(FIRST_POS_DATE),
    POST_POS_N_BINDING_DATE = as.Date(POST_POS_N_BINDING_DATE),
    POST_POS_N_BINDING_RESULT = str_trim(POST_POS_N_BINDING_RESULT),
    delay_days = as.integer(POST_POS_N_BINDING_DATE - FIRST_POS_DATE)
  ) %>%
  filter(!is.na(POST_POS_N_BINDING_DATE), !is.na(POST_POS_N_BINDING_RESULT))

# Arm-wise summaries
summ_by_arm <- anl %>%
  group_by(ARM) %>%
  summarise(
    n_subjects              = n(),
    n_positive              = sum(POST_POS_N_BINDING_RESULT == "POS"),
    pct_positive            = 100 * n_positive / n_subjects,
    pct_pos_ci_low          = 100 * prop.test(n_positive, n_subjects)$conf.int[1],
    pct_pos_ci_high         = 100 * prop.test(n_positive, n_subjects)$conf.int[2],
    delay_median            = median(delay_days, na.rm = TRUE),
    delay_q1                = quantile(delay_days, 0.25, na.rm = TRUE),
    delay_q3                = quantile(delay_days, 0.75, na.rm = TRUE),
    delay_mean              = mean(delay_days, na.rm = TRUE),
    delay_sd                = sd(delay_days, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Results by Treatment Arm ===\n")
print(summ_by_arm, n = 99)

# Statistical comparisons
tab_pos <- table(anl$ARM, anl$POST_POS_N_BINDING_RESULT == "POS")
cat("\n=== Contingency Table ===\n")
print(tab_pos)

pos_test <- chisq.test(tab_pos)
cat("\n=== Chi-square Test ===\n")
print(pos_test)

# Risk difference
arm_counts <- anl %>%
  group_by(ARM) %>%
  summarise(n = n(), pos = sum(POST_POS_N_BINDING_RESULT == "POS"), .groups = "drop") %>%
  arrange(match(ARM, c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")))

if (nrow(arm_counts) == 2) {
  p1 <- arm_counts$pos[1] / arm_counts$n[1]
  p0 <- arm_counts$pos[2] / arm_counts$n[2]
  rd <- p1 - p0
  se_rd <- sqrt(p1*(1-p1)/arm_counts$n[1] + p0*(1-p0)/arm_counts$n[2])
  ci_rd <- rd + c(-1,1) * qnorm(0.975) * se_rd
  
  cat("\n=== Risk Difference ===\n")
  cat("BNT162b2 - Placebo: ", round(rd*100, 1), "%\n")
  cat("95% CI: (", round(ci_rd[1]*100, 1), "%, ", round(ci_rd[2]*100, 1), "%)\n")
}

# Delay comparison
cat("\n=== Wilcoxon Test for Delay Days ===\n")
wilcox_result <- wilcox.test(delay_days ~ ARM, data = anl)
print(wilcox_result)