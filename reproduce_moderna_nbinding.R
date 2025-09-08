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

# Filtering and selecting necessary columns
adsl_data_filtered <- adsl_data %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT, V01DT, V02DT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)
print(adsl_data_filtered)

# MB data
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
print(mb_data_filtered)

# ADVA data
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

print(adva_data_filtered)

# Combining both datasets
testing_data <- rbind(mb_data_filtered, adva_data_filtered)
print(testing_data)

# -------------------------------------------------------------------------------

# Merging ADSL and MB data
merged_mb_data <- merge(adsl_data_filtered, mb_data_filtered, by = "SUBJID")
print(merged_mb_data)

# Merging ADSL and ADVA data
merged_adva_data <- merge(adsl_data_filtered, adva_data_filtered, by = "SUBJID")

# Combining both datasets
final_data <- rbind(merged_mb_data, merged_adva_data)

print(final_data)

# -------------------------------------------------------------------------------



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

print(subjects)

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

# --- Last tests on/before dose 2 (optional, for NEG_AT_DOSE_2) ---------------
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

# --- First qualifying positive PCR (>=14 days post dose 2) -------------------
first_pos <- tj %>%
  mutate(DAY14 = VAX102DT + days(14)) %>%
  filter(TESTTYPE %in% c("PCR Central", "PCR Local"), TESTRESULT == "POS", TESTDATE >= DAY14) %>%
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
      is.na(NBIND_VISIT_1_RESULT) | is.na(PCR_VISIT_1_RESULT) ~ NA, # missing baseline data
      TRUE ~ (NBIND_VISIT_1_RESULT == "NEG" & PCR_VISIT_1_RESULT == "NEG")
    ),
    NEG_AT_DOSE_2 = case_when(
      is.na(NBIND_VISIT_2_RESULT) | is.na(PCR_VISIT_2_RESULT) ~ NA,
      TRUE ~ (NBIND_VISIT_2_RESULT == "NEG" & PCR_VISIT_2_RESULT == "NEG")
    )
  )

# This is the flattened dataframe containing everything we need
flattened %>% glimpse()

# --- Apply the inclusion criteria ("Moderna-like") ---------------------------
eligible_subjects <- flattened %>%
  filter(
    ARM %in% c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)"), # (1) already ensured
    NEG_AT_DOSE_1 %in% TRUE,                              # (2) baseline N-binding & PCR both NEG at/≤ dose 1
    !ANY_POS_BEFORE_DOSE2,                                # (4) no evidence of infection before dose 2
    !is.na(FIRST_POS_DATE),                               # (5a) first POS PCR ≥14d after dose 2
    !is.na(POST_POS_N_BINDING_DATE)                       # (5b) has N-binding ≥4d after that PCR
  )

# Peek a few rows you care about
eligible_subjects %>%
  select(SUBJID, ARM, VAX101DT, VAX102DT,
         NBIND_VISIT_1_TESTDATE, NBIND_VISIT_1_RESULT,
         PCR_VISIT_1_TESTDATE,   PCR_VISIT_1_RESULT,
         FIRST_POS_DATE, FIRST_POS_TESTTYPE,
         POST_POS_N_BINDING_DATE, POST_POS_N_BINDING_RESULT) %>%
  arrange(FIRST_POS_DATE) %>%
  print(n = 400)

# -------------------------------------------------------------------------------


# 1) Prepare analysis frame ----------------------------------------------------
anl <- eligible_subjects %>%
  mutate(
    FIRST_POS_DATE          = as.Date(FIRST_POS_DATE),
    POST_POS_N_BINDING_DATE = as.Date(POST_POS_N_BINDING_DATE),
    POST_POS_N_BINDING_RESULT = str_trim(POST_POS_N_BINDING_RESULT),
    delay_days = as.integer(POST_POS_N_BINDING_DATE - FIRST_POS_DATE)
  ) %>%
  # Keep subjects that actually have a qualifying N-binding measurement
  filter(!is.na(POST_POS_N_BINDING_DATE), !is.na(POST_POS_N_BINDING_RESULT))

# 2) Arm-wise summaries (positivity and delay stats) --------------------------
summ_by_arm <- anl %>%
  group_by(ARM) %>%
  summarise(
    n_subjects              = n(),
    n_positive              = sum(POST_POS_N_BINDING_RESULT == "POS"),
    pct_positive            = 100 * n_positive / n_subjects,
    # 95% CI for proportion (Wilson via prop.test)
    pct_pos_ci_low          = 100 * prop.test(n_positive, n_subjects)$conf.int[1],
    pct_pos_ci_high         = 100 * prop.test(n_positive, n_subjects)$conf.int[2],
    
    # Delay stats
    n_with_delay            = sum(!is.na(delay_days)),
    delay_median            = median(delay_days, na.rm = TRUE),
    delay_q1                = quantile(delay_days, 0.25, na.rm = TRUE),
    delay_q3                = quantile(delay_days, 0.75, na.rm = TRUE),
    delay_mean              = mean(delay_days, na.rm = TRUE),
    delay_sd                = sd(delay_days, na.rm = TRUE),
    delay_min               = min(delay_days, na.rm = TRUE),
    delay_max               = max(delay_days, na.rm = TRUE),
    .groups = "drop"
  )

print(summ_by_arm, n = 99)

# 3) Overall summary (optional) -----------------------------------------------
summ_overall <- anl %>%
  summarise(
    n_subjects   = n(),
    n_positive   = sum(POST_POS_N_BINDING_RESULT == "POS"),
    pct_positive = 100 * n_positive / n_subjects,
    pct_pos_ci_low  = 100 * prop.test(n_positive, n_subjects)$conf.int[1],
    pct_pos_ci_high = 100 * prop.test(n_positive, n_subjects)$conf.int[2],
    delay_median = median(delay_days, na.rm = TRUE),
    delay_q1     = quantile(delay_days, 0.25, na.rm = TRUE),
    delay_q3     = quantile(delay_days, 0.75, na.rm = TRUE),
    delay_mean   = mean(delay_days, na.rm = TRUE),
    delay_sd     = sd(delay_days, na.rm = TRUE),
    delay_min    = min(delay_days, na.rm = TRUE),
    delay_max    = max(delay_days, na.rm = TRUE)
  )

print(summ_overall)

# 4) Between-arm comparisons ---------------------------------------------------
# 4a) % anti-N positive difference (chi-square test of independence)
tab_pos <- table(anl$ARM, anl$POST_POS_N_BINDING_RESULT == "POS")
print(tab_pos)

pos_test <- chisq.test(tab_pos)
pos_test

# Risk difference and CI (simple Wald; for publication use better CI if needed)
arm_counts <- anl %>%
  group_by(ARM) %>%
  summarise(n = n(), pos = sum(POST_POS_N_BINDING_RESULT == "POS"), .groups = "drop")

# Ensure order: Vaccine first, then Placebo (adjust if you prefer other order)
arm_counts <- arm_counts %>%
  arrange(match(ARM, c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")))

p1 <- arm_counts$pos[1] / arm_counts$n[1]
p0 <- arm_counts$pos[2] / arm_counts$n[2]
rd <- p1 - p0
se_rd <- sqrt(p1*(1-p1)/arm_counts$n[1] + p0*(1-p0)/arm_counts$n[2])
ci_rd <- rd + c(-1,1) * qnorm(0.975) * se_rd
data.frame(
  contrast = "BNT162b2 - Placebo",
  risk_diff = rd,
  rd_95ci_lo = ci_rd[1],
  rd_95ci_hi = ci_rd[2]
)

# 4b) Delay comparison (Wilcoxon rank-sum)
wilcox.test(delay_days ~ ARM, data = anl)



