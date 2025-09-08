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
  filter(!is.na(MBDTC), !is.na(MBORRES), !is.na(VISIT), # , VISIT != 'V1_DAY1_VAX1_L'
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

# Keep only the two arms of interest
arms_of_interest <- adsl_data_filtered %>%
  filter(ARM %in% c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)")) %>%
  select(SUBJID, ARM)

# Positive PCRs (Central or Local), dated
pcr_pos <- mb_data_filtered %>%
  filter(TESTTYPE %in% c("PCR Central", "PCR Local"),
         TESTRESULT == "POS") %>%
  mutate(TESTDATE = ymd(TESTDATE)) %>%
  inner_join(arms_of_interest, by = "SUBJID")

# 1) First positive PCR per subject (across Central/Local)
first_pos_per_subject <- pcr_pos %>%
  arrange(SUBJID, TESTDATE) %>%
  group_by(SUBJID, ARM) %>%
  slice_min(TESTDATE, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    SUBJID, ARM,
    first_pos_date  = TESTDATE,
    first_pos_type  = TESTTYPE,
    first_pos_visit = VISIT
  )

print(first_pos_per_subject, n = 10)

# 2) Earliest positive within each arm (overall, who & when)
first_pos_in_each_arm <- first_pos_per_subject %>%
  group_by(ARM) %>%
  slice_min(first_pos_date, with_ties = FALSE) %>%
  ungroup()

print(first_pos_in_each_arm)

# Earliest POS per arm *by* PCR type (Central vs Local):
first_pos_in_each_arm_by_type <- pcr_pos %>%
  group_by(ARM, TESTTYPE) %>%
  slice_min(TESTDATE, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    ARM, TESTTYPE,
    earliest_pos_date = TESTDATE,
    SUBJID, VISIT
  )

print(first_pos_in_each_arm_by_type)
