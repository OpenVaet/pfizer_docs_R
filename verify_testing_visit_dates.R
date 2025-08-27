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
