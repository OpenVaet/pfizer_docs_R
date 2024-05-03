library(haven)
library(dplyr)
library(lubridate)
library(furrr)
library(stringr)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)

symptoms_file <- 'xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt'

# Reads the primary XPT files
symptoms <- read_xpt(symptoms_file)
print(colnames(symptoms))
print(symptoms)

# Filters the symptoms data to only include subjects in the randomized population
symptoms <- symptoms[symptoms$SUBJID %in% randomized_pop$SUBJID, ]
symptoms <- symptoms %>%
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE")
symptoms <- symptoms %>%
  filter(AVALC == "Y")
print(colnames(symptoms))
print(symptoms)

# Sustains only columns required for the current analysis.
symptoms_selected <- symptoms[c("SUBJID", "SITEID", "ARM", "AVISIT", "PARAM", "VISITNUM", "VISIT", "ADT", "ASTDT")]
print(symptoms_selected)

# Loads the supplementary file.
symptoms_sup_file <- 'xpt_data/FDA-CBER-2021-5683-0539816-0593326-125742_S1_M5_c4591001-01-S-Supp-D-face.xpt'
symptoms_sup <- read_xpt(symptoms_sup_file)
symptoms_sup <- symptoms_sup %>%
  filter(FATEST == "First Symptom Date")

# Filters the symptoms sup data to only include subjects in the randomized population
symptoms_sup <- symptoms_sup %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"))
symptoms_sup <- symptoms_sup[symptoms_sup$SUBJID %in% randomized_pop$SUBJID, ]
print(colnames(symptoms_sup))
print(symptoms_sup)

# Sustains only columns required for the current analysis (date of first symptoms).
symptoms_sup_selected <- symptoms_sup[c("SUBJID", "FAORRES", "VISIT")]
print(symptoms_sup_selected)

# Creates a dataframe subjects_sympto_visits containing, for each unique SUBJID-VISIT pair in symptoms_selected
subjects_sympto_visits <- symptoms_selected %>%
  group_by(SUBJID, VISIT, ARM, SITEID) %>%
  summarise(
    EARLIESTSYMPTDT = min(ADT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    symptoms_sup_selected %>%
      mutate(FAORRES = as.Date(FAORRES)),
    by = "SUBJID",
    relationship = "many-to-many"
  ) %>%
  mutate(
    EARLIESTDT = coalesce(FAORRES, EARLIESTSYMPTDT),
    VISIT = VISIT.x
  ) %>%
  select(-VISIT.y)

# Removes the VISIT.x column from the subjects_sympto_visits dataframe
subjects_sympto_visits <- subjects_sympto_visits %>%
  select(-VISIT.x)

print(subjects_sympto_visits)

# Displays the exceptions where EARLIESTSYMPTDT is not greater than or equal to EARLIESTDT
exceptions <- subjects_sympto_visits[subjects_sympto_visits$EARLIESTSYMPTDT < subjects_sympto_visits$EARLIESTDT, ]
if (nrow(exceptions) > 0) {
  print(exceptions)
  
  # Reformats the exceptions dataframe to only contain the earliest date in EARLIESTDT
  exceptions <- exceptions %>%
    group_by(SUBJID, VISIT) %>%
    summarize(
      EARLIESTSYMPTDT = min(EARLIESTSYMPTDT),
      FAORRES = min(FAORRES, na.rm = TRUE),
      EARLIESTDT = min(EARLIESTDT)
    )
  print(exceptions)
} else {
  cat("No exceptions found. EARLIESTSYMPTDT is greater than or equal to EARLIESTDT for all rows.\n")
}
print(subjects_sympto_visits)

# Reformats the subjects_sympto_visits dataframe to ensure each row is unique
subjects_sympto_visits <- subjects_sympto_visits %>%
  group_by(SUBJID, ARM, SITEID, VISIT) %>%
  summarize(
    EARLIESTSYMPTDT = min(EARLIESTSYMPTDT),
    FAORRES = if (all(is.na(FAORRES))) NA else min(FAORRES, na.rm = TRUE),
    EARLIESTDT = min(EARLIESTDT),
    .groups = "drop"
  )
print(subjects_sympto_visits)

# Filters out rows where EARLIESTDT is after 2021-03-13
subjects_sympto_visits <- subjects_sympto_visits %>%
  filter(ymd(EARLIESTDT) <= ymd("2021-03-13"))

# Removes the EARLIESTSYMPTDT column from the subjects_sympto_visits dataframe
subjects_sympto_visits <- subjects_sympto_visits %>%
  select(-EARLIESTSYMPTDT)

print(subjects_sympto_visits)

# Loads tests file.
tests_file <- "xpt_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.xpt"
tests <- read_xpt(tests_file)

# Filters the tests dataframe
tests <- tests %>%
  filter(MBTEST %in% c("Cepheid RT-PCR assay for SARS-CoV-2", "SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2"))
tests <- tests %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"))
print(colnames(tests))
print(tests)

# Sustains only columns required for the current analysis.
tests_selected <- tests[c("SUBJID", "VISIT", "MBTEST", "MBORRES", "MBDTC")]

# Replaces values in MBORRES column
tests_selected$MBORRES <- replace(tests_selected$MBORRES, tests_selected$MBORRES == "NEGATIVE", "NEG")
tests_selected$MBORRES <- replace(tests_selected$MBORRES, tests_selected$MBORRES == "POSITIVE", "POS")
tests_selected$MBORRES <- replace(tests_selected$MBORRES, tests_selected$MBORRES == "INDETERMINATE", "IND")
print(tests_selected)

# Filters out rows where MBDTC is after 2021-03-13
tests_selected <- tests_selected %>%
  filter(ymd(MBDTC) <= ymd("2021-03-13"))

# Creates the central_test dataframe
central_test = tests_selected[tests_selected$MBTEST == 'Cepheid RT-PCR assay for SARS-CoV-2', c('SUBJID', 'VISIT', 'MBORRES', 'MBDTC')]

# Creates the local_test dataframe
local_test = tests_selected[tests_selected$MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', c('SUBJID', 'VISIT', 'MBORRES', 'MBDTC')]

print(central_test)
print(local_test)
print(subjects_sympto_visits)

subjects_sympto_visits_formatted <- subjects_sympto_visits %>%
  left_join(
    central_test %>% 
      distinct(SUBJID, VISIT, .keep_all = TRUE) %>% 
      rename(MBDTC_central = MBDTC),
    by = c("SUBJID", "VISIT")
  ) %>%
  left_join(
    local_test %>% 
      distinct(SUBJID, VISIT, .keep_all = TRUE) %>% 
      rename(MBDTC_local = MBDTC),
    by = c("SUBJID", "VISIT")
  ) %>%
  group_by(SUBJID, VISIT) %>%
  mutate(
    central_diff = ifelse(all(is.na(MBDTC_central)), NA, min(abs(ymd(MBDTC_central) - ymd(EARLIESTDT)), na.rm = TRUE)),
    local_diff = ifelse(all(is.na(MBDTC_local)), NA, min(abs(ymd(MBDTC_local) - ymd(EARLIESTDT)), na.rm = TRUE))
  )
print(subjects_sympto_visits_formatted)
subjects_sympto_visits_formatted <- subjects_sympto_visits_formatted %>%
  select(-FAORRES, -MBORRES.x, -MBORRES.y)
print(subjects_sympto_visits_formatted)

# Whenever the difference in days for central_diff or local_diff is superior to 4, passes it to NA.
subjects_sympto_visits_under_5 <- subjects_sympto_visits_formatted %>% 
  mutate(
    central_diff = ifelse(central_diff > 4, NA, central_diff),
    local_diff = ifelse(local_diff > 4, NA, local_diff)
  )

# Groups by ARM and count the number of visits with and without central and local tests
arm_summary <- subjects_sympto_visits_under_5 %>%
  group_by(ARM) %>%
  summarize(
    total_visits = n(),
    central_test_visits = sum(!is.na(central_diff)),
    local_test_visits = sum(!is.na(local_diff))
  )

# Prints the result
print(arm_summary)

# Calculates the number of symptomatic visits with Central and Local tests for each ARM
arm_test_counts <- subjects_sympto_visits_under_5 %>%
  group_by(ARM, has_central_test = !is.na(central_diff), has_local_test = !is.na(local_diff)) %>%
  tally()

# Calculates percentages of visits resulting in tests
central_test_percentages <- arm_test_counts %>%
  group_by(ARM) %>%
  summarise(
    total_visits = sum(n),
    central_test_visits = sum(n[has_central_test]),
    central_test_percentage = (sum(n[has_central_test]) / sum(n)) * 100
  )

local_test_percentages <- arm_test_counts %>%
  group_by(ARM) %>%
  summarise(
    total_visits = sum(n),
    local_test_visits = sum(n[has_local_test]),
    local_test_percentage = (sum(n[has_local_test]) / sum(n)) * 100
  )


# Calculates chi-square statistic and p-value for central test
central_test_table <- matrix(c(
  central_test_percentages$central_test_visits[1],
  central_test_percentages$total_visits[1] - central_test_percentages$central_test_visits[1],
  central_test_percentages$central_test_visits[2],
  central_test_percentages$total_visits[2] - central_test_percentages$central_test_visits[2]
), nrow = 2, byrow = TRUE)
rownames(central_test_table) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(central_test_table) <- c("Central Test", "No Central Test")
central_test_chi_sq <- chisq.test(central_test_table)

# Calculates chi-square statistic and p-value for local test
local_test_table <- matrix(c(
  local_test_percentages$local_test_visits[1],
  local_test_percentages$total_visits[1] - local_test_percentages$local_test_visits[1],
  local_test_percentages$local_test_visits[2],
  local_test_percentages$total_visits[2] - local_test_percentages$local_test_visits[2]
), nrow = 2, byrow = TRUE)
rownames(local_test_table) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(local_test_table) <- c("Local Test", "No Local Test")
local_test_chi_sq <- chisq.test(local_test_table)

print(central_test_percentages)
print(central_test_table)
print(central_test_chi_sq)
print(local_test_percentages)
print(local_test_table)
print(local_test_chi_sq)

# Filters out rows where EARLIESTDT is after 2020-11-14
subjects_sympto_visits_nov_14 <- subjects_sympto_visits_under_5 %>%
  filter(ymd(EARLIESTDT) <= ymd("2020-11-14"))
print(subjects_sympto_visits_nov_14)

# Groups by ARM and count the number of visits with and without central and local tests
arm_summary_nov_14 <- subjects_sympto_visits_nov_14 %>%
  group_by(ARM) %>%
  summarize(
    total_visits = n(),
    central_test_visits = sum(!is.na(central_diff )),
    local_test_visits = sum(!is.na(local_diff ))
  )

# Prints the result
print(arm_summary_nov_14)


# Calculates the number of symptomatic visits with Central and Local tests for each ARM
arm_test_counts_nov_14 <- subjects_sympto_visits_nov_14 %>%
  group_by(ARM, has_central_test = !is.na(central_diff), has_local_test = !is.na(local_diff)) %>%
  tally()

# Calculates percentages of visits resulting in tests
central_test_percentages_nov_14 <- arm_test_counts_nov_14 %>%
  group_by(ARM) %>%
  summarise(
    total_visits = sum(n),
    central_test_visits = sum(n[has_central_test]),
    central_test_percentage = (sum(n[has_central_test]) / sum(n)) * 100
  )

local_test_percentages_nov_14 <- arm_test_counts_nov_14 %>%
  group_by(ARM) %>%
  summarise(
    total_visits = sum(n),
    local_test_visits = sum(n[has_local_test]),
    local_test_percentage = (sum(n[has_local_test]) / sum(n)) * 100
  )


# Calculates chi-square statistic and p-value for central test
central_test_table_nov_14 <- matrix(c(
  central_test_percentages_nov_14$central_test_visits[1],
  central_test_percentages_nov_14$total_visits[1] - central_test_percentages_nov_14$central_test_visits[1],
  central_test_percentages_nov_14$central_test_visits[2],
  central_test_percentages_nov_14$total_visits[2] - central_test_percentages_nov_14$central_test_visits[2]
), nrow = 2, byrow = TRUE)
rownames(central_test_table_nov_14) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(central_test_table_nov_14) <- c("Central Test", "No Central Test")
central_test_chi_sq_nov_14 <- chisq.test(central_test_table_nov_14)

# Calculates chi-square statistic and p-value for local test
local_test_table_nov_14 <- matrix(c(
  local_test_percentages_nov_14$local_test_visits[1],
  local_test_percentages_nov_14$total_visits[1] - local_test_percentages_nov_14$local_test_visits[1],
  local_test_percentages_nov_14$local_test_visits[2],
  local_test_percentages_nov_14$total_visits[2] - local_test_percentages_nov_14$local_test_visits[2]
), nrow = 2, byrow = TRUE)
rownames(local_test_table_nov_14) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(local_test_table_nov_14) <- c("Local Test", "No Local Test")
local_test_chi_sq_nov_14 <- chisq.test(local_test_table_nov_14)

print(central_test_percentages_nov_14)
print(central_test_table_nov_14)
print(central_test_chi_sq_nov_14)
print(local_test_percentages_nov_14)
print(local_test_table_nov_14)
print(local_test_chi_sq_nov_14)

