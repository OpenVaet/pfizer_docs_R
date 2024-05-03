library(haven)
library(dplyr)
library(lubridate)
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

# Filters tests_selected to keep only rows where VISIT contains "COVID_"
tests_selected <- tests_selected[grepl("COVID_", tests_selected$VISIT), ]
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

subjects_sympto_visits <- subjects_sympto_visits %>%
  left_join(
    central_test %>% 
      group_by(SUBJID, VISIT) %>% 
      summarise(.groups = "drop", 
                EARLIESTCENTRALDT = min(MBDTC), 
                EARLIESTCENTRALPOSDT = ifelse(length(MBDTC[MBORRES == "POS"]) == 0, NA, min(MBDTC[MBORRES == "POS"]))),
    by = c("SUBJID", "VISIT")
  ) %>%
  left_join(
    local_test %>% 
      group_by(SUBJID, VISIT) %>% 
      summarise(.groups = "drop", 
                EARLIESTLOCALDT = min(MBDTC), 
                EARLIESTLOCALPOSDT = ifelse(length(MBDTC[MBORRES == "POS"]) == 0, NA, min(MBDTC[MBORRES == "POS"]))),
    by = c("SUBJID", "VISIT")
  )
subjects_sympto_visits <- subjects_sympto_visits %>%
  select(-FAORRES)
print(subjects_sympto_visits)

# Adds the total of days between the symptoms & the test
subjects_sympto_visits <- subjects_sympto_visits %>%
  mutate(
    EARLIESTCENTRALDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALDT), as.numeric(as.Date(EARLIESTCENTRALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTCENTRALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALPOSDT), as.numeric(as.Date(EARLIESTCENTRALPOSDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALDT), as.numeric(as.Date(EARLIESTLOCALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALPOSDT), as.numeric(as.Date(EARLIESTLOCALPOSDT) - as.Date(EARLIESTDT)), NA)
  )
print(subjects_sympto_visits)

# Groups by ARM and count the number of visits with and without central and local tests
arm_summary <- subjects_sympto_visits %>%
  group_by(SITEID, ARM) %>%
  summarize(
    total_visits = n(),
    central_test_visits = sum(!is.na(EARLIESTCENTRALDT)),
    local_test_visits = sum(!is.na(EARLIESTLOCALDT)),
    .groups = "drop"
  )

# Prints the result
print(arm_summary)

# Calculates the number of symptomatic visits with Central and Local tests for each ARM
arm_test_counts <- subjects_sympto_visits %>%
  group_by(SITEID, ARM, has_central_test = !is.na(EARLIESTCENTRALDT), has_local_test = !is.na(EARLIESTLOCALDT)) %>%
  tally()

# Calculates percentages of visits resulting in tests
central_test_percentages <- arm_test_counts %>%
  group_by(SITEID, ARM) %>%
  summarise(
    total_visits = sum(n),
    central_test_visits = sum(n[has_central_test]),
    central_test_percentage = (sum(n[has_central_test]) / sum(n)) * 100,
    .groups = "drop"
  )

local_test_percentages <- arm_test_counts %>%
  group_by(SITEID, ARM) %>%
  summarise(
    total_visits = sum(n),
    local_test_visits = sum(n[has_local_test]),
    local_test_percentage = (sum(n[has_local_test]) / sum(n)) * 100,
    .groups = "drop"
  )
print(central_test_percentages)
print(local_test_percentages)

# Filter Central data to only include SITEIDs with at least 50 subjects tested
central_filtered_data <- central_test_percentages %>%
  group_by(SITEID) %>%
  filter(sum(total_visits) >= 50)

# Perform chi-squared test for each SITEID
central_results <- central_filtered_data %>%
  group_by(SITEID) %>%
  summarise(
    chi_square_statistic = chisq.test(c(central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                                        total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                                        central_test_visits[ARM == "Placebo"],
                                        total_visits[ARM == "Placebo"] - central_test_visits[ARM == "Placebo"]))$statistic,
    df = chisq.test(c(central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                      total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                      central_test_visits[ARM == "Placebo"],
                      total_visits[ARM == "Placebo"] - central_test_visits[ARM == "Placebo"]))$parameter,
    p_value = chisq.test(c(central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                           total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                           central_test_visits[ARM == "Placebo"],
                           total_visits[ARM == "Placebo"] - central_test_visits[ARM == "Placebo"]))$p.value,
    BNT162b2_tested = central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
    BNT162b2_not_tested = total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
    Placebo_tested = central_test_visits[ARM == "Placebo"],
    Placebo_not_tested = total_visits[ARM == "Placebo"] - central_test_visits[ARM == "Placebo"],
    BNT162b2_test_pct = (central_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] / total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"]) * 100,
    Placebo_test_pct = (central_test_visits[ARM == "Placebo"] / total_visits[ARM == "Placebo"]) * 100
  )


# Print the results
print(central_results, n=120)

# Filter Local data to only include SITEIDs with at least 50 subjects tested
local_filtered_data <- local_test_percentages %>%
  group_by(SITEID) %>%
  filter(sum(total_visits) >= 50)
print(local_filtered_data, n=200)

# Perform chi-squared test for each SITEID
local_results <- local_filtered_data %>%
  group_by(SITEID) %>%
  summarise(
    BNT162b2_tested = local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
    BNT162b2_not_tested = total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
    BNT162b2_test_pct = (local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] / total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"]) * 100,
    Placebo_tested = local_test_visits[ARM == "Placebo"],
    Placebo_not_tested = total_visits[ARM == "Placebo"] - local_test_visits[ARM == "Placebo"],
    Placebo_test_pct = (local_test_visits[ARM == "Placebo"] / total_visits[ARM == "Placebo"]) * 100,
    chi_square_statistic = chisq.test(c(local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                                        total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                                        local_test_visits[ARM == "Placebo"],
                                        total_visits[ARM == "Placebo"] - local_test_visits[ARM == "Placebo"]))$statistic,
    p_value = chisq.test(c(local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                           total_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - local_test_visits[ARM == "BNT162b2 Phase 2/3 (30 mcg)"],
                           local_test_visits[ARM == "Placebo"],
                           total_visits[ARM == "Placebo"] - local_test_visits[ARM == "Placebo"]))$p.value
  )

# Writes the result to a CSV file
local_significant_results <- local_results %>%
  filter(p_value <= 0.05)

# Print the results
print(local_results, n=120)

write.csv(local_significant_results, "phase_3_local_tests_by_sites.csv", row.names = FALSE)


# Define the data
bnt_tested <- 750
bnt_not_tested <- 27
placebo_tested <- 868
placebo_not_tested <- 30

# Create a contingency table
contingency_table <- matrix(c(bnt_tested, bnt_not_tested, placebo_tested, placebo_not_tested), nrow = 2, ncol = 2)
colnames(contingency_table) <- c("Tested", "Not Tested")
rownames(contingency_table) <- c("BNT", "Placebo")

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print the results
print(contingency_table)
print(chi_square_test)
