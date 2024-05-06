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

# Writes the result to a CSV file
write.csv(subjects_sympto_visits, "phase_3_subjects_sympto_visits.csv", row.names = FALSE)

# Groups by ARM and count the number of visits with and without central and local tests
arm_summary <- subjects_sympto_visits %>%
  group_by(ARM) %>%
  summarize(
    total_visits = n(),
    central_test_visits = sum(!is.na(EARLIESTCENTRALDT)),
    local_test_visits = sum(!is.na(EARLIESTLOCALDT))
  )

# Prints the result
print(arm_summary)

# Calculates the number of symptomatic visits with Central and Local tests for each ARM
arm_test_counts <- subjects_sympto_visits %>%
  group_by(ARM, has_central_test = !is.na(EARLIESTCENTRALDT), has_local_test = !is.na(EARLIESTLOCALDT)) %>%
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

# Loads the html template
template <- readLines("chi_square_template.html")

# Replaces COLUMN_A, COLUMN_B, ROW_A_LABEL, ROW_A_VALUE_1 etc by the values of the contingency table
chi_march_13_template <- template
chi_march_13_template <- gsub("COLUMN_A", colnames(local_test_table)[1], chi_march_13_template)
chi_march_13_template <- gsub("COLUMN_B", colnames(local_test_table)[2], chi_march_13_template)
chi_march_13_template <- gsub("ROW_A_LABEL", rownames(local_test_table)[1], chi_march_13_template)
chi_march_13_template <- gsub("ROW_A_VALUE_1", local_test_table[1, 1], chi_march_13_template)
chi_march_13_template <- gsub("ROW_A_VALUE_2", local_test_table[1, 2], chi_march_13_template)
chi_march_13_template <- gsub("ROW_B_LABEL", rownames(local_test_table)[2], chi_march_13_template)
chi_march_13_template <- gsub("ROW_B_VALUE_1", local_test_table[2, 1], chi_march_13_template)
chi_march_13_template <- gsub("ROW_B_VALUE_2", local_test_table[2, 2], chi_march_13_template)

# Replaces P_VALUE with the p-value from the chi-square test
chi_march_13_template <- gsub("P_VALUE", format.pval(local_test_chi_sq$p.value, digits = 4), chi_march_13_template)

# Writes the modified template
writeLines(chi_march_13_template, "local_test_chi_sq_march_13.html")

# Filters out rows where EARLIESTDT is after 2020-11-14
subjects_sympto_visits_nov_14 <- subjects_sympto_visits %>%
  filter(ymd(EARLIESTDT) <= ymd("2020-11-14"))

subjects_sympto_visits_nov_14 <- subjects_sympto_visits_nov_14 %>%
  mutate(
    EARLIESTCENTRALDT = ifelse(ymd(EARLIESTCENTRALDT) > ymd("2020-11-14"), NA, EARLIESTCENTRALDT),
    EARLIESTCENTRALPOSDT = ifelse(ymd(EARLIESTCENTRALPOSDT) > ymd("2020-11-14"), NA, EARLIESTCENTRALPOSDT),
    EARLIESTLOCALDT = ifelse(ymd(EARLIESTLOCALDT) > ymd("2020-11-14"), NA, EARLIESTLOCALDT),
    EARLIESTLOCALPOSDT = ifelse(ymd(EARLIESTLOCALPOSDT) > ymd("2020-11-14"), NA, EARLIESTLOCALPOSDT)
  )
subjects_sympto_visits_nov_14 <- subjects_sympto_visits_nov_14 %>%
  mutate(
    EARLIESTCENTRALDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALDT), as.numeric(as.Date(EARLIESTCENTRALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTCENTRALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALPOSDT), as.numeric(as.Date(EARLIESTCENTRALPOSDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALDT), as.numeric(as.Date(EARLIESTLOCALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALPOSDT), as.numeric(as.Date(EARLIESTLOCALPOSDT) - as.Date(EARLIESTDT)), NA)
  )
print(subjects_sympto_visits_nov_14)

# Writes the result to a CSV file
write.csv(subjects_sympto_visits_nov_14, "phase_3_subjects_sympto_visits_nov_14.csv", row.names = FALSE)

# Groups by ARM and count the number of visits with and without central and local tests
arm_summary_nov_14 <- subjects_sympto_visits_nov_14 %>%
  group_by(ARM) %>%
  summarize(
    total_visits = n(),
    central_test_visits = sum(!is.na(EARLIESTCENTRALDT)),
    local_test_visits = sum(!is.na(EARLIESTLOCALDT))
  )

# Prints the result
print(arm_summary_nov_14)


# Calculates the number of symptomatic visits with Central and Local tests for each ARM
arm_test_counts_nov_14 <- subjects_sympto_visits_nov_14 %>%
  group_by(ARM, has_central_test = !is.na(EARLIESTCENTRALDT), has_local_test = !is.na(EARLIESTLOCALDT)) %>%
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

# Replaces COLUMN_A, COLUMN_B, ROW_A_LABEL, ROW_A_VALUE_1 etc by the values of the contingency table
chi_nov_14_template <- template
chi_nov_14_template <- gsub("COLUMN_A", colnames(local_test_table_nov_14)[1], chi_nov_14_template)
chi_nov_14_template <- gsub("COLUMN_B", colnames(local_test_table_nov_14)[2], chi_nov_14_template)
chi_nov_14_template <- gsub("ROW_A_LABEL", rownames(local_test_table_nov_14)[1], chi_nov_14_template)
chi_nov_14_template <- gsub("ROW_A_VALUE_1", local_test_table_nov_14[1, 1], chi_nov_14_template)
chi_nov_14_template <- gsub("ROW_A_VALUE_2", local_test_table_nov_14[1, 2], chi_nov_14_template)
chi_nov_14_template <- gsub("ROW_B_LABEL", rownames(local_test_table_nov_14)[2], chi_nov_14_template)
chi_nov_14_template <- gsub("ROW_B_VALUE_1", local_test_table_nov_14[2, 1], chi_nov_14_template)
chi_nov_14_template <- gsub("ROW_B_VALUE_2", local_test_table_nov_14[2, 2], chi_nov_14_template)

# Replaces P_VALUE with the p-value from the chi-square test
chi_nov_14_template <- gsub("P_VALUE", format.pval(local_test_chi_sq_nov_14$p.value, digits = 4), chi_nov_14_template)

# Writes the modified template
writeLines(chi_nov_14_template, "local_test_chi_sq_nov_14.html")