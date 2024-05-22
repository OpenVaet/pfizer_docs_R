library(readr)
library(haven)
library(lubridate)
library(dplyr)
library(ggplot2)

cut_off_date <- ymd("2020-11-14")

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
print(randomized_pop)

# Earliest date of randomization.
randomized_pop$RANDDT <- as.Date(randomized_pop$RANDDT)
earliest_randomization_date <- min(randomized_pop$RANDDT)
print(earliest_randomization_date)

# Loads the tests & filters them to the Phase 3 population randomized, from first randomization to cut-off, tests after 1st dose only.
tests_file <- 'subjects_test_data.csv'
tests <- read.csv(tests_file)
tests_data <- tests[tests$SUBJID %in% randomized_pop$SUBJID, ]
central_pcrs_tests_data <- tests_data %>%
  filter(
    ymd(TESTDATE) <= cut_off_date &
    ymd(TESTDATE) >= ymd(earliest_randomization_date) &
    TESTRESULT == 'POS' &
    TESTTYPE == "PCR Central" &
    ymd(TESTDATE) > ymd(VAX101DT)
)

# Adds a column "Days from start" & Filters to the first positive 
central_pcrs_tests_data$DAYSFROMSTART <- as.Date(central_pcrs_tests_data$TESTDATE) - as.Date(earliest_randomization_date)
central_pcrs_tests_data <- central_pcrs_tests_data %>%
  group_by(SUBJID) %>%
  arrange(TESTDATE) %>%
  slice(1)
print(central_pcrs_tests_data)

# Creates a new dataframe containing, for each ARM, for each DAYSFROMSTART, the total of unique SUBJID in central_pcrs_tests_data
# BNT
bnt_central_pcrs_tests_data <- central_pcrs_tests_data %>%
  filter(
    ARM == 'BNT162b2 Phase 2/3 (30 mcg)'    
)
bnt_central_pcrs_pos_by_days_from_start <- bnt_central_pcrs_tests_data %>%
  group_by(DAYSFROMSTART) %>%
  summarize(unique_SUBJID = n_distinct(SUBJID))
print(bnt_central_pcrs_pos_by_days_from_start)
# Placebo
placebo_central_pcrs_tests_data <- central_pcrs_tests_data %>%
  filter(
    ARM == 'Placebo'    
  )
placebo_central_pcrs_pos_by_days_from_start <- placebo_central_pcrs_tests_data %>%
  group_by(DAYSFROMSTART) %>%
  summarize(unique_SUBJID = n_distinct(SUBJID))
print(placebo_central_pcrs_pos_by_days_from_start)

# Calculates the cumulative sum of unique_SUBJID
bnt_central_pcrs_pos_by_days_from_start$cum_unique_SUBJID <- cumsum(bnt_central_pcrs_pos_by_days_from_start$unique_SUBJID)
placebo_central_pcrs_pos_by_days_from_start$cum_unique_SUBJID <- cumsum(placebo_central_pcrs_pos_by_days_from_start$unique_SUBJID)
print(bnt_central_pcrs_pos_by_days_from_start, n=100)
print(placebo_central_pcrs_pos_by_days_from_start, n=100)

# Creates the first chart (PCR Positive)
ggplot() + 
  geom_line(data = bnt_central_pcrs_pos_by_days_from_start, aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, color = "BNT162b2"), size = 1.2) + 
  geom_line(data = placebo_central_pcrs_pos_by_days_from_start, aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, color = "Placebo"), size = 1.2) + 
  geom_text(data = bnt_central_pcrs_pos_by_days_from_start %>% 
              mutate(DAYSFROMSTART_INT = as.integer(DAYSFROMSTART)) %>% 
              filter(DAYSFROMSTART_INT %% 3 == 0 | DAYSFROMSTART == max(DAYSFROMSTART)), 
            aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, label = cum_unique_SUBJID), color = "blue", hjust = -0.2) + 
  geom_text(data = placebo_central_pcrs_pos_by_days_from_start %>% 
              mutate(DAYSFROMSTART_INT = as.integer(DAYSFROMSTART)) %>% 
              filter(DAYSFROMSTART_INT %% 3 == 0 | DAYSFROMSTART == max(DAYSFROMSTART)), 
            aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, label = cum_unique_SUBJID), color = "red", hjust = -0.2) + 
  labs(x = "Days from Start", y = "Cumulative Number of Tests", title = "C4591001 - Positive PCRs by arms to November 14, 2020", color = "Arm") + 
  scale_color_manual(values = c("blue", "red")) + 
  theme_minimal() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

##############################################################
##################### DATA WITH SYMPTOMS ##################### 
##############################################################
# Loads symptoms recorded.
adsympt_file <- 'xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt'
adsympt_data <- read_xpt(adsympt_file)
adsympt_selected_data <- adsympt_data[c("USUBJID", "PARCAT1", "PARAM", "AVISIT", "AVALC", "ADT", "AENDT")]
adsympt_filtered_data <- adsympt_selected_data[adsympt_selected_data$PARCAT1 == 'SIGNS AND SYMPTOMS OF DISEASE', ]
adsympt_filtered_data$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adsympt_filtered_data$USUBJID)
adsympt_filtered_data <- adsympt_filtered_data[c("SUBJID", "PARCAT1", "PARAM", "AVISIT", "AVALC", "ADT", "AENDT")]
adsympt_filtered_data <- adsympt_filtered_data %>%
  filter(
    AVALC == 'Y' &
    as.Date(ADT) <= cut_off_date
  )
print(adsympt_filtered_data)

# Creates a new dataframe containing the positive tests distant from symptoms from at most 4 days
central_pcrs_tests_data <- central_pcrs_tests_data %>%
  mutate(SUBJID = as.character(SUBJID))
central_pcrs_tests_data_with_symptoms <- central_pcrs_tests_data %>%
  inner_join(
    adsympt_filtered_data, 
    by = "SUBJID"
  ) %>%
  filter(
    abs(as.Date(TESTDATE) - as.Date(ADT)) <= 4
  )
central_pcrs_tests_data_with_symptoms <- central_pcrs_tests_data_with_symptoms %>%
  group_by(SUBJID) %>%
  arrange(TESTDATE) %>%
  slice(1)
print(central_pcrs_tests_data_with_symptoms)

# Creates a new dataframe containing, for each ARM, for each DAYSFROMSTART, the total of unique SUBJID in central_pcrs_tests_data_with_symptoms
# BNT
bnt_central_pcrs_tests_data_with_symptoms <- central_pcrs_tests_data_with_symptoms %>%
  filter(
    ARM == 'BNT162b2 Phase 2/3 (30 mcg)'    
  )
bnt_central_pcrs_pos_by_days_from_start_with_symptoms <- bnt_central_pcrs_tests_data_with_symptoms %>%
  group_by(DAYSFROMSTART) %>%
  summarize(unique_SUBJID = n_distinct(SUBJID))
print(bnt_central_pcrs_pos_by_days_from_start_with_symptoms)
# Placebo
placebo_central_pcrs_tests_data_with_symptoms <- central_pcrs_tests_data_with_symptoms %>%
  filter(
    ARM == 'Placebo'    
  )
placebo_central_pcrs_pos_by_days_from_start_with_symptoms <- placebo_central_pcrs_tests_data_with_symptoms %>%
  group_by(DAYSFROMSTART) %>%
  summarize(unique_SUBJID = n_distinct(SUBJID))
print(placebo_central_pcrs_pos_by_days_from_start_with_symptoms)

# Calculates the cumulative sum of unique_SUBJID
bnt_central_pcrs_pos_by_days_from_start_with_symptoms$cum_unique_SUBJID <- cumsum(bnt_central_pcrs_pos_by_days_from_start_with_symptoms$unique_SUBJID)
placebo_central_pcrs_pos_by_days_from_start_with_symptoms$cum_unique_SUBJID <- cumsum(placebo_central_pcrs_pos_by_days_from_start_with_symptoms$unique_SUBJID)
print(bnt_central_pcrs_pos_by_days_from_start_with_symptoms, n=100)
print(placebo_central_pcrs_pos_by_days_from_start_with_symptoms, n=100)

# Creates the second chart (PCR Positive with Official Symptoms within 4 days)
ggplot() + 
  geom_line(data = bnt_central_pcrs_pos_by_days_from_start_with_symptoms, aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, color = "BNT162b2"), size = 1.2) + 
  geom_line(data = placebo_central_pcrs_pos_by_days_from_start_with_symptoms, aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, color = "Placebo"), size = 1.2) + 
  geom_text(data = bnt_central_pcrs_pos_by_days_from_start_with_symptoms %>% 
              mutate(DAYSFROMSTART_INT = as.integer(DAYSFROMSTART)) %>% 
              filter(DAYSFROMSTART_INT %% 3 == 0 | DAYSFROMSTART == max(DAYSFROMSTART)), 
            aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, label = cum_unique_SUBJID), color = "blue", hjust = -0.2) + 
  geom_text(data = placebo_central_pcrs_pos_by_days_from_start_with_symptoms %>% 
              mutate(DAYSFROMSTART_INT = as.integer(DAYSFROMSTART)) %>% 
              filter(DAYSFROMSTART_INT %% 3 == 0 | DAYSFROMSTART == max(DAYSFROMSTART)), 
            aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, label = cum_unique_SUBJID), color = "red", hjust = -0.2) + 
  labs(x = "Days from Start", y = "Cumulative Number of Tests", title = "C4591001 - Cumulative (Official) Symptomatic Positive PCRs for BNT162b2 and Placebo to November 14, 2020", color = "Arm") + 
  scale_color_manual(values = c("blue", "red")) + 
  theme_minimal() + 
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Compares the rate of BNT & Placebo with or without symptoms within 4 days.
# Calculates the cumulative sum of unique_SUBJID
bnt_central_pcrs_pos_by_days_from_start$cum_unique_SUBJID <- cumsum(bnt_central_pcrs_pos_by_days_from_start$unique_SUBJID)
placebo_central_pcrs_pos_by_days_from_start$cum_unique_SUBJID <- cumsum(placebo_central_pcrs_pos_by_days_from_start$unique_SUBJID)
print(bnt_central_pcrs_pos_by_days_from_start, n=100)
print(placebo_central_pcrs_pos_by_days_from_start, n=100)

# Retrieves the highest cumulative total of bnt subjects with positive PCR and the total of placebo subjects
bnt_total_pos <- tail(bnt_central_pcrs_pos_by_days_from_start$cum_unique_SUBJID, 1)
placebo_total_pos <- tail(placebo_central_pcrs_pos_by_days_from_start$cum_unique_SUBJID, 1)

# Retrieves the same values for positive PCR + symptoms
bnt_total_pos_with_symptoms <- tail(bnt_central_pcrs_pos_by_days_from_start_with_symptoms$cum_unique_SUBJID, 1)
placebo_total_pos_with_symptoms <- tail(placebo_central_pcrs_pos_by_days_from_start_with_symptoms$cum_unique_SUBJID, 1)

# Calculates the total of BNT & Placebo without symptoms within 4 days
bnt_total_without_symptoms <- bnt_total_pos - bnt_total_pos_with_symptoms
placebo_total_without_symptoms <- placebo_total_pos - placebo_total_pos_with_symptoms
print(paste("BNT total without symptoms:", bnt_total_without_symptoms))
print(paste("Placebo total without symptoms:", placebo_total_without_symptoms))
print(paste("BNT total with symptoms:", bnt_total_pos_with_symptoms))
print(paste("Placebo total with symptoms:", placebo_total_pos_with_symptoms))

# Printing no-symptom vs symptom chi-square.
symptomatic_pcr_table <- matrix(c(
  bnt_total_without_symptoms,
  bnt_total_pos_with_symptoms,
  placebo_total_without_symptoms,
  placebo_total_pos_with_symptoms
), nrow = 2, byrow = TRUE)
rownames(symptomatic_pcr_table) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(symptomatic_pcr_table) <- c("No Symptom at +/- 4 days", "Symptoms at +/- 4 days")
print(symptomatic_pcr_table)
symptomatic_pcr_chi_sq <- chisq.test(symptomatic_pcr_table)

# Loads the html template
template <- readLines("chi_square_template.html")
print(template)

# Replaces COLUMN_A, COLUMN_B, ROW_A_LABEL, ROW_A_VALUE_1 etc by the values of the contingency table
symptomatic_pcr_template <- template
symptomatic_pcr_template <- gsub("COLUMN_A", colnames(symptomatic_pcr_table)[1], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("COLUMN_B", colnames(symptomatic_pcr_table)[2], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("ROW_A_LABEL", rownames(symptomatic_pcr_table)[1], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("ROW_A_VALUE_1", symptomatic_pcr_table[1, 1], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("ROW_A_VALUE_2", symptomatic_pcr_table[1, 2], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("ROW_B_LABEL", rownames(symptomatic_pcr_table)[2], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("ROW_B_VALUE_1", symptomatic_pcr_table[2, 1], symptomatic_pcr_template)
symptomatic_pcr_template <- gsub("ROW_B_VALUE_2", symptomatic_pcr_table[2, 2], symptomatic_pcr_template)

# Replaces P_VALUE with the p-value from the chi-square test
symptomatic_pcr_template <- gsub("P_VALUE", format.pval(symptomatic_pcr_chi_sq$p.value, digits = 4), symptomatic_pcr_template)

# Writes the modified template
writeLines(symptomatic_pcr_template, "symptomatic_pcr_chi_sq.html")

##############################################################
################# DATA WITH SYMPTOMS MERGED ################## 
##############################################################
# Loads merged symptoms.
merged_symptoms_file <- 'covid_symptoms_accross_datasets.csv'
merged_symptoms <- read.csv(merged_symptoms_file)
merged_symptoms <- merged_symptoms %>%
  filter(
      as.Date(REPORTDATE) <= cut_off_date
  )
print(merged_symptoms)
merged_symptoms <- merged_symptoms[c("SUBJID", "REPORTDATE", "SYMPTOM")]
print(merged_symptoms)

# Creates a new dataframe containing the positive tests distant from merged symptoms from at most 4 days
merged_symptoms <- merged_symptoms %>%
  mutate(SUBJID = as.character(SUBJID))
central_pcrs_tests_data_with_symptoms_merged <- central_pcrs_tests_data %>%
  inner_join(
    merged_symptoms, 
    by = "SUBJID"
  ) %>%
  filter(
    abs(as.Date(TESTDATE) - as.Date(REPORTDATE)) <= 4
  )
central_pcrs_tests_data_with_symptoms_merged <- central_pcrs_tests_data_with_symptoms_merged %>%
  group_by(SUBJID) %>%
  arrange(TESTDATE) %>%
  slice(1)
print(central_pcrs_tests_data_with_symptoms_merged)

# Creates a new dataframe containing, for each ARM, for each DAYSFROMSTART, the total of unique SUBJID in central_pcrs_tests_data_with_symptoms_merged
# BNT
bnt_central_pcrs_tests_data_with_symptoms_merged <- central_pcrs_tests_data_with_symptoms_merged %>%
  filter(
    ARM == 'BNT162b2 Phase 2/3 (30 mcg)'    
  )
bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged <- bnt_central_pcrs_tests_data_with_symptoms_merged %>%
  group_by(DAYSFROMSTART) %>%
  summarize(unique_SUBJID = n_distinct(SUBJID))
print(bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged)
# Placebo
placebo_central_pcrs_tests_data_with_symptoms_merged <- central_pcrs_tests_data_with_symptoms_merged %>%
  filter(
    ARM == 'Placebo'    
  )
placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged <- placebo_central_pcrs_tests_data_with_symptoms_merged %>%
  group_by(DAYSFROMSTART) %>%
  summarize(unique_SUBJID = n_distinct(SUBJID))
print(placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged)

# Calculates the cumulative sum of unique_SUBJID
bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged$cum_unique_SUBJID <- cumsum(bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged$unique_SUBJID)
placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged$cum_unique_SUBJID <- cumsum(placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged$unique_SUBJID)
print(bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged, n=100)
print(placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged, n=100)

# Creates the third chart 
ggplot() + 
  geom_line(data = bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged, aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, color = "BNT162b2"), size = 1.2) + 
  geom_line(data = placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged, aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, color = "Placebo"), size = 1.2) + 
  geom_text(data = bnt_central_pcrs_pos_by_days_from_start_with_symptoms_merged %>% 
              mutate(DAYSFROMSTART_INT = as.integer(DAYSFROMSTART)) %>% 
              filter(DAYSFROMSTART_INT %% 3 == 0 | DAYSFROMSTART == max(DAYSFROMSTART)), 
            aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, label = cum_unique_SUBJID), color = "blue", hjust = -0.2) + 
  geom_text(data = placebo_central_pcrs_pos_by_days_from_start_with_symptoms_merged %>% 
              mutate(DAYSFROMSTART_INT = as.integer(DAYSFROMSTART)) %>% 
              filter(DAYSFROMSTART_INT %% 3 == 0 | DAYSFROMSTART == max(DAYSFROMSTART)), 
            aes(x = DAYSFROMSTART, y = cum_unique_SUBJID, label = cum_unique_SUBJID), color = "red", hjust = -0.2) + 
  labs(x = "Days from Start", y = "Cumulative Number of Tests", title = "C4591001 - Cumulative (All Datasets) Symptomatic Positive PCRs for BNT162b2 and Placebo from Dose 1 to November 14, 2020", color = "Arm") + 
  scale_color_manual(values = c("blue", "red")) + 
  theme_minimal() + 
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
