library(readr)
library(haven)
library(lubridate)
library(dplyr)
library(ggplot2)

cut_off_date <- ymd("2020-11-14")

# Loads the Phase 3 population randomized and filters on subjects having received 1 dose prior cut-off.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
randomized_pop_with_d1 <- randomized_pop %>%
  filter(
      !is.na(VAX101DT) &
        ymd(VAX101DT) <= cut_off_date &
        (COUNTRY == 'USA')
  )

# Earliest date of vaccination.
randomized_pop_with_d1$VAX101DT <- as.Date(randomized_pop_with_d1$VAX101DT)
earliest_vax_date <- min(randomized_pop_with_d1$VAX101DT)
print(randomized_pop_with_d1)
print(earliest_vax_date)

# Loads the tests & filters them to the Phase 3 population randomized, from first randomization to cut-off, tests after 1st dose only.
tests_file <- 'subjects_test_data.csv'
tests <- read.csv(tests_file)
tests_data <- tests[tests$SUBJID %in% randomized_pop_with_d1$SUBJID, ]
central_pcrs_tests_data <- tests_data %>%
  filter(
    ymd(TESTDATE) <= cut_off_date &
      ymd(TESTDATE) >= ymd(earliest_vax_date) &
      TESTTYPE == "PCR Central" &
      ymd(TESTDATE) > ymd(VAX101DT)
  )
central_pcrs_tests_pos_data <- central_pcrs_tests_data %>%
  filter(
      TESTRESULT == 'POS'
  )
print(central_pcrs_tests_data)
print(central_pcrs_tests_pos_data)

# Create a sequence of dates between earliest_vax_date and cut_off_date
date_seq <- seq(earliest_vax_date, cut_off_date, by = "day")

# Initialize a data frame to store the results
exposure_data <- data.frame(DATE = date_seq, EXPOSED = numeric(length(date_seq)))

# Loop through each date and calculate the number of exposed subjects
for (i in seq_along(date_seq)) {
  exposed_subjects <- randomized_pop_with_d1 %>%
    filter(ymd(VAX101DT) <= date_seq[i])
  exposed_subjects_bnt <- randomized_pop_with_d1 %>%
    filter(ymd(VAX101DT) <= date_seq[i] &
             ARM == "BNT162b2 Phase 2/3 (30 mcg)")
  exposed_subjects_placebo <- randomized_pop_with_d1 %>%
    filter(ymd(VAX101DT) <= date_seq[i] &
             ARM == "Placebo")
  exposure_data$EXPOSED[i] <- nrow(exposed_subjects)
  exposure_data$EXPOSEDBNT[i] <- nrow(exposed_subjects_bnt)
  exposure_data$EXPOSEDPLACEBO[i] <- nrow(exposed_subjects_placebo)
}

# Add a column for total tests
exposure_data$TOTALPCR <- sapply(date_seq, function(x) {
  sum(central_pcrs_tests_data$TESTDATE == x)
})

# Add a column for total positive tests
exposure_data$TOTALPOS <- sapply(date_seq, function(x) {
  sum(central_pcrs_tests_pos_data$TESTDATE == x)
})

# Add a column for total PCR tests for BNT162b2 Phase 2/3 (30 mcg)
exposure_data$TOTALPCRBNT <- sapply(date_seq, function(x) {
  sum(central_pcrs_tests_data$TESTDATE == x & central_pcrs_tests_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)")
})

# Add a column for total PCR tests for Placebo
exposure_data$TOTALPCRPLACEBO <- sapply(date_seq, function(x) {
  sum(central_pcrs_tests_data$TESTDATE == x & central_pcrs_tests_data$ARM == "Placebo")
})

# Add a column for total positive tests for BNT162b2 Phase 2/3 (30 mcg)
exposure_data$TOTALPOSBNT <- sapply(date_seq, function(x) {
  sum(central_pcrs_tests_pos_data$TESTDATE == x & central_pcrs_tests_pos_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)")
})

# Add a column for total positive tests for Placebo
exposure_data$TOTALPOSPLACEBO <- sapply(date_seq, function(x) {
  sum(central_pcrs_tests_pos_data$TESTDATE == x & central_pcrs_tests_pos_data$ARM == "Placebo")
})

# Loads the symptoms & filters them on population.
symptoms_file <- 'covid_symptoms_accross_datasets.csv'
symptoms <- read.csv(symptoms_file)
symptoms_data <- symptoms[symptoms$SUBJID %in% randomized_pop_with_d1$SUBJID, ]
print(symptoms_data)

uniq  <- unique(central_pcrs_tests_pos_data$ARM)
print(uniq)

# Add a column for total symptoms
exposure_data$TOTALSYMPTOMS <- sapply(date_seq, function(x) {
  length(unique(symptoms_data$SUBJID[symptoms_data$REPORTDATE == x]))
})

# Add a column for total symptoms for BNT162b2 Phase 2/3 (30 mcg)
exposure_data$TOTALSYMPTOMSBNT <- sapply(date_seq, function(x) {
  length(unique(symptoms_data$SUBJID[symptoms_data$REPORTDATE == x & symptoms_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)"]))
})

# Add a column for total symptoms for Placebo
exposure_data$TOTALSYMPTOMSPLACEBO <- sapply(date_seq, function(x) {
  length(unique(symptoms_data$SUBJID[symptoms_data$REPORTDATE == x & symptoms_data$ARM == "Placebo"]))
})

# Add a column for symptom rate
exposure_data$SYMPTOMRATE <- exposure_data$TOTALSYMPTOMS * 100 / exposure_data$EXPOSED

# Add a column for symptom rate for BNT162b2 Phase 2/3 (30 mcg)
exposure_data$SYMPTOMRATEBNT <- exposure_data$TOTALSYMPTOMSBNT * 100 / exposure_data$EXPOSEDBNT

# Add a column for symptom rate for Placebo
exposure_data$SYMPTOMRATEPLACEBO <- exposure_data$TOTALSYMPTOMSPLACEBO * 100 / exposure_data$EXPOSEDPLACEBO

# Add a column for test on symptom rate for BNT162b2 Phase 2/3 (30 mcg)
# exposure_data$TESTONSYMPTOMRATEBNT <- exposure_data$TOTALPCRBNT * 100 / exposure_data$TOTALSYMPTOMSBNT

# # Add a column for test on symptom rate for Placebo
# exposure_data$TESTONSYMPTOMRATEPLACEBO <- exposure_data$TOTALPCRPLACEBO * 100 / exposure_data$TOTALSYMPTOMSPLACEBO

# # Add a column for positive rate on test for Placebo
# exposure_data$POSRATEONTESTPLACEBO <- exposure_data$TOTALPOSPLACEBO * 100 / exposure_data$TOTALPCRPLACEBO

# # Add a column for positive rate on test for BNT162b2 Phase 2/3 (30 mcg)
# exposure_data$POSRATEONTESTBNT <- exposure_data$TOTALPOSBNT * 100 / exposure_data$TOTALPCRBNT

# Print the results
print(exposure_data)

# Add a column "DAYSFROMSTART" being cut_off_date - DATE
exposure_data$DAYSFROMSTART <- cut_off_date - exposure_data$DATE


exposure_data_selected <- exposure_data[c("DAYSFROMSTART", "TOTALSYMPTOMSBNT", "EXPOSEDBNT", "SYMPTOMRATEBNT", "TOTALSYMPTOMSPLACEBO", "EXPOSEDPLACEBO", "SYMPTOMRATEPLACEBO")]
exposure_data_selected <- exposure_data_selected %>%
  filter(
    DAYSFROMSTART <= 100
  )
print(exposure_data_selected)

# Create a plot to show the evolution of SYMPTOMRATEBNT and SYMPTOMRATEPLACEBO over time
ggplot(exposure_data_selected, aes(x = as.numeric(DAYSFROMSTART))) + 
  geom_line(aes(y = SYMPTOMRATEBNT, color = "BNT162b2 Phase 2/3 (30 mcg)"), size = 1.2) + 
  geom_line(aes(y = SYMPTOMRATEPLACEBO, color = "Placebo"), size = 1.2) + 
  labs(title = "C4591001 - USA Phase 2-3 Subjects Recipients of 1 Dose, Rate Reporting COVID symptoms / Subjects Exposed",
       x = "Days from start",
       y = "Symptom rate (%)",
       color = "ARM") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) + 
  scale_color_manual(values = c("blue", "red"))

write.csv(exposure_data, "exposure_data.csv", row.names = FALSE)
