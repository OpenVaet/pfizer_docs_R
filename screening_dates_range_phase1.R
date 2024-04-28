# Load necessary packages
library(haven)
library(lubridate)
library(dplyr)
library(tidyr)

# Read the XPT file
adsl_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt')

# Convert RFICDT column to Date format
adsl_data$RFICDT <- ymd(adsl_data$RFICDT)

# Filter out subjects without a proper RFICDT date
adsl_data <- adsl_data %>% filter(
	!is.na(RFICDT),
	PHASEN == 1
)

# Select the relevant columns for later use.
selected_data <- adsl_data %>% select(
  SUBJID, SITEID, AGE, SEX, ARM, COHORT, DTHDT, ENRLFL, RANDFL, RFICDT, 
  VAX101DT, VAX101, VAX102, VAX102DT, VAX201DT, VAX201, VAX202, VAX202DT, 
  UNBLNDDT, PHASE, PHASEN, RANDDT, SAFFL, SEX
)

# Write the selected data to a CSV file
write.csv(selected_data, "phase_1_subjects_adsl_data.csv", row.names = FALSE)

# Defines protocol modification dates as strings.
first_protocol_modification_date <- "2020-04-15"
last_protocol_modification_date <- "2020-07-30"

# Generate a sequence of dates from the earliest date to the last protocol modification date
date_sequence <- seq(from = as.Date(first_protocol_modification_date), to = as.Date(last_protocol_modification_date), by = "day")

# Create a data frame with the date sequence
daily_recruitment <- data.frame(RFICDT = date_sequence)

# Count the number of subjects screened on each date within adsl_data
daily_counts <- adsl_data %>%
  group_by(RFICDT) %>%
  summarise(subjects_screened = n(), .groups = 'drop')

# Merge the sequence of dates with the count of subjects
daily_recruitment <- merge(daily_recruitment, daily_counts, by = "RFICDT", all.x = TRUE)

# Replace NA values with 0 for days with no subjects screened
daily_recruitment$subjects_screened[is.na(daily_recruitment$subjects_screened)] <- 0

# Write the data to a CSV file
write.csv(daily_recruitment, "subjects_screened_daily_phase_1.csv", row.names = FALSE)

# Output to console for confirmation
print("The subjects_screened_daily_phase_1.csv file has been created.")

# Fetch the earliest and latest date of screening
earliest_date <- min(adsl_data$RFICDT, na.rm = TRUE)
latest_date <- max(adsl_data$RFICDT, na.rm = TRUE)

# Output the earliest and latest date
print(paste("Earliest date of screening: ", earliest_date))
print(paste("Latest date of screening: ", latest_date))

# Count the number of subjects randomized and categorize non-randomized subjects based on 'ARM' column
daily_randomization_counts <- adsl_data %>%
  mutate(Non_Randomized_Category = case_when(
    is.na(RANDDT) & ARM == "SCREEN FAILURE" ~ "Screen_Failure",
    is.na(RANDDT) & ARM == "NOT ASSIGNED" ~ "Not_Assigned",
    TRUE ~ as.character(ARM)  # Default to the actual ARM value if not NA and not one of the specified categories
  )) %>%
  group_by(RFICDT) %>%
  summarise(
    Randomized = sum(!is.na(RANDDT), na.rm = TRUE),
    Screen_Failure = sum(Non_Randomized_Category == "Screen_Failure", na.rm = TRUE),
    Not_Assigned = sum(Non_Randomized_Category == "Not_Assigned", na.rm = TRUE),
    .groups = 'drop'
  )

# Merge the randomization counts with the existing daily recruitment data frame
daily_recruitment <- merge(daily_recruitment, daily_randomization_counts, by = "RFICDT", all.x = TRUE)

# Replace NA values with 0 for days with no subjects in each category
daily_recruitment$Randomized[is.na(daily_recruitment$Randomized)] <- 0
daily_recruitment$Screen_Failure[is.na(daily_recruitment$Screen_Failure)] <- 0
daily_recruitment$Not_Assigned[is.na(daily_recruitment$Not_Assigned)] <- 0

# Remove the 'Screened' column as it is no longer needed
daily_recruitment <- select(daily_recruitment, -subjects_screened)

# Write the data to a CSV file with the new structure
write.csv(daily_recruitment, "subjects_randomization_phase_1.csv", row.names = FALSE)

# Output to console for confirmation
print("The subjects_randomization_phase_1.csv file has been created with the updated randomization data.")

# Convert the 'RFICDT' column to Date format for comparison
daily_recruitment$RFICDT <- as.Date(daily_recruitment$RFICDT)

daily_recruitment <- select(daily_recruitment, -Screen_Failure)

# Create a new column to categorize the data based on the date criterion
daily_recruitment$Period <- ifelse(daily_recruitment$RFICDT < as.Date("2020-05-19"), "Before", "After")

# Exclude 'Screen_Failure' subjects and create a contingency table for the chi-square test
contingency_table <- daily_recruitment %>%
  group_by(Period) %>%
  summarise(
    Randomized = sum(Randomized),
    Not_Assigned = sum(Not_Assigned)
  ) %>%
  select(-Period) %>%
  as.matrix()  # Convert to matrix for the chi-square test
print(contingency_table)

# Perform the chi-square test
chi_square_result <- chisq.test(contingency_table)

# Output the result of the chi-square test
print(chi_square_result)

# Extract the relevant components from the chi-square test result
chi_square_summary <- data.frame(
  statistic = chi_square_result$statistic,
  parameter = chi_square_result$parameter,
  p_value = chi_square_result$p.value,
  method = as.character(chi_square_result$method),
  data_name = as.character(chi_square_result$data.name)
)

# Write the summary to a CSV file
write.csv(chi_square_summary, "phase_1_randomization_chi_square_results.csv", row.names = TRUE)

# Output to console for confirmation
print("The phase_1_randomization_chi_square_results.csv file has been created with the chi-square test results.")
