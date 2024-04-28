# Load necessary packages
library(haven)
library(dplyr)

# Read the phase 1 subjects data to get the list of subject IDs
phase_1_subjects <- read.csv("phase_1_subjects_adsl_data.csv")
phase_1_subjids <- phase_1_subjects$SUBJID

# Read the new XPT file
adva_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.xpt')

# Filter the ADVA data to include only subjects present in the phase 1 subjects data
adva_data_filtered <- adva_data %>% 
  filter(SUBJID %in% phase_1_subjids)

# Count the total of unique SUBJID for each VISIT
visit_counts <- adva_data_filtered %>%
  group_by(VISIT) %>%
  summarise(Unique_Subjects = n_distinct(SUBJID))

# Print the visit counts to the console
print(visit_counts)

# Write the visit counts to a CSV file
write.csv(visit_counts, "phase_1_visits_adva.csv", row.names = FALSE)

# Ensure unique entries for each subject (SUBJID), for each VISIT and each ISDTC, and for each PARAM
unique_adva_data <- adva_data_filtered %>% 
  distinct(SUBJID, VISIT, ISDTC, PARAM, AVALC, .keep_all = TRUE)

# Remove PARAM = "N-binding antibody - N-binding Antibody Assay" from unique_adva_data
unique_adva_data <- unique_adva_data %>%
  filter(PARAM != "N-binding antibody - N-binding Antibody Assay")

# Detect and print the "SUBJID - VISIT - ISDTC - PARAM" sequences with different results "AVALC"
different_results <- unique_adva_data %>%
  group_by(SUBJID, VISIT, ISDTC, PARAM) %>%
  filter(n_distinct(AVALC) > 1) %>%
  summarise(
    Result_A = first(AVALC[AVISIT != ""]),
    Result_B = first(AVALC[AVISIT == ""]),
    `Difference A - B` = as.numeric(first(AVALC[AVISIT != ""])) - as.numeric(first(AVALC[AVISIT == ""])),
    .groups = 'drop'
  )

# Check if there are any different results and write them to a CSV file
if (nrow(different_results) > 0) {
  print('different_results : ')
  print(different_results)
  write.csv(different_results, "phase_1_different_adva_results.csv", row.names = FALSE)
} else {
  print("No different results found for the same 'SUBJID - VISIT - ISDTC - PARAM' sequence.")
}



# Initialize an empty data frame to store the results
averages_df <- data.frame(
  VISIT = character(),
  PARAM = character(),
  Average_AVALC_with_AVISIT_complete = numeric(),
  Average_AVALC_with_AVISIT_incomplete = numeric(),
  stringsAsFactors = FALSE
)


# Loop over each unique combination of SUBJID, VISIT, ISDTC, and PARAM
unique_combinations <- unique(unique_adva_data[c("SUBJID", "VISIT", "ISDTC", "PARAM")])

for (i in 1:nrow(unique_combinations)) {
  # Extract the current combination
  current_combination <- unique_combinations[i, ]
  # print(current_combination$SUBJID)

  # Filter the data for the current combination
  current_data <- subset(unique_adva_data, SUBJID == current_combination$SUBJID & VISIT == current_combination$VISIT & ISDTC == current_combination$ISDTC & PARAM == current_combination$PARAM)
  # print(current_data)

  # Calculate the sum for AVALC where AVISIT is complete
  data_with_avisit_complete <- current_data[(!is.na(current_data$AVISIT) & current_data$AVISIT != "") | current_data$AVISIT != "", ]
  avalc_with_avisit_complete <- as.numeric(data_with_avisit_complete$AVALC)

  # Calculate the sum for AVALC where AVISIT is incomplete
  data_with_avisit_incomplete <- current_data[current_data$AVISIT == "" | current_data$AVISIT != "", ]
  avalc_with_avisit_incomplete <- as.numeric(data_with_avisit_incomplete$AVALC)
  print(avalc_with_avisit_complete)
  print(avalc_with_avisit_incomplete)
  if (avalc_with_avisit_complete != avalc_with_avisit_incomplete) {
    break
  }
}

# Calculate the average of sums for each VISIT and PARAM
for (i in seq_along(sums_list)) {
  sums <- sums_list[[i]]
  num_subjects <- nrow(filter(unique_adva_data, VISIT == sums$VISIT & PARAM == sums$PARAM))
  
  averages_df <- rbind(averages_df, data.frame(
    VISIT = sums$VISIT,
    PARAM = sums$PARAM,
    Average_AVALC_with_AVISIT_complete = sums$Sum_AVALC_with_AVISIT_complete / num_subjects,
    Average_AVALC_with_AVISIT_incomplete = sums$Sum_AVALC_with_AVISIT_incomplete / num_subjects
  ))
}

# Write the averages dataframe to a CSV file
write.csv(averages_df, "phase_1_visit_param_stats.csv", row.names = FALSE)
print(averages_df)



