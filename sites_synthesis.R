library(lubridate)
library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)

# File paths
randomization_offsets_file <- 'offset_randomization_between_fa_m6.csv'
deviations_file <- 'deviations_significant_results_by_sites.csv'
imbalanced_pcrs_file <- 'phase_3_local_tests_by_sites.csv'
missing_subjects_file <- 'missing_subjects_by_sites.csv'
trial_sites_file <- 'trial_site_data.json'

# Read the data
deviations <- read.csv(deviations_file)
randomization_offsets <- read.csv(randomization_offsets_file)
imbalanced_pcrs <- read.csv(imbalanced_pcrs_file)
missing_subjects <- read.csv(missing_subjects_file)
trial_sites_data <- fromJSON(trial_sites_file, simplifyDataFrame = TRUE)

# Create deviations_unique_sites containing unique SITEID from deviations
deviations_unique_sites <- deviations %>%
  select(SITEID) %>%
  distinct()

# Create imbalanced_pcrs_unique_sites containing unique SITEID from imbalanced_pcrs
imbalanced_pcrs_unique_sites <- imbalanced_pcrs %>%
  select(SITEID) %>%
  distinct()

# Rename columns to "SITEID"
randomization_offsets <- randomization_offsets %>%
  rename(SITEID = Trial.Site.ID)
randomization_offsets <- randomization_offsets %>%
  rename(OffsetRando = Offset.M6.FA)
trial_sites_data <- trial_sites_data %>%
  rename(SITEID = trial_site_id)
missing_subjects <- missing_subjects %>%
  rename(SITEID = trial_site_id)

# Convert SITEID columns to character type to ensure compatibility
trial_sites_data$SITEID <- as.character(trial_sites_data$SITEID)
randomization_offsets$SITEID <- as.character(randomization_offsets$SITEID)
deviations_unique_sites$SITEID <- as.character(deviations_unique_sites$SITEID)
imbalanced_pcrs_unique_sites$SITEID <- as.character(imbalanced_pcrs_unique_sites$SITEID)
missing_subjects$SITEID <- as.character(missing_subjects$SITEID)

# Add HASDEV column
trial_sites_data <- trial_sites_data %>%
  mutate(HASDEV = ifelse(SITEID %in% deviations_unique_sites$SITEID, 1, 0))

# Add HASPCRIMBALANCE column
trial_sites_data <- trial_sites_data %>%
  mutate(HASPCRIMBALANCE = ifelse(SITEID %in% imbalanced_pcrs_unique_sites$SITEID, 1, 0))

# Add Offset.M6.FA column from randomization_offsets
trial_sites_data <- trial_sites_data %>%
  left_join(randomization_offsets %>% select(SITEID, OffsetRando), by = "SITEID")

# Add total_missing column from missing_subjects
trial_sites_data <- trial_sites_data %>%
  left_join(missing_subjects %>% select(SITEID, total_missing), by = "SITEID")

# Replace NA values in total_missing with 0
trial_sites_data <- trial_sites_data %>%
  mutate(total_missing = replace_na(total_missing, 0))

# Print updated trial_sites_data
print(trial_sites_data)






# Loads the template
template <- readLines("sites_synthesis_template.html")

# Initializes the table rows
table_rows <- ""

clean <- 0
total <- 0
# Iterates over each subject and add their HTML data
for (i in 1:nrow(trial_sites_data)) {
  total <- total + 1
  is_clean <- 1
  row <- trial_sites_data[i, ]
  SITEID <- trial_sites_data$SITEID[i]
  state <- trial_sites_data$state[i]
  if (is.na(state)) {
    state <- trial_sites_data$country[i]
  }
  investigator <- trial_sites_data$investigator[i]
  table_rows <- paste0(table_rows, "
      <tr>
        <td>", SITEID, "</td>
        <td>", state, "</td>
        <td>", investigator, "</td>")
  print(total_missing)
  total_missing <- trial_sites_data$total_missing[i]
  HASDEV <- trial_sites_data$HASDEV[i]
  HASPCRIMBALANCE <- trial_sites_data$HASPCRIMBALANCE[i]
  OffsetRando <- trial_sites_data$OffsetRando[i]
  if (HASDEV == 0) {
    table_rows <- paste0(table_rows, "
        <td></td>
  ")
  } else {
    is_clean <- 0
    table_rows <- paste0(table_rows, "
        <td style=\"background:#fa8072\">Yes</td>
  ")
  }
  if (HASPCRIMBALANCE == 0) {
    table_rows <- paste0(table_rows, "
        <td></td>
  ")
  } else {
    is_clean <- 0
    table_rows <- paste0(table_rows, "
        <td style=\"background:#fa8072\">Yes</td>
  ")
  }
  if (OffsetRando >= 0) {
    table_rows <- paste0(table_rows, "
        <td>", OffsetRando, "</td>
  ")
  } else {
    is_clean <- 0
    table_rows <- paste0(table_rows, "
        <td style=\"background:#fa8072\">", OffsetRando, "</td>
  ")
  }
  if (total_missing == 0) {
    table_rows <- paste0(table_rows, "
        <td>", total_missing, "</td>
  ")
  } else {
    is_clean <- 0
    table_rows <- paste0(table_rows, "
        <td style=\"background:#fa8072\">", total_missing, "</td>
  ")
  }
  if (is_clean) {
    clean <- clean + 1
  }
  table_rows <- paste0(table_rows, "
      </tr>")
}

# Replaces the <--TABLE_ROWS--> placeholder with the actual table rows
template <- gsub("<--TABLE_ROWS-->", table_rows, template)

# Prints the resulting HTML to a file
writeLines(template, "sites_synthesis.html")

print(paste('Sites clean : ', clean, '/', total))
