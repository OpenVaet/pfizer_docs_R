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
sympto_pcrs_file <- 'sites_symptomatic_positive_pcrs.csv'
trial_sites_file <- 'trial_site_data.json'

# Read the data
deviations <- read.csv(deviations_file)
randomization_offsets <- read.csv(randomization_offsets_file)
imbalanced_pcrs <- read.csv(imbalanced_pcrs_file)
missing_subjects <- read.csv(missing_subjects_file)
sympto_pcrs <- read.csv(sympto_pcrs_file)
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

# Calculate total_missing for SITEID 1231
total_missing_1231 <- missing_subjects %>%
  filter(SITEID == 1231) %>%
  summarize(total_missing = sum(total_missing)) %>%
  pull(total_missing)
print(total_missing_1231)

# Calculate total_missing for SITEID 4444
total_missing_4444 <- missing_subjects %>%
  filter(SITEID == 4444) %>%
  summarize(total_missing = sum(total_missing)) %>%
  pull(total_missing)
print(total_missing_4444)

# Replace the existing total_missing value for SITEID 1231
missing_subjects <- missing_subjects %>%
  mutate(total_missing = ifelse(SITEID == 1231, total_missing_1231 + total_missing_4444, total_missing))

# Delete the row with SITEID 4444
missing_subjects <- missing_subjects %>%
  filter(SITEID != 4444)


print(sympto_pcrs)
print(missing_subjects)
print(sum(missing_subjects$total_missing))
print(randomization_offsets)

# Convert SITEID columns to character type to ensure compatibility
trial_sites_data$SITEID <- as.character(trial_sites_data$SITEID)
randomization_offsets$SITEID <- as.character(randomization_offsets$SITEID)
deviations_unique_sites$SITEID <- as.character(deviations_unique_sites$SITEID)
imbalanced_pcrs_unique_sites$SITEID <- as.character(imbalanced_pcrs_unique_sites$SITEID)
missing_subjects$SITEID <- as.character(missing_subjects$SITEID)
sympto_pcrs$SITEID <- as.character(sympto_pcrs$SITEID)

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

# Add symptomatic PCR columns from sympto_pcrs
trial_sites_data <- trial_sites_data %>%
  left_join(sympto_pcrs %>% select(SITEID, PLACEBONEGBASELINE, PLACEBOPOS, BNTNEGBASELINE, BNTPOS), by = "SITEID")

# Replace NA values in total_missing with 0
trial_sites_data <- trial_sites_data %>%
  mutate(total_missing = replace_na(total_missing, 0))
print(sum(trial_sites_data$total_missing))
print(sort(unique(sympto_pcrs$SITEID)))
print(sort(unique(randomization_offsets$SITEID)))

# Filters out site 1002 (no subject negative at baseline)
# trial_sites_data <- trial_sites_data %>% 
#   filter(SITEID != "1002")

# Print updated trial_sites_data
print(trial_sites_data)

# Initialize variables for pagination
rows_per_page <- 33
page_number <- 1
row_counter <- 0
table_rows <- ""

clean <- 0
total <- 0

# Initialize counters for totals and clean totals
total_PLACEBONEGBASELINE <- 0
total_PLACEBOPOS <- 0
total_BNTNEGBASELINE <- 0
total_BNTPOS <- 0
total_HASDEV <- 0
total_HASPCRIMBALANCE <- 0
total_OffsetRando <- 0
total_total_missing <- 0

clean_PLACEBONEGBASELINE <- 0
clean_PLACEBOPOS <- 0
clean_BNTNEGBASELINE <- 0
clean_BNTPOS <- 0


# Calculate the totals and clean totals across all pages
total_BNTNEGBASELINE <- sum(trial_sites_data$BNTNEGBASELINE)
total_PLACEBONEGBASELINE <- sum(trial_sites_data$PLACEBONEGBASELINE)
total_BNTPOS <- sum(trial_sites_data$BNTPOS)
total_PLACEBOPOS <- sum(trial_sites_data$PLACEBOPOS)
total_total_missing <- sum(trial_sites_data$total_missing)
total_OffsetRando <- sum(ifelse(!is.na(trial_sites_data$OffsetRando) & trial_sites_data$OffsetRando < 0, trial_sites_data$OffsetRando, 0))

clean_BNTNEGBASELINE <- sum(trial_sites_data$BNTNEGBASELINE[trial_sites_data$HASDEV == 1 & trial_sites_data$HASPCRIMBALANCE == 1 & trial_sites_data$OffsetRando >= 0 & trial_sites_data$total_missing == 0])
clean_PLACEBONEGBASELINE <- sum(trial_sites_data$PLACEBONEGBASELINE[trial_sites_data$HASDEV == 1 & trial_sites_data$HASPCRIMBALANCE == 1 & trial_sites_data$OffsetRando >= 0 & trial_sites_data$total_missing == 0])
clean_BNTPOS <- sum(trial_sites_data$BNTPOS[trial_sites_data$HASDEV == 1 & trial_sites_data$HASPCRIMBALANCE == 1 & trial_sites_data$OffsetRando >= 0 & trial_sites_data$total_missing == 0])
clean_PLACEBOPOS <- sum(trial_sites_data$PLACEBOPOS[trial_sites_data$HASDEV == 1 & trial_sites_data$HASPCRIMBALANCE == 1 & trial_sites_data$OffsetRando >= 0 & trial_sites_data$total_missing == 0])

# Function to write HTML file
write_html_file <- function(page_number, table_rows, include_totals = FALSE) {
  template <- readLines("sites_synthesis_template.html")
  if (include_totals) {
    # Create totals row
    totals_row <- paste0("
      <tr>
        <td>Total</td>
        <td></td>
        <td>", total_BNTNEGBASELINE, "</td>
        <td>", total_PLACEBONEGBASELINE, "</td>
        <td></td>
        <td></td>
        <td>", total_OffsetRando, "</td>
        <td>", total_total_missing, "</td>
        <td>", total_BNTPOS, "</td>
        <td>", total_PLACEBOPOS, "</td>
      </tr>")
    
    # Create clean totals row
    clean_totals_row <- paste0("
      <tr>
        <td>Clean</td>
        <td></td>
        <td>", clean_BNTNEGBASELINE, "</td>
        <td>", clean_PLACEBONEGBASELINE, "</td>
        <td></td>
        <td></td>
        <td></td>
        <td></td>
        <td>", clean_BNTPOS, "</td>
        <td>", clean_PLACEBOPOS, "</td>
      </tr>")
    
    content <- gsub("<--TABLE_ROWS-->", paste0(totals_row, clean_totals_row, table_rows), template)
  } else {
    content <- gsub("<--TABLE_ROWS-->", table_rows, template)
  }
  writeLines(content, paste0("sites_synthesis_", page_number, ".html"))
}

# Write HTML file with totals
write_html_file(page_number, table_rows, include_totals = TRUE)

print(paste('Sites clean : ', clean, '/', total))

# Iterates over each subject and add their HTML data
for (i in 1:nrow(trial_sites_data)) {
  total <- total + 1
  is_clean <- 1
  row <- trial_sites_data[i, ]
  SITEID <- trial_sites_data$SITEID[i]
  if (SITEID == 1002) next
  PLACEBONEGBASELINE <- trial_sites_data$PLACEBONEGBASELINE[i]
  PLACEBOPOS <- trial_sites_data$PLACEBOPOS[i]
  BNTNEGBASELINE <- trial_sites_data$BNTNEGBASELINE[i]
  BNTPOS <- trial_sites_data$BNTPOS[i]
  state <- trial_sites_data$state[i]
  if (is.na(state)) {
    state <- trial_sites_data$country[i]
  }
  investigator <- trial_sites_data$investigator[i]
  table_rows <- paste0(table_rows, "
      <tr>
        <td>", SITEID, "</td>
        <td>", state, "</td>
        <td>", BNTNEGBASELINE, "</td>
        <td>", PLACEBONEGBASELINE, "</td>")
  total_missing <- trial_sites_data$total_missing[i]
  print(total_missing)
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
        <td></td>
  ")
  } else {
    is_clean <- 0
    table_rows <- paste0(table_rows, "
        <td style=\"background:#fa8072\">", OffsetRando, "</td>
  ")
  }
  if (total_missing == 0) {
    table_rows <- paste0(table_rows, "
        <td></td>
  ")
  } else {
    is_clean <- 0
    table_rows <- paste0(table_rows, "
        <td style=\"background:#fa8072\">-", total_missing, "</td>
  ")
  }
  if (is_clean) {
    clean <- clean + 1
    clean_PLACEBONEGBASELINE <- clean_PLACEBONEGBASELINE + PLACEBONEGBASELINE
    clean_PLACEBOPOS <- clean_PLACEBOPOS + PLACEBOPOS
    clean_BNTNEGBASELINE <- clean_BNTNEGBASELINE + BNTNEGBASELINE
    clean_BNTPOS <- clean_BNTPOS + BNTPOS
  }
  table_rows <- paste0(table_rows, "
        <td>", BNTPOS, "</td>
        <td>", PLACEBOPOS, "</td>")
  table_rows <- paste0(table_rows, "
      </tr>")
  
  row_counter <- row_counter + 1
  
  # If the page is full, write the HTML file
  if (row_counter == rows_per_page) {
    include_totals <- page_number == 1
    write_html_file(page_number, table_rows, include_totals)
    
    # Reset for the next page
    page_number <- page_number + 1
    row_counter <- 0
    table_rows <- ""
  }
}

# If there are remaining rows, write the last page
if (row_counter > 0) {
  include_totals <- page_number == 1
  write_html_file(page_number, table_rows, include_totals)
}

print(paste('Sites clean : ', clean, '/', total))
