# Load necessary package
library(haven)
library(dplyr)
library(readr)
library(jsonlite)

# Loading trial sites data.
trial_sites_file <- 'trial_site_data.json'
trial_sites_data <- fromJSON(trial_sites_file, simplifyDataFrame = TRUE)

# Check the structure of the JSON data
str(trial_sites_data)

# Read the phase 1 subjects data to get the list of subject IDs
phase_1_subjects <- read.csv("phase_1_subjects_adsl_data.csv")
phase_1_subjids <- phase_1_subjects$SUBJID

# If trial_sites_data is not a data frame, you may need to perform additional steps to convert it to a data frame
# For example, if trial_sites_data is a list of lists, you might need to use a function like bind_rows() to combine them into a data frame
if (!is.data.frame(trial_sites_data)) {
  trial_sites_data <- bind_rows(trial_sites_data)
}

# Read the XPT file
adsl_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt')

# Filter the ADSL data to include only subjects present in the phase 1 subjects data
adsl_data_filtered <- adsl_data %>% 
  filter(SUBJID %in% phase_1_subjids)

# Extract recruitment site id and convert 4444 to 1231
adsl_data_filtered <- adsl_data_filtered %>% 
  mutate(ORISITEID = substr(SUBJID, 1, 4)) %>%
  mutate(ORISITEID = ifelse(ORISITEID == "4444", "1231", ORISITEID))

# Count the total of subjects screened, by site
subjects_screened_by_site <- adsl_data_filtered %>%
  group_by(ORISITEID) %>%
  summarise(Subjects_Screened = n(), .groups = 'drop')

# Ensure subjects_screened_by_site is a data frame
subjects_screened_by_site <- as.data.frame(subjects_screened_by_site)

# Print out column names to confirm they are correct
print(colnames(subjects_screened_by_site))
print(colnames(trial_sites_data))

# Join the trial sites data with the subjects screened by site
# Make sure that the column names used for joining are correct and present in both data frames
sites_frame <- merge(subjects_screened_by_site, trial_sites_data, by.x = "ORISITEID", by.y = "trial_site_id", all.x = TRUE)

# Select the relevant columns (assuming 'country' and 'name' are the correct column names in your JSON file)
sites_frame <- sites_frame %>%
  select(ORISITEID, Country = country, Site_Name = name, Investigator = investigator, Latitude = latitude, Longitude = longitude, Subjects_Screened)

# Write the data to a CSV file
write.csv(sites_frame, "subjects_screened_by_sites_phase1.csv", row.names = FALSE)

# Count the total of unique entries in "SITEID"
unique_siteid_count <- length(unique(adsl_data_filtered$SITEID))
print(paste("Total unique SITEID entries: ", unique_siteid_count))

# Count the number of subjects in each arm
arm_counts <- table(adsl_data_filtered$ARM)
print(arm_counts)
