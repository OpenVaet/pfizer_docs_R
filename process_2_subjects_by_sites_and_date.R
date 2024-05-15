library(haven)
library(dplyr)
library(lubridate)
library(stringr)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)

print(randomized_pop)

# Extracts the trial_site_id (the site which actually recruited the subject in the study - not the current trial site)
randomized_pop$ORISITEID <- as.numeric(sub("(....)....", "\\1", randomized_pop$SUBJID))

print(randomized_pop)

filtered_data <- randomized_pop %>%
  filter(VAX101DT >= "2020-10-19", 
         ORISITEID %in% c(1133, 1135, 1146, 1170))

print(filtered_data)

# Writes filtered_data to a new CSV file
write.csv(filtered_data, "process_2_recipients_by_date_and_sites.csv", row.names = FALSE)


agegr1_counts <- filtered_data %>%
  count(ARM, AGEGR1)

print(agegr1_counts)
