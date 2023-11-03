# Load necessary libraries
library(haven)
library(dplyr)

# Load the list of targeted SUBJID
target_subjids <- read.csv("not_assigned_screen_failure.csv")

# Initialize a data frame to store results
results <- data.frame(file = character(), count = integer())
found_subjects <- data.frame(file = character(), SUBJID = character())

# Get a list of all XPT files in the xpt_data folder
files <- list.files(path = "xpt_data", pattern = "*.xpt", full.names = TRUE)

# Process each file
for (file in files) {
  print(paste(file));
  # Skip the heaviest file (already checked for being void, comment if you want to check)
  if (file == "xpt_data/FDA-CBER-2021-5683-0066701 to -0123167_125742_S1_M5_c4591001-A-D-adfacevd.xpt") {
    next
  }
  
  # Read the XPT file
  data <- read_xpt(file)
  
  # Check if the file has a SUBJID or USUBJID header
  if ("SUBJID" %in% names(data)) {
    # Count the number of subjects found in the file
    count <- sum(data$SUBJID %in% target_subjids$SUBJID)
    found <- data$SUBJID[data$SUBJID %in% target_subjids$SUBJID]
  } else if ("USUBJID" %in% names(data)) {
    # Extract the SUBJID from the USUBJID
    data <- data %>% mutate(SUBJID = sub("^C4591001 .... (........)$", "\\1", USUBJID))
    # Count the number of subjects found in the file
    count <- sum(data$SUBJID %in% target_subjids$SUBJID)
    found <- data$SUBJID[data$SUBJID %in% target_subjids$SUBJID]
  } else {
    # If the file does not have a SUBJID or USUBJID header, skip it
    next
  }
  
  # Add the results to the results data frame
  results <- rbind(results, data.frame(file = file, count = count))
  found_subjects <- rbind(found_subjects, data.frame(file = rep(file, length(found)), SUBJID = found))
}

# Write the results to a CSV file
write.csv(results, "na_subjects_found_in_xpt.csv", row.names = FALSE)
write.csv(found_subjects, "na_subjects_found.csv", row.names = FALSE)
