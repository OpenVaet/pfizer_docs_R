library(readr)
library(dplyr)

# Initialize the sites data
subjects <- list()

# Read and process the adsl data
adsl_data <- read.csv('csv_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.csv', sep = ',', stringsAsFactors = FALSE)

# Initialize a vector to store the subjid of not assigned or screen failure subjects
not_assigned_or_screen_failure <- c()

for(i in 1:nrow(adsl_data)){
  subjid <- as.character(adsl_data[i, "SUBJID"])
  arm <- as.character(adsl_data[i, "ARM"])
  if (arm == "NOT ASSIGNED" || arm == "SCREEN FAILURE") {
    not_assigned_or_screen_failure <- c(not_assigned_or_screen_failure, subjid)
  }
}

# Convert the vector to a data frame
df <- data.frame("SUBJID" = not_assigned_or_screen_failure)

# Write the data frame to a CSV file
write.csv(df, 'not_assigned_screen_failure.csv', row.names = FALSE)
