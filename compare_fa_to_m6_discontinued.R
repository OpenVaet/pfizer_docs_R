# Load necessary library
library(dplyr)

# Read the CSV files
m6 <- read.csv("discontinued_subjects_m6.csv")
fa <- read.csv("discontinued_subjects_fa.csv")

# Find subjects in FA that are not in M6
dediscontinued <- fa %>%
  filter(!FA_SUBJID %in% m6$M6_SUBJID)

# Write the result to a new CSV file
write.csv(dediscontinued, "dediscontinued.csv", row.names = FALSE)
