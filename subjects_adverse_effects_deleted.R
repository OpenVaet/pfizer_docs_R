# Load necessary package
library(haven)
library(dplyr)
library(tidyr)

# Reads the XPT file & retains data required for analysis.
adae_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt')
print(adae_data)
print(colnames(adae_data))
adae_data_selected <- adae_data[c("USUBJID", "AESPID", "AESTDTC")]

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
adae_data_selected$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adae_data_selected$USUBJID)
write.csv(adae_data_selected, "adae_data_selected.csv", row.names = FALSE)
print(adae_data_selected)

# Initialize counters
skipped_empty_aespid <- 0

# Initialize an empty list of lists
data_list <- list()

# Process the data
for (i in 1:nrow(adae_data_selected)) {
  aespid <- adae_data_selected[i, "AESPID"]
  subjid <- as.character(adae_data_selected[i, "SUBJID"])
  if (is.null(aespid) || aespid == '') {
    skipped_empty_aespid <- skipped_empty_aespid + 1
    next
  }
  aespid <- as.character(aespid)
  
  # Check if the subject ID is already in the list
  if (!(subjid %in% names(data_list))) {
    data_list[[subjid]] <- list()
  }
  
  # Add the AESPID to the subject ID's list
  data_list[[subjid]][[as.character(aespid)]] <- 1
}
print(data_list)

cat("skipped_empty_aespid : ", skipped_empty_aespid, "\n")

# Create a data frame to store the missing AESPID combinations
missing_aespid <- 0
missing_aespid_df <- data.frame()

for (subjid in names(data_list)) {
  aespid_values <- names(data_list[[subjid]])
  max_aespid <- max(as.integer(aespid_values))
  if (is.null(max_aespid)) {
    stop("max_aespid is null")
  }
  for (aespid in 1:max_aespid) {
    if (!(as.character(aespid) %in% aespid_values)) {
      missing_aespid <- missing_aespid + 1
      cat("missing on [", subjid, "] : ", aespid, "\n")
      missing_aespid_df <- rbind(missing_aespid_df, data.frame(SUBJID = subjid, AESPID = aespid))
    }
  }
}

# Write the missing AESPID combinations to a CSV file
write.csv(missing_aespid_df, "missing_aespid.csv", row.names = FALSE)
cat("missing_aespid : ", missing_aespid, "\n")
