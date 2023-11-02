# Load necessary libraries
library(haven)
library(dplyr)

# Load the list of targeted SUBJID
target_subjids <- read.csv("over_N_xpt_lost.csv")

# Get a list of all XPT files in the xpt_data folder
files <- list.files(path = "xpt_data", pattern = "*.xpt", full.names = TRUE)

# Create the output directory if it doesn't exist
if (!dir.exists("over_N_xpt_files_non_randomized")) {
  dir.create("over_N_xpt_files_non_randomized")
}

text_output_file <- "over_N_xpt_text_subjects_log.txt"
if (file.exists(text_output_file)) {
  file.remove(text_output_file)
}

# Process each file
for (file in files) {
  print(paste(file));
  
  # Skip the specified file
  if (file == "xpt_data/FDA-CBER-2021-5683-0066701 to -0123167_125742_S1_M5_c4591001-A-D-adfacevd.xpt") {
    next
  }
  
  # Read the XPT file
  data <- read_xpt(file)
  
  # Check if the file has a SUBJID or USUBJID header
  if ("SUBJID" %in% names(data)) {
    # Filter the data for targeted subjects
    found <- data[data$SUBJID %in% target_subjids$SUBJID, ]
  } else if ("USUBJID" %in% names(data)) {
    # Extract the SUBJID from the USUBJID
    data <- data %>% mutate(SUBJID = sub("^C4591001 .... (........)$", "\\1", USUBJID))
    # Filter the data for targeted subjects
    found <- data[data$SUBJID %in% target_subjids$SUBJID, ]
  } else {
    # If the file does not have a SUBJID or USUBJID header, skip it
    next
  }
  
  # If any targeted subjects were found, write them to a text file and a CSV file
  if (nrow(found) > 0) {
    # Write to the text file
    write(paste("File:", file), file = text_output_file, append = TRUE)
    write.table(found, file = text_output_file, append = TRUE, row.names = FALSE, col.names = TRUE)
    
    # Get the base name of the file (without the path or extension)
    base_name <- basename(file)
    base_name <- sub("\\.xpt$", "", base_name)
    
    # Create the output file name
    output_file <- paste0("over_N_xpt_files_non_randomized/", base_name, ".csv")
    
    # Write to the CSV file
    write.csv(found, file = output_file, row.names = FALSE)
  }
}
