library(haven)
library(dplyr)

# Defines the path to the sub-folder containing .xpt files
xpt_path <- "xpt_data_12_15"

target_subjid <- 10071620

# Gets a list of all .xpt files in the directory
xpt_files <- list.files(path = xpt_path, pattern = "\\.xpt$", full.names = TRUE)

# Creates or open the file to write the names of files with SUBJID = target_subjid
output_file <- paste0("all_fields_", target_subjid, "_12_15.txt")
fileConn <- file(output_file, open = "wt")

# Loops through each .xpt file
for (file in xpt_files) {
  # Read the .xpt file
  df <- read_xpt(file)
  
  # Checks if 'SUBJID' exists
  if ("SUBJID" %in% names(df)) {
    # If SUBJID exists, check if any row has SUBJID = target_subjid
    if (any(df$SUBJID == target_subjid)) {
      # Write the file name to the output file
      writeLines(paste("File:", basename(file)), fileConn)
      
      # Captures rows where SUBJID = target_subjid
      matching_rows <- df %>% filter(SUBJID == target_subjid)
      
      # Writes each row to the output file
      for (i in 1:nrow(matching_rows)) {
        # Converts the row to a character vector
        row_text <- paste(names(matching_rows), matching_rows[i, ], sep = ": ", collapse = ", ")
        writeLines(row_text, fileConn)
      }
      writeLines("", fileConn)
    }
  } else if ("USUBJID" %in% names(df)) {
    # If SUBJID does not exist but USUBJID does, create SUBJID from USUBJID
    df$SUBJID <- substr(df$USUBJID, 15, 24)
    
    # Checks if any row has SUBJID = target_subjid
    if (any(df$SUBJID == target_subjid)) {
      # Write the file name to the output file
      writeLines(paste("File:", basename(file)), fileConn)
      
      # Captures rows where SUBJID = target_subjid
      matching_rows <- df %>% filter(SUBJID == target_subjid)
      
      # Writes each row to the output file
      for (i in 1:nrow(matching_rows)) {
        # Convert the row to a character vector
        row_text <- paste(names(matching_rows), matching_rows[i, ], sep = ": ", collapse = ", ")
        writeLines(row_text, fileConn)
      }
      writeLines("", fileConn)
    }
  }
}

# Closes the connection to the output file
close(fileConn)
