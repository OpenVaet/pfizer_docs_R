# Load the necessary library
library(haven)

# Get a list of all .xpt files in the "xpt_data" sub-folder
files <- list.files(path = "xpt_data", pattern = "\\.xpt$")

# Create ssv_data directory if it doesn't exist
dir <- "ssv_data"
if(!dir.exists(dir)){
  dir.create(dir)
}

# Loop over the files
for (file in files) {
  # Create the .CSV file name by replacing the .xpt extension with .CSV
  csv_file <- sub("\\.xpt$", ".csv", file)
  
  # Check if the CSV file already exists
  if(!file.exists(paste0(dir, "/", csv_file))){
    # Read the .xpt file from the "xpt_data" sub-folder
    data <- read_xpt(paste0("xpt_data/", file))
    
    # Write the data to a .CSV file in the "ssv_data" sub-folder
    write.csv2(data, paste0(dir, "/", csv_file), row.names = TRUE)
  }
}
