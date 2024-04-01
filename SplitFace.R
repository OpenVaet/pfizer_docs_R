# Extracts ZIP to XPT.
library(tools)
library(zip)
library(httr)
library(rvest)
library(xml2)

# Verifies if the ADSL file has been properly retrieved.
file_path <- 'xpt_data/FDA-CBER-2021-5683-0484461-0537913-125742_S1_M5_c4591001-S-D-face.xpt'
if (!file.exists(file_path)) {
  stop("FACE file not found", call. = FALSE)
}

# Reads the XPT file
library(haven)
data <- read_xpt(file_path)

# Defines the chunk size
chunk_size <- 50000

# Splits the data into chunks and write to CSV files
num_chunks <- ceiling(nrow(data) / chunk_size)
for (i in 1:num_chunks) {
  start_row <- (i - 1) * chunk_size + 1
  end_row <- min(i * chunk_size, nrow(data))
  chunk <- data[start_row:end_row, ]
  csv_filename <- paste0("face_", i, ".csv")
  write.csv(chunk, file = csv_filename, row.names = FALSE)
}