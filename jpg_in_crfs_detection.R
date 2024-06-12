# Load necessary libraries
library(jsonlite)
library(httr)
library(rvest)
library(stringi)
library(tools)
library(fs)
library(dplyr)
library(knitr)
library(htmltools)
library(DT)
library(tesseract)
library(magick)
library(pdftools)
library(rmarkdown)
library(flextable)
library(lubridate)

# You'll need the XPDF version corresponding to your OS.
# Both files below are coming from https://www.xpdfreader.com/download.html
# Place the "pdftohtml.exe" (windows) or "pdftohtml" (linux) file,
# located in the bin32/64 subfolder of the archive you downloaded,
# in your project repository.

# Define the path to the pdftohtml executable
pdf_to_html_executable <- 'pdftohtml' # Use appropriate executable name for your OS

# Create the output directory if it does not exist
dir_create('crf_html')

pages_replaced <- list()

# Data frame to store information about skipped files
skipped_files <- data.frame(file = character(), size_mb = numeric(), stringsAsFactors = FALSE)

# Iterate over the PDF files
pdf_files <- dir_ls('pdf_data', glob = '*_CRF_*.pdf')

total_files_parsed <- 0
files_with_anomalies <- 0
for (file in pdf_files) {
  file_e <- strsplit(file, '-')[[1]]
  subjid <- file_e[length(file_e)]
  
  if (subjid == 'reissue.pdf') {
    subjid <- file_e[length(file_e) - 1]
  } else if (grepl('reissue', subjid)) {
    subjid <- strsplit(subjid, ' ')[[1]][1]
  }
  
  subjid <- sub('\\.pdf$', '', subjid)
  
  if (!grepl('^\\d{8}$', subjid)) {
    stop('Invalid subject ID format.')
  }
  
  cat("Processing file:", file, "\n")
  cat("Subject ID:", subjid, "\n")
  
  # Get the file size in MB
  file_size_bytes <- file_info(file)$size
  file_size_mb <- file_size_bytes / (1024 * 1024)
  file_size_mb <- round(file_size_mb, 2)
  if (file_size_mb > 15) {
    cat("Skipping file [", file, "] (", file_size_mb, " MB)\n", sep = "")
    # Add skipped file information to the data frame
    skipped_files <- rbind(skipped_files, data.frame(file = file, size_mb = file_size_mb, stringsAsFactors = FALSE))
    next
  }
  total_files_parsed <- total_files_parsed + 1
  cat("File size:", file_size_mb, "MB\n")
  
  output_folder <- file.path('crf_html', subjid)
  
  if (!dir_exists(output_folder)) {
    # Convert PDF to HTML
    cmd <- paste(pdf_to_html_executable, shQuote(file), shQuote(output_folder))
    cat("Running command:", cmd, "\n")
    
    # Capture both stdout and stderr
    system_result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
    cat("Command output:\n", paste(system_result, collapse = "\n"), "\n")
  }
  
  # Ensure the output folder is created and contains HTML files
  if (!dir_exists(output_folder)) {
    cat("Output folder does not exist:", output_folder, "\n")
    next
  }
  
  # List HTML pages using list.files
  html_pages <- list.files(output_folder, pattern = 'page.*\\.html$', full.names = TRUE)
  
  if (length(html_pages) == 0) {
    cat("No HTML pages found for", subjid, "\n")
    break
  }
  
  has_anomalies <- 0
  for (page in html_pages) {
    html <- read_html(page)
    
    # Extract the visible text content
    text <- html %>%
      html_nodes(xpath = "//body//text()[normalize-space()]") %>%
      html_text(trim = TRUE) %>%
      paste(collapse = " ")
    
    text <- stri_trim_both(text)
    text_length <- nchar(text)
    page_num <- stri_match_first_regex(page, 'page(\\d+).html')[, 2]
    
    if (text_length < 50) {
      has_anomalies <- 1
      if (is.null(pages_replaced[[subjid]])) {
        pages_replaced[[subjid]] <- list()
      }
      pages_replaced[[subjid]][[page_num]] <- text_length
      cat('---->', file, '->', page_num, '|', text_length, "\n")
    }
  }
  
  if (has_anomalies == 1) {
    files_with_anomalies <- files_with_anomalies + 1
  }
}

# Write the results to a CSV file
output_file <- 'jpg_in_crfs.csv'
file_conn <- file(output_file, 'w', encoding = 'UTF-8')
writeLines("subjid,page_num,text_length", file_conn)  # Add header to CSV

for (subjid in sort(names(pages_replaced))) {
  for (page in sort(names(pages_replaced[[subjid]]))) {
    text_length <- pages_replaced[[subjid]][[page]]
    writeLines(paste(subjid, page, text_length, sep = ','), file_conn)
  }
}

close(file_conn)

# Write the skipped files information to a CSV file
skipped_output_file <- 'jpg_full_high_size.csv'
write.csv(skipped_files, skipped_output_file, row.names = FALSE)
print(skipped_files)
skipped_html_table <- flextable(skipped_files) %>%
  set_header_labels(
    "file" = "File",
    "size_mb" = "Size (Mb)"
  ) %>%
  theme_zebra() %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Skipped files (high size)")
save_as_html(skipped_html_table, path = "crf_skipped_files_table.html")

# Print skipped files information
cat("Skipped files due to size:\n")
print(skipped_files)

cat("Total files abnormal : ", files_with_anomalies)
cat("Total files processed : ", total_files_parsed)

# Read jpg_in_crfs.csv
jpg_in_crfs <- read.csv('jpg_in_crfs.csv', stringsAsFactors = FALSE)

# Read phase_3_randomized_pop.csv
phase_3_randomized_pop <- read.csv('phase_3_randomized_pop.csv', stringsAsFactors = FALSE)

# Change the column name from subjid to SUBJID
colnames(jpg_in_crfs)[colnames(jpg_in_crfs) == "subjid"] <- "SUBJID"

# Merge the dataframes based on SUBJID, keeping only the entries in jpg_in_crfs
merged_data <- merge(jpg_in_crfs, phase_3_randomized_pop, by = "SUBJID")

print(merged_data)

# Write the merged data to a new CSV file
write.csv(merged_data, 'merged_output.csv', row.names = FALSE)

# Calculate total number of rows for each ARM
total_rows_per_arm <- merged_data %>%
  group_by(ARM) %>%
  summarise(total_rows = n())

# Calculate total number of unique SUBJID for each ARM
unique_subjid_per_arm <- merged_data %>%
  group_by(ARM) %>%
  summarise(unique_subjid = n_distinct(SUBJID))

# Print results
print(total_rows_per_arm)
print(unique_subjid_per_arm)
print(jpg_in_crfs)

# Create a new column with the URL
jpg_in_crfs <- jpg_in_crfs %>%
  mutate(URL = paste0("crf_html/", SUBJID, "/page", page_num, ".html"))

# Generate the HTML table using DT
datatable_content <- jpg_in_crfs %>%
  mutate(Link = paste0('<a target="_blank" href="', URL, '">', URL, '</a>')) %>%
  select(SUBJID, page_num, Link)

# Create a DT table
datatable_object <- datatable(
  datatable_content,
  escape = FALSE,
  options = list(
    pageLength = 100,
    autoWidth = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = '_all'))
  ),
  rownames = FALSE
)

# Save the HTML content to a file
html_file <- "filtered_pages.html"
saveWidget(datatable_object, file = html_file, selfcontained = TRUE)

# Print a message indicating the file has been created
cat("HTML file created:", html_file, "\n")
print(jpg_in_crfs)

# Generate URLs for PNG files
jpg_in_crfs <- jpg_in_crfs %>%
  mutate(PNG_URL = paste0("crf_html/", SUBJID, "/page", page_num, ".png"))

# Perform OCR on PNG files and create corresponding text files
for (i in seq(nrow(jpg_in_crfs))) {
  subj_id <- jpg_in_crfs$SUBJID[i]
  page_num <- jpg_in_crfs$page_num[i]
  png_url <- jpg_in_crfs$PNG_URL[i]
  output_txt <- paste0("crf_html/", subj_id, "/page", page_num, "_png.txt")
  
  # Check if the text file already exists
  if (!file.exists(output_txt)) {
    # Perform OCR
    ocr_text <- ocr(png_url)
    
    # Write OCR output to text file
    cat(ocr_text, file = output_txt)
    cat("OCR performed and saved to", output_txt, "\n")
  } else {
    cat("Text file already exists for", output_txt, "\n")
  }
}

# Function to extract Generated Time (GMT) from text content
extract_generated_time <- function(txt_content) {
  gmt_line <- grep("Generated Time \\(GMT\\):", txt_content, value = TRUE)
  if (length(gmt_line) > 0) {
    gmt_value <- sub("Generated Time \\(GMT\\): (.*) Cn", "\\1", gmt_line)
    return(gmt_value)
  } else {
    return(NA)
  }
}

# Function to extract Form from text content
extract_form <- function(txt_content) {
  form_line <- grep("Form: ", txt_content, value = TRUE)
  if (length(form_line) > 0) {
    form_value <- sub(".*Form: (.*)", "\\1", form_line)
    return(form_value)
  } else {
    return(NA)
  }
}

# Read each generated text file and extract Generated Time (GMT)
extracted_data <- character(nrow(jpg_in_crfs))
gmt_time <- character(nrow(jpg_in_crfs))
form_data <- character(nrow(jpg_in_crfs))
for (i in seq(nrow(jpg_in_crfs))) {
  subj_id <- jpg_in_crfs$SUBJID[i]
  page_num <- jpg_in_crfs$page_num[i]
  txt_file <- paste0("crf_html/", subj_id, "/page", page_num, "_png.txt")
  
  # Read text file
  txt_content <- readLines(txt_file)
  
  # Extract Generated Time (GMT)
  extracted_time <- extract_generated_time(txt_content)
  extracted_data[i] <- extracted_time
  
  # Extract Form
  extracted_form <- extract_form(txt_content)
  form_data[i] <- extracted_form
  
  # Extract the time part
  gmt_time[i] <- sub("^.*Generated Time \\(GMT\\): (.*)$", "\\1", extracted_time)
  
}

# Add Generated Time (GMT) and Gmt_time to jpg_in_crfs dataframe
jpg_in_crfs$Extracted_Data <- extracted_data
jpg_in_crfs$Gmt_time <- gmt_time
jpg_in_crfs$Form <- form_data

# Parse Gmt_time into separate components
jpg_in_crfs$Gmt_time <- dmy_hm(jpg_in_crfs$Gmt_time)

# Extract year, month, and day components
jpg_in_crfs$Year <- year(jpg_in_crfs$Gmt_time)
jpg_in_crfs$Month <- month(jpg_in_crfs$Gmt_time)
jpg_in_crfs$Day <- day(jpg_in_crfs$Gmt_time)

# Add Gmt_YYYYMMDD column with the YYYY-MM-DD format
jpg_in_crfs$Gmt_YYYYMMDD <- sprintf("%04d-%02d-%02d", 
                                    jpg_in_crfs$Year, 
                                    jpg_in_crfs$Month, 
                                    jpg_in_crfs$Day)

print(jpg_in_crfs)

# Select the relevant columns to create the new dataframe
jpg_in_crfs_print <- jpg_in_crfs %>%
  select(SUBJID, page_num, Form, Gmt_YYYYMMDD)

# Print the new dataframe to check the content
print(jpg_in_crfs_print)

# Save the new dataframe to a CSV file
write.csv(jpg_in_crfs_print, 'jpg_in_crfs.csv', row.names = FALSE)


# Filter merged_data to sustain only the rows where ARM = "Placebo"
placebo_data <- merged_data %>% filter(ARM == "Placebo")

# Filter jpg_in_crfs to sustain only the SUBJID which are in the filtered merged_data generated on "1"
filtered_jpg_in_crfs <- jpg_in_crfs %>% filter(SUBJID %in% placebo_data$SUBJID)
print(filtered_jpg_in_crfs)
filtered_jpg_in_crfs_merged_data <- merge(filtered_jpg_in_crfs, phase_3_randomized_pop, by = "SUBJID")
print(filtered_jpg_in_crfs_merged_data)

# Calculate the total number of rows for each unique date in the Gmt_YYYYMMDD column
date_counts <- table(filtered_jpg_in_crfs_merged_data$Gmt_YYYYMMDD)

# Print the total number of rows for each unique date
print(date_counts)

# Filter the dataframe for Gmt_YYYYMMDD = "2021-04-19"
subset_data <- subset(filtered_jpg_in_crfs_merged_data, Gmt_YYYYMMDD == "2021-04-19")

# Print the unique SUBJID
unique_subjid <- unique(subset_data$SUBJID)
print(unique_subjid)

# Create a table of unique SUBJID depending on the presence of VAX201DT
subjid_count_vax <- table(!is.na(filtered_jpg_in_crfs_merged_data$VAX201DT))

# Print the total of unique SUBJID depending on the presence of VAX201DT
print(subjid_count_vax)

# Count the total of unique SUBJID depending on the presence of VAX201DT
total_subjid_vax_defined <- length(unique(filtered_jpg_in_crfs_merged_data$SUBJID[!is.na(filtered_jpg_in_crfs_merged_data$VAX201DT)]))
total_subjid_vax_na <- length(unique(filtered_jpg_in_crfs_merged_data$SUBJID[is.na(filtered_jpg_in_crfs_merged_data$VAX201DT)]))

# Print the results
cat("Total unique SUBJID with VAX201DT defined:", total_subjid_vax_defined, "\n")
cat("Total unique SUBJID with VAX201DT NA:", total_subjid_vax_na, "\n")

# Filter subjects with VAX201DT NA
subjects_vax_na <- unique(filtered_jpg_in_crfs_merged_data[is.na(filtered_jpg_in_crfs_merged_data$VAX201DT), "SUBJID"])

# Print the unique subjects
cat("Unique subjects with VAX201DT NA:\n")
print(subjects_vax_na)

# Create a table of counts based on the Form entry
form_counts <- as.data.frame(table(jpg_in_crfs$Form))

# Split the data frame into two halves
half_n <- ceiling(nrow(form_counts) / 2)
first_half <- form_counts[1:half_n, ]
second_half <- form_counts[(half_n + 1):nrow(form_counts), ]

# Ensure the two halves have the same number of rows by adding NA if necessary
if (nrow(first_half) > nrow(second_half)) {
  second_half <- rbind(second_half, data.frame(Var1 = NA, Freq = NA))
}

# Combine the two halves side by side
form_counts_combined <- bind_cols(first_half, second_half)

# Rename the columns appropriately
colnames(form_counts_combined) <- c("Form1", "Count1", "Form2", "Count2")

# Create a flextable
form_table <- flextable(form_counts_combined)

# Set header labels
form_table <- set_header_labels(form_table, 
                                Form1 = "Form", 
                                Count1 = "Count", 
                                Form2 = "Form", 
                                Count2 = "Count")

# Add styling
form_table <- theme_zebra(form_table) %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit()

# Set caption
form_table <- set_caption(form_table, "Table 3: Total entries in jpg_in_crfs based on Form")

# Save as HTML
save_as_html(form_table, path = "filtered_jpg_in_crfs_form_table.html")
