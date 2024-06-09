# Load necessary libraries
library(jsonlite)
library(httr)
library(rvest)
library(stringi)
library(tools)
library(fs)

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

# Iterate over the PDF files
pdf_files <- dir_ls('pdf_data', glob = '*_CRF_*.pdf')

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
  
  cat(file, "\n")
  cat(subjid, "\n")
  
  output_folder <- file.path('crf_html', subjid)
  
  if (!dir_exists(output_folder)) {
    # Convert PDF to HTML
    cmd <- paste(pdf_to_html_executable, shQuote(file), shQuote(output_folder))
    cat(cmd, "\n")
    system(cmd)
  }
  
  html_pages <- dir_ls(output_folder, glob = 'page*.html')
  
  for (page in html_pages) {
    html <- read_html(page)
    text <- html_text(html)
    text <- stri_trim_both(text)
    text_length <- nchar(text)
    
    if (text_length < 50) {
      page_num <- stri_match_first_regex(page, 'page(\\d+).html')[, 2]
      if (is.null(pages_replaced[[subjid]])) {
        pages_replaced[[subjid]] <- list()
      }
      pages_replaced[[subjid]][[page_num]] <- 1
      cat(file, '->', page, '|', text, '|', text_length, "\n")
    }
  }
}

# Write the results to a CSV file
output_file <- 'jpg_in_crfs.csv'
file_conn <- file(output_file, 'w', encoding = 'UTF-8')

for (subjid in sort(names(pages_replaced))) {
  for (page in sort(names(pages_replaced[[subjid]]))) {
    writeLines(paste(subjid, page, sep = ','), file_conn)
  }
}

close(file_conn)
