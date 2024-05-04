library(httr)
library(rvest)
library(xml2)
library(pdftools)
library(htmltools)
library(stringr)

# UA used to scrap target.
user_agent <- "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36"

# Root url where we can find the Pfizer Docs.
docs_url <- 'https://phmpt.org/pfizer-16-plus-documents/'
print(paste("Getting index on", docs_url))

# Sends a GET request
res <- GET(docs_url, user_agent(user_agent))

# Checks if the request is successful
if(http_error(res)){
  stop(paste("Failed to get", docs_url))
}

# Parses the content
content <- content(res, as="text")
tree <- read_html(content)

# Creates pdf_data directory if required
pdf_path <- "pdf_data"
if (!dir.exists(pdf_path)) {
  dir.create(pdf_path)
}

# Creates html_data directory if required
html_path <- "html_data"
if (!dir.exists(html_path)) {
  dir.create(html_path)
}

# Clears the output file if it exists
output_file <- "eua_official_cases.csv"
if (file.exists(output_file)) {
  file.remove(output_file)
}
write(paste('SUBJID', sep = ","), 
      file = output_file, append = TRUE)

# Flag to control printing
to_print <- 0

# Downloads each file.
trs <- html_nodes(tree, 'tbody tr')
for(i in 1:length(trs)){
  tr <- trs[i]
  tds <- html_nodes(tr, 'td')
  if(length(tds) > 0){
    file_name <- html_text(tds[1])
    file_date <- html_text(tds[2])
    file_size <- html_text(tds[3])
    file_url <- html_attr(html_node(tds[4], 'a'), 'href')
    online_file <- basename(file_url)
    file_ext <- tools::file_ext(online_file)
    if(grepl("^125742_S1_M5_CRF_c4591001 \\d+ \\d+\\.pdf$", file_name) || 
       grepl("_M1_356h\\.pdf$", file_name)){
      next
    }
    if(file_name == '125742_S1_M5_5351_c4591001-fa-interim-lab-measurements-sensitive.pdf'){
      local_file <- ifelse(file_ext == "pdf", paste0(pdf_path, "/", online_file), 
                           stop(paste0("Unknown extension : [", file_ext, "] on file [", file_name, "] (", online_file, "), contact the script authors to obtain an update.")))
      if (!file.exists(local_file)) {
        print(paste0("Downloading [", local_file, "] from [", file_url, "]"))
        download.file(file_url, local_file, mode = "wb")
      }
      
      # Extracts text from the PDF file
      text <- pdf_text(local_file)
      
      # Collapses the text into a single string
      text <- paste(text, collapse = " ")
      
      # Creates an HTML file
      html_file <- paste0(html_path, "/", tools::file_path_sans_ext(online_file), ".html")
      html <- htmltools::tagList(
        htmltools::tags$head(
          htmltools::tags$title(tools::file_path_sans_ext(online_file))
        ),
        htmltools::tags$body(
          htmltools::tags$p(text)
        )
      )
      htmltools::save_html(html, html_file)
      
      
      # Reads the file
      lines <- readLines(con = html_file, encoding = "UTF-8")
      
      # Loops over each line
      for (line in lines) {
        # Removes trailing newline character
        line <- sub("\n$", "", line)
        
        # Start printing when line exactly matches "16.2.8.2"
        if (grepl("16\\.2\\.8\\.2", line)) {
          to_print <- 1
        }
        
        # Stop printing when line exactly matches "16.2.8.3"
        if (grepl("16\\.2\\.8\\.3", line)) {
          to_print <- 0
        }
        
        # Write the line to the debug file if the to_print flag is set
        if (to_print == 1) {
          # Check if the line matches the desired format
          if (grepl("^ {34,}(\\d{8}) .*$", line)) {
            # Extract the 8-digit number
            number <- sub("^ {34,}(\\d{8}) .*$", "\\1", line)
            write(paste(number, sep = ","), 
                  file = output_file, append = TRUE)
          }
        }
      }
    }
  }
}

