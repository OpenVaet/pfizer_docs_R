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

# Send a GET request
res <- GET(docs_url, user_agent(user_agent))

# Check if the request is successful
if(http_error(res)){
  stop(paste("Failed to get", docs_url))
}

# Parse the content
content <- content(res, as="text")
tree <- read_html(content)

# Create pdf_data directory if required
pdf_path <- "pdf_data"
if (!dir.exists(pdf_path)) {
  dir.create(pdf_path)
}

# Create html_data directory if required
html_path <- "html_data"
if (!dir.exists(html_path)) {
  dir.create(html_path)
}

# Clear the output file if it exists
output_file <- "investigators_screening_randomization.csv"
if (file.exists(output_file)) {
  file.remove(output_file)
}
write(paste('File', 'Trial Site ID', 'Subjects Screened', 'Subjects Randomized', sep = ";"), 
      file = output_file, append = TRUE)

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
    if(grepl("c4591001.*investigators.*pdf", file_name)){
      local_file <- ifelse(file_ext == "pdf", paste0(pdf_path, "/", online_file), 
                      stop(paste0("Unknown extension : [", file_ext, "] on file [", file_name, "] (", online_file, "), contact the script authors to obtain an update.")))
      if (!file.exists(local_file)) {
        print(paste0("Downloading [", local_file, "] from [", file_url, "]"))
        download.file(file_url, local_file, mode = "wb")
      }
      
      # Extract text from the PDF file
      text <- pdf_text(local_file)

      # Collapse the text into a single string
      text <- paste(text, collapse = " ")

      # Create an HTML file
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

      
      # Read the file
      lines <- readLines(con = html_file, encoding = "UTF-8")

      # Loop over each line
      for (line in lines) {
        # Remove trailing newline character
        line <- sub("\n$", "", line)
        
        # Check if line matches the pattern
        if (grepl("^\\s{65}(....).*?(\\d+)\\s+(\\d+)$", line)) {
          # Extract the required fields
          fields <- str_match(line, "^\\s{65}(....).*?(\\d+)\\s+(\\d+)$")
          
          # Extract the trial_site_id
          trial_site_id <- gsub(" ", "", fields[2])
    
          # Check if trial_site_id consists only of digits and its length is 4
          if (grepl("^\\d+$", trial_site_id) && nchar(trial_site_id) == 4) {
            # Extract the subjects_screened and subjects_randomized
            subjects_screened <- fields[3]
            subjects_randomized <- fields[4]
            
            # Print the output
            print(paste("Trial Site ID:", trial_site_id, 
                        "Subjects Screened:", subjects_screened, 
                        "Subjects Randomized:", subjects_randomized))
            
            # Write to the output file
            write(paste(basename(local_file), trial_site_id, subjects_screened, subjects_randomized, sep = ";"), 
                  file = output_file, append = TRUE)
          }
        }
      }
    }
  }
}
