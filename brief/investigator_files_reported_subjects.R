# =====================================================================
# C4591001 Protocol Deviations: Subject Totals Reported in Investigator Files
# =====================================================================
# This script reviews the total of subjects declared on each site in
# two versions (EUA & BLA) of the Investigators files.
# =====================================================================

library(httr)
library(rvest)
library(xml2)
library(pdftools)
library(htmltools)
library(stringr)
library(flextable)
library(dplyr)
library(readr)
library(haven)

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
output_file <- "investigators_screening_randomization.csv"
if (file.exists(output_file)) {
  file.remove(output_file)
}
write(paste('File', 'Trial Site ID', 'Subjects Screened', 'Subjects Randomized', sep = ","), 
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
        
        # Checks if line matches the pattern
        if (grepl("^\\s{65}(....).*?(\\d+)\\s+(\\d+)$", line)) {
          # Extract the required fields
          fields <- str_match(line, "^\\s{65}(....).*?(\\d+)\\s+(\\d+)$")
          
          # Extracts the trial_site_id
          trial_site_id <- gsub(" ", "", fields[2])
    
          # Checks if trial_site_id consists only of digits and its length is 4
          if (grepl("^\\d+$", trial_site_id) && nchar(trial_site_id) == 4) {
            # Extract the subjects_screened and subjects_randomized
            subjects_screened <- fields[3]
            subjects_randomized <- fields[4]
            
            # Prints the output
            print(paste("Trial Site ID:", trial_site_id, 
                        "Subjects Screened:", subjects_screened, 
                        "Subjects Randomized:", subjects_randomized))
            
            # Writes to the output file
            write(paste(basename(local_file), trial_site_id, subjects_screened, subjects_randomized, sep = ","), 
                  file = output_file, append = TRUE)
          }
        }
      }
    }
  }
}

# Reads the output file.
invest_data <- read_csv(output_file, show_col_types = FALSE)
print(invest_data)

# Reformats the randomized data between files
stats <- list()
for (i in 1:nrow(invest_data)) {
  if (invest_data$File[i] == '125742_S1_M5_5351_c4591001-fa-interim-investigators.pdf') {
    stats[[as.character(invest_data$`Trial Site ID`[i])]]$FA <- as.numeric(invest_data$`Subjects Randomized`[i])
  } else if (invest_data$File[i] == '125742_S1_M5_5351_c4591001-interim-mth6-investigators.pdf') {
    stats[[as.character(invest_data$`Trial Site ID`[i])]]$M6 <- as.numeric(invest_data$`Subjects Randomized`[i])
  } else if (invest_data$File[i] == 'File') {
    next
  } else {
    stop(paste(invest_data$File[i]))
  }
}

# Calculates the offset and create the output data frame
output_data <- data.frame(
  `Trial Site ID` = names(stats),
  FA = sapply(stats, function(x) x$FA),
  M6 = sapply(stats, function(x) x$M6),
  `Offset M6-FA` = sapply(stats, function(x) x$M6 - x$FA)
)

# Writes the output data to a CSV file
write.csv(output_data, "offset_randomization_between_fa_m6.csv", row.names = FALSE)

print(output_data)

# Calculates the total of negative offsets
negative_offsets <- output_data[output_data$`Offset.M6.FA` < 0, ]$`Offset.M6.FA`
total_negative_offsets <- sum(negative_offsets)

# Prints the total of negative offsets
print(paste("Total of negative offsets:", total_negative_offsets))

# Filter the data to only include entries with Offset.M6.FA < 0
output_data_filtered <- output_data[output_data$Offset.M6.FA < 0, ]

# Split the data into two data frames
num_rows <- nrow(output_data_filtered)
split_index <- ceiling(num_rows / 2)

output_data_left <- output_data_filtered[1:split_index, 1:4]
output_data_right <- output_data_filtered[(split_index + 1):num_rows, 1:4]

# Merge the left and right data frames
output_data_merged <- data.frame(
  "Trial.Site.ID.1" = output_data_left$`Trial.Site.ID`,
  "FA.1" = output_data_left$FA,
  "M6.1" = output_data_left$M6,
  "Offset M6-FA.1" = output_data_left$`Offset.M6.FA`,
  "Trial.Site.ID.2" = output_data_right$`Trial.Site.ID`,
  "FA.2" = output_data_right$FA,
  "M6.2" = output_data_right$M6,
  "Offset M6-FA.2" = output_data_right$`Offset.M6.FA`
)


print(output_data_merged)

# Creates the formatted table
html_table <- flextable(output_data_merged) %>%
  set_header_labels(
    "Trial.Site.ID.1" = "Trial Site ID",
    "FA.1" = "FA",
    "M6.1" = "M6",
    "Offset.M6.FA.1" = "Offset M6-FA",
    "Trial.Site.ID.2" = "Trial Site ID",
    "FA.2" = "FA",
    "M6.2" = "M6",
    "Offset.M6.FA.2" = "Offset M6-FA"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table, path = "sites_with_negative_screening.html")

# Loads the ADSL file.
file_path <- 'xpt_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt'
data <- read_xpt(file_path)
print(colnames(data))
selected_data <- data[c("SUBJID", "SITEID", "RANDDT", "AGE")]

# Creates a new column storing ORIGINSITEID and summarizes how many subjects changed sites.
selected_data$ORIGINSITEID <- as.numeric(sub("(....)....", "\\1", selected_data$SUBJID))
print(selected_data)

# 1. Adding a dataframe subjects_changing_sites with the rows where selected_data.SITEID != ORIGINSITEID
subjects_changing_sites <- selected_data[selected_data$SITEID != selected_data$ORIGINSITEID, ]
print(subjects_changing_sites)

# 2. Adding a dataframe with the sum of subjects going IN and OUT of each SITEID (the SITEID is the destination site while ORIGINSITEID is the original one)
site_transfers <- aggregate(SUBJID ~ SITEID + ORIGINSITEID, data = subjects_changing_sites, FUN = length)
colnames(site_transfers)[3] <- "NUM_SUBJECTS"
site_summary <- merge(
  aggregate(NUM_SUBJECTS ~ SITEID, data = site_transfers, FUN = sum, na.rm = TRUE, subset = (SITEID != ORIGINSITEID)),
  aggregate(NUM_SUBJECTS ~ ORIGINSITEID, data = site_transfers, FUN = sum, na.rm = TRUE, subset = (SITEID != ORIGINSITEID)),
  by.x = "SITEID",
  by.y = "ORIGINSITEID",
  all = TRUE
)
colnames(site_summary) <- c("SITEID", "NUM_IN", "NUM_OUT")
print(site_summary)

# 3. Filter on the anomalies on SITEID = 1018 so we know how many subjects came in and out
site_1018_anomalies <- site_summary[site_summary$SITEID == 1018, ]
print(site_1018_anomalies)

# 4. Add a dataframe containing all subjects with AGE < 16 and print it
subjects_under_16 <- selected_data[selected_data$AGE < 16, ]
print(subjects_under_16)
site_1018_subjects_under_16 <- subjects_under_16[subjects_under_16$ORIGINSITEID == 1018, ]
print(site_1018_subjects_under_16)

