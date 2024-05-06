library(httr)
library(rvest)
library(xml2)
library(flextable)
library(dplyr)

# Creates zip_data directory
zip_path <- "zip_data"
if (!dir.exists(zip_path)) {
  dir.create(zip_path)
}

# UA used to scrap target.
user_agent <- "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36"

# Root url where we can find the Pfizer Docs.
docs_url <- 'https://phmpt.org/pfizer-16-plus-documents/'
print(paste("Getting index on", docs_url))

file_path <- 'xpt_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt'

# If file_path is missing, downloads files
if (!file.exists(file_path)) {
  # Gets the ADSL file
  res <- GET(docs_url, user_agent(user_agent))
  if(http_error(res)){
    stop(paste("Failed to get", docs_url))
  }
  content <- content(res, as="text")
  tree <- read_html(content)

  # Downloads file.
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
      if(grepl("c4591001.*adsl.*xpt", file_name)){
        local_file <- ifelse(file_ext == "zip", paste0(zip_path, "/", online_file), 
                             ifelse(file_ext == "xpt", paste0(xpt_path, "/", online_file), 
                                    stop(paste0("Unknown extension : [", file_ext, "] on file [", file_name, "] (", online_file, "), contact the script authors to obtain an update."))))
        if (!file.exists(local_file)) {
          print(paste0("Downloading [", local_file, "] from [", file_url, "]"))
          download.file(file_url, local_file, mode = "wb")
        }
      }
    }
  }

  # Extracts ZIP to XPT.
  library(tools)
  library(zip)
  xpt_path <- "xpt_data"
  if (!dir.exists(xpt_path)) {
    dir.create(xpt_path)
  }
  zip_path <- 'zip_data'
  zip_files <- list.files(path = zip_path, pattern = "*.zip", full.names = TRUE)
  for (zip_file in zip_files) {
    print(paste("zip : [", zip_file, "]"))
    
    if (!file.exists(zip_file)) {
      print(paste('Not an archive : ', zip_file))
      quit()
    }
    
    files_in_zip <- utils::unzip(zip_file, list = TRUE)$Name
    file_name <- files_in_zip[1]
    xpt_file <- file.path(xpt_path, file_name)
    
    if (!file.exists(xpt_file)) {
      print(paste("Extracting [", xpt_file, "]"))
      utils::unzip(zip_file, exdir = xpt_path)
    }
  }
}

# Verifies if the ADSL file has been properly retrieved.
if (!file.exists(file_path)) {
  stop("ADSL file not found", call. = FALSE)
}

# Reads the XPT file & loads USUBJID (Unique Subject Id including Study ID & Current Trial Site Id)
library(haven)
data <- read_xpt(file_path)
selected_data <- data[c("USUBJID", "RFICDT")]

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
selected_data$subject_id <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", selected_data$USUBJID)

# Extracts the trial_site_id (the site which actually recruited the subject in the study - not the current trial site)
selected_data$trial_site_id <- as.numeric(sub("(....)....", "\\1", selected_data$subject_id))

# Extracts the subject incremental identifier (attributed to the subject when he is screened by the site).
selected_data$subject_trial_site_incremental_number <- as.numeric(sub("....(....)", "\\1", selected_data$subject_id))

print(selected_data)

# Organizes the subject_trial_site_incremental_number by trial_site_id
# Create an empty data frame with the correct structure
unique_sites <- unique(selected_data$trial_site_id)
missing_subjects <- data.frame(USUBJID = character(), RFICDT = character(), subject_id = character(), trial_site_id = integer(), subject_trial_site_incremental_number = integer(), missing = character(), stringsAsFactors = FALSE)

for (site in unique_sites) {
  site_data <- selected_data[selected_data$trial_site_id == site, ]
  existing_ids <- sort(as.numeric(site_data$subject_trial_site_incremental_number))
  latest_id <- max(existing_ids)
  full_seq <- 1001:latest_id
  missing_ids <- setdiff(full_seq, existing_ids)
  
  for (id in missing_ids) {
    cat(paste("Missing incremental number detected:", id, "in trial site", site, "\n"))
    new_row <- data.frame(USUBJID = NA, RFICDT = NA, subject_id = paste0(site, sprintf("%04d", id)), trial_site_id = site, subject_trial_site_incremental_number = id, missing = "Yes", stringsAsFactors = FALSE)
    missing_subjects <- rbind(missing_subjects, new_row)
  }
}
print(missing_subjects)

# Prints the total number of missing subjects
total_missing_subjects <- nrow(missing_subjects)
cat(paste("Total subjects missing identified:", total_missing_subjects, "\n"))
all_subjects <- selected_data
all_subjects$missing <- "No"
all_subjects <- rbind(all_subjects, missing_subjects)
all_subjects <- all_subjects[order(all_subjects$subject_id), ]
write.csv(all_subjects[c("subject_id", "RFICDT", "missing")], "all_subjects_missing_or_not.csv", row.names = FALSE)
print(all_subjects)
print(unique(all_subjects$missing))

# Filters for site 1231 and date 2020-08-21 to provide an highlight of Augusto Roux's recruitment day
filtered_subjects <- all_subjects %>% 
  filter(trial_site_id == 1231)
print(filtered_subjects)

# Initiates an empty storage of the latest date seen.
latest_RFICDT <- ''
latest_site_id <- ''
missing_indices <- c()

for (i in 1:nrow(filtered_subjects)) {
  missing <- filtered_subjects$missing[i]
  subject_id  <- filtered_subjects$subject_id [i]
  trial_site_id <- filtered_subjects$trial_site_id[i]
  if (latest_site_id != '') {
    if (latest_site_id != trial_site_id) {
      latest_RFICDT <- ''
    }
  }
  if (latest_RFICDT == '2020-08-21') {
    latest_site_id <- trial_site_id
    if (missing == "Yes") {
      print(paste('i : ', i ))
      print(paste('Subject_id : ', subject_id ))
      print(paste('missing : ', missing ))
      print(paste('latest_RFICDT : ', latest_RFICDT ))
      missing_indices <- c(missing_indices, i)
    }
  }
  if (missing != "Yes") {
    latest_RFICDT <- filtered_subjects$RFICDT[i]
  }
}

# Calculate the center of "Yes" rows
center_row <- median(missing_indices)
print(center_row)

# Select the 50 rows around the center row
subjects_missing_highlight <- filtered_subjects %>% 
  arrange(subject_trial_site_incremental_number) %>% 
  slice((center_row - 23):(center_row + 23))
print(subjects_missing_highlight)

library(rmarkdown)

# Load the template
template <- readLines("missing_subjects_template.html")

# Initialize the table rows
table_rows <- ""

# Iterate over each subject and add their HTML data
for (i in 1:nrow(subjects_missing_highlight)) {
  row <- subjects_missing_highlight[i, ]
  missing <- subjects_missing_highlight$missing[i]
  if (missing == 'Yes') {
    table_rows <- paste0(table_rows, "
      <tr>
        <td>", row$subject_id, "</td>
        <td>", format(row$RFICDT, "%Y-%m-%d"), "</td>
        <td style=\"background:#fa8072\">", row$missing, "</td>
      </tr>
  ")
  } else {
    table_rows <- paste0(table_rows, "
      <tr>
        <td>", row$subject_id, "</td>
        <td>", format(row$RFICDT, "%Y-%m-%d"), "</td>
        <td>", row$missing, "</td>
      </tr>
  ")
  }
}

# Replace the <--TABLE_ROWS--> placeholder with the actual table rows
template <- gsub("<--TABLE_ROWS-->", table_rows, template)

# Print the resulting HTML to a file
writeLines(template, "subjects_missing.html")

# Create a summary data frame
summary_data <- data.frame(
  trial_site_id = unique(missing_subjects$trial_site_id),
  total_missing = sapply(unique(missing_subjects$trial_site_id), function(x) sum(missing_subjects$trial_site_id == x)),
  total_subjects = sapply(unique(missing_subjects$trial_site_id), function(x) sum(selected_data$trial_site_id == x)),
  percentage_missing_per_site = sapply(unique(missing_subjects$trial_site_id), function(x) sum(missing_subjects$trial_site_id == x) / sum(selected_data$trial_site_id == x) * 100),
  percentage_missing_total = sapply(unique(missing_subjects$trial_site_id), function(x) sum(missing_subjects$trial_site_id == x) / total_missing_subjects * 100),
  stringsAsFactors = FALSE
)
print(summary_data)

# Write the summary data to a CSV file
write.csv(summary_data, "missing_subjects_by_sites.csv", row.names = FALSE)

# Split the data into two data frames
num_rows <- nrow(summary_data)
split_index <- ceiling(num_rows / 2)
output_data_left <- summary_data[1:split_index, 1:5]
output_data_right <- summary_data[(split_index + 1):(split_index + nrow(output_data_left)), 1:5]
print(output_data_left)
output_data_merged <- cbind(output_data_left, output_data_right)
colnames(output_data_merged) <- c(
  "trial_site_id.1", "total_missing.1", "total_subjects.1", "percentage_missing_per_site.1", "percentage_missing_total.1",
  "trial_site_id.2", "total_missing.2", "total_subjects.2", "percentage_missing_per_site.2", "percentage_missing_total.2"
)
output_data_merged$trial_site_id.1 <- as.character(output_data_merged$trial_site_id.1)
output_data_merged$trial_site_id.2 <- as.character(output_data_merged$trial_site_id.2)
print(output_data_merged)
# Creates the formatted table
html_table <- flextable(output_data_merged) %>%
  set_header_labels(
    "trial_site_id.1" = "Trial Site ID",
    "total_missing.1" = "Missing",
    "total_subjects.1" = "Total",
    "percentage_missing_per_site.1" = "% / Site",
    "percentage_missing_total.1" = "% / Total",
    "trial_site_id.2" = "Trial Site ID",
    "total_missing.2" = "Missing",
    "total_subjects.2" = "Total",
    "percentage_missing_per_site.2" = "% / Site",
    "percentage_missing_total.2" = "% / Total"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table, path = "sites_with_subjects_missing.html")
