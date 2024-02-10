library(httr)
library(rvest)
library(xml2)

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

# Gets the ADSL file
res <- GET(docs_url, user_agent(user_agent))
if(http_error(res)){
  stop(paste("Failed to get", docs_url))
}
content <- content(res, as="text")
tree <- read_html(content)

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

# Verifies if the ADSL file has been properly retrieved.
file_path <- 'xpt_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt'
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
missing_subjects <- data.frame(trial_site_id = character(), missing_id = integer(), stringsAsFactors = FALSE)

# Loops over each site
unique_sites <- unique(selected_data$trial_site_id)
for (site in unique_sites) {
  # Gets all the subject_trial_site_incremental_numbers for this trial_site_id
  site_data <- selected_data[selected_data$trial_site_id == site, ]
  existing_ids <- sort(as.numeric(site_data$subject_trial_site_incremental_number))
  
  # Gets the latest subject_trial_site_incremental_number
  latest_id <- max(existing_ids)
  
  # Creates a sequence from 1001 to the latest ID
  full_seq <- 1001:latest_id
  
  # Finds missing IDs
  missing_ids <- setdiff(full_seq, existing_ids)
  
  # Prints missing incremental numbers and add them to the missing_subjects dataframe
  for (missing_id in missing_ids) {
    cat(paste("Missing incremental number detected:", missing_id, "in trial site", site, "\n"))
    # Add the missing ID to the dataframe
    missing_subjects <- rbind(missing_subjects, data.frame(trial_site_id = site, missing_id = missing_id, stringsAsFactors = FALSE))
  }
}

# Prints the total number of missing subjects
total_missing_subjects <- nrow(missing_subjects)
cat(paste("Total subjects missing identified:", total_missing_subjects, "\n"))

