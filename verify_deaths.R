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
print(colnames(data))
selected_data <- data[c("USUBJID", "ARM", "DTHDT", "VAX201DT")]
deaths_only <- selected_data[!is.na(selected_data$DTHDT), ]

# Adjusts the ARM column based on VAX201DT (only Placebo subjects were receiving BNT)
deaths_only$ARM[!is.na(deaths_only$VAX201DT)] <- "Placebo -> BNT162b2"

# Count the total rows by ARM
table(deaths_only$ARM)
