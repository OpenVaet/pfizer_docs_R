library(httr)
library(rvest)
library(xml2)
library(haven)
library(ggplot2)
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

# Reads the XPT file.
data <- read_xpt(file_path)
print(colnames(data))
selected_data <- data[c("SUBJID", "RFICDT", "PHASEN")]
print(selected_data)

selected_data <- selected_data %>%
  mutate(PHASEN = ifelse(PHASEN > 3, 3, PHASEN))

# Converts "PHASEN=4" to "3"
Phasen_counts <- selected_data %>%
  group_by(PHASEN) %>%
  tally(name = "Total_Subjects")
print(Phasen_counts)

# Adds WEEKNUM and YEAR columns
selected_data$RFICDT <- as.Date(selected_data$RFICDT)
selected_data$YEAR <- format(selected_data$RFICDT, "%Y")
selected_data$WEEKNUM <- format(selected_data$RFICDT, "%U")

# Groups by YEAR, WEEKNUM, and PHASEN, and count the number of subjects recruited weekly
weekly_recruitment <- selected_data %>%
  mutate(YEAR_WEEKNUM = paste(YEAR, WEEKNUM, sep = "-")) %>%
  group_by(YEAR_WEEKNUM, PHASEN) %>%
  tally(name = "Subjects") %>%
  arrange(YEAR_WEEKNUM, PHASEN)

# Outputs a column chart
ggplot(weekly_recruitment, aes(x = YEAR_WEEKNUM, y = Subjects, fill = factor(PHASEN))) + 
  geom_col(position = "dodge") + 
  scale_fill_discrete(breaks = c("1", "2", "3"), labels = c("1", "2", "3")) + 
  labs(x = "Year-Week Number", y = "Number of Subjects", fill = "Phase", 
       title = "C4591001 - Weekly recruitment of phases 1, 2 & 3 Subjects") + 
  theme_classic() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(angle = 90, size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

