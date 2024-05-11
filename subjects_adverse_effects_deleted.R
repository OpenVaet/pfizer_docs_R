# Load necessary package
library(haven)
library(dplyr)
library(tidyr)
library(pdftools)
library(rmarkdown)
library(flextable)
library(tidyverse)

# Reads the XPT file & retains data required for analysis.
adae_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt')
print(adae_data)
print(colnames(adae_data))
adae_data_selected <- adae_data[c("USUBJID", "AESPID", "AESTDTC", "ARM")]

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
adae_data_selected$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adae_data_selected$USUBJID)
write.csv(adae_data_selected, "adae_data_selected.csv", row.names = FALSE)
print(adae_data_selected)

# Initializes counters
skipped_empty_aespid <- 0
data_list <- list()

# Processes the data
for (i in 1:nrow(adae_data_selected)) {
  aespid <- adae_data_selected[i, "AESPID"]
  subjid <- as.character(adae_data_selected[i, "SUBJID"])
  if (is.null(aespid) || aespid == '') {
    skipped_empty_aespid <- skipped_empty_aespid + 1
    next
  }
  aespid <- as.character(aespid)
  if (!(subjid %in% names(data_list))) {
    data_list[[subjid]] <- list()
  }
  data_list[[subjid]][[as.character(aespid)]] <- 1
}
print(data_list)

cat("skipped_empty_aespid : ", skipped_empty_aespid, "\n")

# Stores the missing AESPID combinations
missing_aespid <- 0
missing_aespid_df <- data.frame()

for (subjid in names(data_list)) {
  aespid_values <- names(data_list[[subjid]])
  max_aespid <- max(as.integer(aespid_values))
  if (is.null(max_aespid)) {
    stop("max_aespid is null")
  }
  for (aespid in 1:max_aespid) {
    if (!(as.character(aespid) %in% aespid_values)) {
      missing_aespid <- missing_aespid + 1
      cat("missing on [", subjid, "] : ", aespid, "\n")
      missing_aespid_df <- rbind(missing_aespid_df, data.frame(SUBJID = subjid, AESPID = aespid))
    }
  }
}

print(missing_aespid_df)
cat("missing_aespid : ", missing_aespid, "\n")

# Selects only the SUBJID and ARM columns from adae_data_selected
adae_data_selected_ARM <- adae_data_selected %>% select(SUBJID, ARM)

# Merges missing_aespid_df with adae_data_selected_ARM
missing_aespid_df <- merge(missing_aespid_df, adae_data_selected_ARM, by = "SUBJID")

# Creates a data frame with unique SUBJID and their corresponding ARM
unique_subj_arm <- data.frame(
  SUBJID = unique(missing_aespid_df$SUBJID),
  ARM = missing_aespid_df[match(unique(missing_aespid_df$SUBJID), missing_aespid_df$SUBJID), "ARM"]
)

print(unique_subj_arm)

arm_counts <- table(unique_subj_arm$ARM)
print(arm_counts)

unique_subjid_count <- length(unique(missing_aespid_df$SUBJID))
cat("Number of unique SUBJID:", unique_subjid_count, "\n")


# Creates html_data directory if required
html_path <- "html_data"
if (!dir.exists(html_path)) {
  dir.create(html_path)
}
crfs <- 0
files <- dir("pdf_data/", pattern = "_CRF_")

# Initializes an empty dataframe to store the results
result_df <- data.frame(SUBJID = character(), AESPID = character(), ADVERSE_EVENT = character(), DATE = character())

# Opens the file in write mode
fileConn <- file("debug.txt", "w")

for (file in files) {
  elems <- strsplit(file, "-")[[1]]
  subjid <- elems[length(elems)]
  if (subjid == "reissue.pdf") {
    subjid <- elems[length(elems) - 1]
  }
  subjid <- gsub("\\.pdf", "", subjid)
  
  if (subjid %in% missing_aespid_df$SUBJID) {
    aespid_values <- missing_aespid_df[missing_aespid_df$SUBJID == subjid, "AESPID"]
    crfs <- crfs + 1
    
    # Extracts text from the PDF file
    text <- pdf_text(file.path("pdf_data", file))
    
    # Collapses the text into a single string
    text <- paste(text, collapse = " ")
    
    # Creates an HTML file
    html_file <- paste0(html_path, "/", tools::file_path_sans_ext(file), ".html")
    html <- htmltools::tagList(
      htmltools::tags$head(
        htmltools::tags$title(tools::file_path_sans_ext(file))
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
      
      if (grepl("DELETED ADVERSE", line)) {
        
        # Extract the date
        if (grepl("[A-Z][a-z]{2}/[0-9]{1,2}/[0-9]{4}", line)) {
          date <- sub(".* ([A-Z][a-z]{2}/[0-9]{1,2}/[0-9]{4}).*", "\\1", line)
        } else if (grepl("[A-Z][a-z]{2}/[0-9]{1,2}/[0-9]{3}", line)) {
          date <- sub(".* ([A-Z][a-z]{2}/[0-9]{1,2}/[0-9]{3}).*", "\\1", line)
        } else if (grepl("[A-Z][a-z]{2}/[0-9]{1,2}/20", line)) {
          date <- sub(".* ([A-Z][a-z]{2}/[0-9]{1,2}/20).*", "\\1", line)
        } else if (grepl("[A-Z][a-z]{2}/UNK/2020", line)) {
          date <- sub(".* ([A-Z][a-z]{2}/UNK/2020).*", "\\1", line)
        } else {
          date <- NA
        }
        
        # Extract the terms on the right of this number between "DELETED ADVERSE" and the date
        adverse_term <- sub(paste0(".*DELETED ADVERSE(.*)", date), "\\1", line)
        first_number <- sub(".*?([0-9]+).*", "\\1", adverse_term)
        terms <- sub("^[^0-9]+([0-9]+)(.*?)(YES|NO|UN).*", "\\2", adverse_term)
        terms <- gsub("[0-9]", "", terms)
        terms <- gsub(" {2,}", " ", terms)
        terms <- trimws(terms, which = "left")
        
        # Add the data to the result dataframe
        result_df <- rbind(result_df, data.frame(SUBJID = subjid, AESPID = first_number, ADVERSE_EVENT = terms, DATE = date))
        
        # Write the line to the file
        # writeLines(line, fileConn)
        writeLines(subjid, fileConn)
        # writeLines(adverse_term, fileConn)
        writeLines(date, fileConn)
        writeLines(first_number, fileConn)
        writeLines(terms, fileConn)
      }
    }
  }
}

# Closes the file connection
close(fileConn)

# Adds the result dataframe to the missing_aespid_df
missing_aespid_df <- merge(missing_aespid_df, result_df, by = c("SUBJID", "AESPID"), all.x = TRUE)
cat("Number of CRFs:", crfs, "\n")

# Removes duplicates based on SUBJID and AESPID
missing_aespid_df_unique <- missing_aespid_df %>% 
  distinct(SUBJID, AESPID, .keep_all = TRUE)

# Render a table containing the total of unique SUBJID in each ARM and the total of rows in each ARM
arm_summary <- missing_aespid_df_unique %>% 
  group_by(ARM) %>% 
  summarise(
    unique_SUBJID = n_distinct(SUBJID),
    total_rows = n(),
    pct_of_subjects = unique_SUBJID * 100 / unique_subjid_count,
    pct_of_aes = total_rows * 100 / missing_aespid
  )
arm_summary <- arm_summary %>%
  mutate(pct_of_subjects = round(pct_of_subjects, 1),
         pct_of_aes = round(pct_of_aes, 1))

print(arm_summary)



print(arm_summary)

# Creates the formatted table
html_table <- flextable(arm_summary) %>%
  set_header_labels(
    "ARM" = "Arm",
    "unique_SUBJID" = "Unique Subjects",
    "total_rows" = "Missing AESPID",
    "pct_of_subjects" = "% / Subjects Missing",
    "pct_of_aes" = "% / AESPID Missing"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table, path = "aespid_missing_by_arm.html")



print(paste('CRFs are available on ', crfs * 100 / unique_subjid_count, '% of Subjects with missing AESPID'))

print(missing_aespid_df_unique)
write.csv(missing_aespid_df_unique, "missing_aespid.csv", row.names = FALSE)



# Remove rows where ADVERSE_EVENT is NA
missing_aespid_with_terms <- missing_aespid_df_unique %>% 
  filter(!is.na(ADVERSE_EVENT))

# List the ADVERSE_EVENT and their corresponding total of rows by ARMS
adverse_event_summary <- missing_aespid_with_terms %>% 
  group_by(ARM, ADVERSE_EVENT) %>% 
  summarise(total_rows = n()) %>% 
  arrange(ARM, total_rows)

print(adverse_event_summary, n=100)
