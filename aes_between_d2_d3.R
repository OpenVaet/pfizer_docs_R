# Loads necessary package
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
adae_data_selected <- adae_data[c("USUBJID", "AESPID", "ASTDT", "ARM", "AGE", "AETERM", "AESER", "SEX", "VAX102DT", "VAX201DT", "VPHASE")]
print(unique(adae_data_selected$VPHASE))

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
adae_data_selected$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adae_data_selected$USUBJID)
print(adae_data_selected)

# Filters on the cross-over subjects.
filtered_adae <- adae_data_selected %>% 
  filter(!is.na(VAX201DT))

# Filters on VPHASE "After unblinding and before Vaccination 3"
filtered_adae_between_d2_d3 <- adae_data_selected %>% 
   filter(VPHASE == "After unblinding and before Vaccination 3")
print(filtered_adae_between_d2_d3)

# Filters on the AEs reported between D2 & D3, adds a column containing the difference in days between AEs & Dose 3.
filtered_adae_between_d2_d3 <- filtered_adae_between_d2_d3 %>% 
  mutate(DAYSDIF = as.numeric(VAX201DT - ASTDT))
print(filtered_adae_between_d2_d3, n=600)

# Isolates the rows where AETERM matches the string "injec"
injec_rows <- filtered_adae_between_d2_d3 %>% 
  filter(grepl("injec", tolower(AETERM)))
print(injec_rows, n=100)

# Creates the formatted table
injec_rows <- injec_rows %>% 
  select(-USUBJID, -AESPID, -AESER, -SEX, -VPHASE)
html_table <- flextable(injec_rows) %>%
  set_header_labels(
    "SUBJID" = "Subject ID",
    "AGE" = "Age",
    "VAX102DT" = "Date Dose 2",
    "VAX201DT" = "Date Dose 3",
    "ARM" = "Arm",
    "ASTDT" = "AE Date",
    "AETERM" = "AE Term",
    "DAYSDIF" = "Days Dose 3 - AE"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table, path = "injection_pain_between_d2_to_d3.html")

table(injec_rows$DAYSDIF)

