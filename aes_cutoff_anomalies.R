# Extracts ZIP to XPT.
library(tools)
library(haven)
library(dplyr)
library(pdftools)
library(rmarkdown)
library(flextable)
library(tidyverse)

# Verifies if the requireed files have been properly retrieved.
# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "AESPID", "VPHASE", "AREL", "AESER", "AESTDTC", "AEENDTC", "AEDECOD")]

# 12231058
adae_12231058_data <- adae_selected_data %>% 
  filter(SUBJID == 12231058)
print(adae_12231058_data)

# Creates the formatted table
html_table_12231058 <- flextable(adae_12231058_data) %>%
  set_header_labels(
    "SUBJID" = "Subject ID",
    "AESPID" = "AESPID",
    "VPHASE" = "VPHASE",
    "AREL" = "Relation",
    "AESER" = "Serious",
    "AESTDTC" = "Start Date",
    "AEENDTC" = "End Date",
    "AEDECOD" = "AEDECOD"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: AES Reported by Subject 12231058")

save_as_html(html_table_12231058, path = "aes_12231058.html")

# 12313610
adae_12313610_data <- adae_selected_data %>% 
  filter(SUBJID == 12313610)
print(adae_12313610_data)

# Creates the formatted table
html_table_12313610 <- flextable(adae_12313610_data) %>%
  set_header_labels(
    "SUBJID" = "Subject ID",
    "AESPID" = "AESPID",
    "VPHASE" = "VPHASE",
    "AREL" = "Relation",
    "AESER" = "Serious",
    "AESTDTC" = "Start Date",
    "AEENDTC" = "End Date",
    "AEDECOD" = "AEDECOD"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: AES Reported by Subject 12313610")

save_as_html(html_table_12313610, path = "aes_12313610.html")
