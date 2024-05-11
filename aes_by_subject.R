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
adae_data_selected <- adae_data[c("USUBJID", "AESPID", "AESTDTC", "ARM", "AGE", "AETERM", "AESER", "SEX", "VAX101DT", "VAX102DT")]

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
adae_data_selected$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adae_data_selected$USUBJID)
print(adae_data_selected)

subjid <- 12181001

filtered_aes <- adae_data_selected %>% 
  filter(SUBJID == subjid)
print(filtered_aes)

subjid <- 10871286

filtered_aes <- adae_data_selected %>% 
  filter(SUBJID == subjid)
print(filtered_aes)