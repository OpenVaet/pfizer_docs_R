# Extracts ZIP to XPT.
library(tools)
library(haven)
library(dplyr)

# Verifies if the requireed files have been properly retrieved.
# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "VPHASE", "AREL", "AEHLGT", "AETERM", "AESER", "AERELTXT", "ATOXGR", "AESTDTC", "AEENDTC", "AEDECOD")]
adae_12312982_data <- adae_selected_data %>% 
  filter(SUBJID == 12312982)
print(adae_12312982_data)
