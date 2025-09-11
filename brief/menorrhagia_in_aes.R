# =====================================================================
# C4591001 Protocol Deviations: Adverse Events - Menstrual Bleedings
# =====================================================================
# This script scans the Adverse Events file for Menorrhagia among
# women recipients of BNT162b2, 16 to 39 years old, among the randomized
# population.
# =====================================================================
# This script requires that:
# 1 - download_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/download_full_prod.R
# 2 - extract_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/extract_full_prod.R
# ... have both been executed first
# ---------------------------------------------------------------------
# 1. SETUP AND CONFIGURATION
# ---------------------------------------------------------------------
# Load required libraries
library(haven)      # For reading XPT files
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling
library(furrr)      # For parallel processing
library(stringr)    # For string manipulation
library(ggplot2)    # For creating plots
library(base64enc)  # For embedding plots in HTML
library(tidyverse)  # For replace_na function on days without recruitment

# Define file paths
DATA_PATH <- "xpt_data/"

# Primary data files
FILES <- list(
  adsl = paste0(DATA_PATH, "FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt")
)

# Analysis parameters
EXCLUDED_SUBJECTS <- c(10561101, 11331382, 11101123, 11331405, 11491117,
                       12691090, 12691070, 11351357, 11341006, 10891112,
                       11231105, 10711213)
MIN_AGE <- 16

# Helpers
safe_parse_date <- function(x) {
  # Robustly parse character/numeric/labelled to Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "labelled")) x <- haven::as_factor(x)
  x <- as.character(x)
  if (length(x) == 0) return(as.Date(character()))
  suppressWarnings({
    dt <- parse_date_time(x, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", "Ymd HMS", "Ymd HM", "Ymd H", "Ymd"))
  })
  as.Date(dt)
}

# ---------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL PROCESSING
# ---------------------------------------------------------------------
cat("Loading and processing ADSL data...\n")
adsl_data <- read_xpt(FILES$adsl)
cat(sprintf("Initial ADSL records: %d\n", nrow(adsl_data)))

randomized_pop <- adsl_data %>%
  filter(!(SUBJID %in% EXCLUDED_SUBJECTS)) %>%
  filter(PHASE != "Phase 1") %>%
  filter(as.numeric(AGETR01) >= MIN_AGE) %>%
  filter(RANDNO != "") %>%
  select(SUBJID, SITEID, COUNTRY, RFICDT, ARM, PHASE,
         AGE, AGETR01, RANDDT, RANDNO, AGEGR1,
         AGEGR2, AGEGR3, UNBLNDDT, SEX, DTHDT,
         VAX101DT, VAX102DT, VAX201DT, VAX202DT, RACE, ETHNIC)
women_randomized_pop <- randomized_pop %>%
  filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")
women_randomized_pop <- women_randomized_pop %>%
  filter(AGE >= 16 & AGE <= 39 & SEX == 'F')
print(women_randomized_pop)

cat(sprintf("Randomized population - Women 16 to 39 receiving BNT162b2 Phase 2/3 (30 mcg) after filtering: %d\n", nrow(women_randomized_pop)))

# ---------------------------------------------------------------------
# 3. MEDICAL HISTORY FILE LOADING
# ---------------------------------------------------------------------
# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "ARM", "AGE", "VPHASE", "AREL", "AERELTXT", "AESTDTC", "AEENDTC", "AEDECOD")]
print(adae_selected_data)
adae_data_filtered <- adae_selected_data %>%
  filter(AEDECOD %in% c("Menorrhagia"))
print(unique(adae_data_filtered$AEDECOD))
print('Subjects reporting Menorrhagia :')
print(adae_data_filtered)

# Filters Menorrhagia subjects to the BNT only.
bnt_adae_data <- adae_data_filtered %>%
  filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")
print(bnt_adae_data)
total_bnt_menorrhagia <- nrow(bnt_adae_data)
total_bnt <- nrow(women_randomized_pop)
print(total_bnt_menorrhagia)
print(total_bnt)
menorrhagia_rate <- total_bnt_menorrhagia * 100 / total_bnt
print(paste('Rates of Menorrhagia / 100.000 : ', menorrhagia_rate))
