# Extracts ZIP to XPT.
library(tools)
library(zip)
library(httr)
library(rvest)
library(xml2)
library(haven)

# Verifies if the requireed files have been properly retrieved.
# ADSL
adsl_path <- 'xpt_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt'
if (!file.exists(adsl_path)) {
  stop("ADSL file not found", call. = FALSE)
}
# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}
# ADSYMPT
adsympt_path <- 'xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt'
if (!file.exists(adsympt_path)) {
  stop("ADAE file not found", call. = FALSE)
}
# FACE
face_path <- 'xpt_data/FDA-CBER-2021-5683-0484461-0537913-125742_S1_M5_c4591001-S-D-face.xpt'
if (!file.exists(face_path)) {
  stop("FACE file not found", call. = FALSE)
}
# ADCEVD
adcevd_path <- 'xpt_data/FDA-CBER-2021-5683-0059000 to -0065773_125742_S1_M5_c4591001-A-D-adcevd.xpt'
if (!file.exists(adcevd_path)) {
  stop("ADCEVD file not found", call. = FALSE)
}

# Reads & filters the ADSL file
adsl_data <- read_xpt(adsl_path)
adsl_selected_data <- adsl_data[c("SUBJID", "USUBJID", "PHASE", "RFICDT", "RANDDT", "AGE", "SEX", "ARM", "ACTARM", "VAX101DT", "VAX102DT", "VAX201DT", "VAX202DT", "UNBLNDDT")]
print(adsl_selected_data)

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "USUBJID", "VPHASE", "APERIODC", "AREL", "AEHLGT", "AEHLT", "AESER", "AERELTXT", "ATOXGR", "AESTDTC", "AEENDTC")]
print(adae_selected_data)

# Reads & filters the ADSYMPT file.
adsympt_data <- read_xpt(adsympt_path)
adsympt_selected_data <- adsympt_data[c("USUBJID", "PARCAT1", "PARAM", "AVISIT", "AVALC", "ADT", "AENDT")]
adsympt_filtered_data <- adsympt_selected_data[adsympt_selected_data$PARCAT1 == 'SIGNS AND SYMPTOMS OF DISEASE', ]
adsympt_filtered_data$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adsympt_filtered_data$USUBJID)
print(adsympt_filtered_data)

# Reads & filters the FACE file.
face_data <- read_xpt(face_path)
face_selected_data <- face_data[c("USUBJID", "VISIT", "FADTC", "FATEST", "FASTRESC")]
face_filtered_data <- face_selected_data[face_selected_data$FATEST %in% c('First Symptom Date', 'Last Symptom Resolved Date'), ]
face_filtered_data$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", face_filtered_data$USUBJID)
print(face_filtered_data)

# Reads & filters the ADCEVD file.
adcevd_data <- read_xpt(adcevd_path)
adcevd_selected_data <- adcevd_data[c("SUBJID", "USUBJID", "ASTDT", "AENDT", "CELNKGRP", "CEGRPID", "CEOCCUR", "EVENTFL")]
adcevd_filtered_data <- adcevd_selected_data[adcevd_selected_data$CEGRPID %in% c('VACCINATION 1-SYSTEMIC', 'VACCINATION 2-SYSTEMIC'), ]
print(adcevd_filtered_data)

# Get a list of all distinct SUBJID values in each file.
distinct_subjid_adae <- unique(adae_selected_data$SUBJID)
distinct_subjid_adcevd <- unique(adcevd_filtered_data$SUBJID)
distinct_subjid_adsympt <- unique(adsympt_filtered_data$SUBJID)
distinct_subjid_face <- unique(face_filtered_data$SUBJID)

# Add the four flags to adsl_selected_data
adsl_selected_data$HASADAE <- adsl_selected_data$SUBJID %in% distinct_subjid_adae
adsl_selected_data$HASADCEVD <- adsl_selected_data$SUBJID %in% distinct_subjid_adcevd
adsl_selected_data$HASADSYMPT <- adsl_selected_data$SUBJID %in% distinct_subjid_adsympt
adsl_selected_data$HASFACE <- adsl_selected_data$SUBJID %in% distinct_subjid_face

# Convert the flags to 'Y' and 'N'
adsl_selected_data$HASADAE <- ifelse(adsl_selected_data$HASADAE, 'Y', 'N')
adsl_selected_data$HASADCEVD <- ifelse(adsl_selected_data$HASADCEVD, 'Y', 'N')
adsl_selected_data$HASADSYMPT <- ifelse(adsl_selected_data$HASADSYMPT, 'Y', 'N')
adsl_selected_data$HASFACE <- ifelse(adsl_selected_data$HASFACE, 'Y', 'N')

# Print the updated adsl_selected_data
print(adsl_selected_data)
write.csv(adsl_selected_data, "symptoms_through_files_synthesis_by_subject.csv", row.names = FALSE)
