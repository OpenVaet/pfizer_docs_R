# Extracts ZIP to XPT.
library(tools)
library(zip)
library(httr)
library(rvest)
library(xml2)
library(haven)
library(dplyr)
library(tidyr)

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
face_selected_data <- face_data[c("USUBJID", "VISIT", "FADTC", "FATEST", "FASTRESC", "FAORRES", "FALNKGRP")]
distinct_test_face <- unique(face_selected_data$FATEST)
print(distinct_test_face)
face_filtered_data <- face_selected_data[face_selected_data$FATEST == 'Occurrence Indicator', ]
face_filtered_data$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", face_filtered_data$USUBJID)
print(face_filtered_data)

# Reads & filters the ADCEVD file.
adcevd_data <- read_xpt(adcevd_path)
adcevd_selected_data <- adcevd_data[c("SUBJID", "USUBJID", "ASTDT", "AENDT", "CELNKGRP", "CEGRPID", "CEOCCUR", "EVENTFL")]
adcevd_filtered_data <- adcevd_selected_data[adcevd_selected_data$CEGRPID %in% c('VACCINATION 1-SYSTEMIC', 'VACCINATION 2-SYSTEMIC'), ]
print(adcevd_filtered_data)

# Gets a list of all distinct SUBJID values in each file.
distinct_subjid_adae <- unique(adae_selected_data$SUBJID)
distinct_subjid_adcevd <- unique(adcevd_filtered_data$SUBJID)
distinct_subjid_adsympt <- unique(adsympt_filtered_data$SUBJID)
distinct_subjid_face <- unique(face_filtered_data$SUBJID)

# Adds the four flags to adsl_selected_data
adsl_selected_data$HASADAE <- adsl_selected_data$SUBJID %in% distinct_subjid_adae
adsl_selected_data$HASADCEVD <- adsl_selected_data$SUBJID %in% distinct_subjid_adcevd
adsl_selected_data$HASADSYMPT <- adsl_selected_data$SUBJID %in% distinct_subjid_adsympt
adsl_selected_data$HASFACE <- adsl_selected_data$SUBJID %in% distinct_subjid_face

# Converts the flags to 'Y' and 'N'
adsl_selected_data$HASADAE <- ifelse(adsl_selected_data$HASADAE, 'Y', 'N')
adsl_selected_data$HASADCEVD <- ifelse(adsl_selected_data$HASADCEVD, 'Y', 'N')
adsl_selected_data$HASADSYMPT <- ifelse(adsl_selected_data$HASADSYMPT, 'Y', 'N')
adsl_selected_data$HASFACE <- ifelse(adsl_selected_data$HASFACE, 'Y', 'N')

# Prints the updated adsl_selected_data
print(adsl_selected_data)
write.csv(adsl_selected_data, "symptoms_through_files_synthesis_by_subject.csv", row.names = FALSE)

# Creates a cross-tabulation of the four flags and ACTARM
flag_counts <- table(adsl_selected_data$ARM,
                     adsl_selected_data$HASADAE,
                     adsl_selected_data$HASADCEVD,
                     adsl_selected_data$HASADSYMPT,
                     adsl_selected_data$HASFACE)

# Converts the flag_counts array to a data frame
flag_counts_df <- as.data.frame.table(flag_counts)

# Renames the columns
names(flag_counts_df) <- c("ACTARM", "HASADAE", "HASADCEVD", "HASADSYMPT", "HASFACE", "Freq")

## Scopes on rows which only have positive results for symptoms in each file.
adsympt_pos_data <- adsympt_filtered_data[adsympt_filtered_data$AVALC == 'Y', ]
print(adsympt_pos_data)
face_pos_data <- face_filtered_data[face_filtered_data$FAORRES == 'Y', ]
face_pos_data <- face_pos_data[face_pos_data$VISIT != 'COVID_A' & face_pos_data$VISIT != 'COVID_B' & face_pos_data$VISIT != 'COVID_C' & face_pos_data$VISIT != 'COVID_D' & face_pos_data$VISIT != 'COVID_E' & face_pos_data$VISIT != 'COVID_F', ]

print(face_pos_data)
adcevd_pos_data <- adcevd_filtered_data[adcevd_filtered_data$CEOCCUR == 'Y', ]
print(adcevd_pos_data)

## Gets a list of all distinct SUBJID values in each positive subset
distinct_pos_subjid_adcevd <- unique(adcevd_pos_data$SUBJID)
distinct_pos_subjid_adsympt <- unique(adsympt_pos_data$SUBJID)
distinct_pos_subjid_face <- unique(face_pos_data$SUBJID)

## Adds the 3 flags to adsl_selected_data
adsl_selected_data$HASPOSADCEVD <- adsl_selected_data$SUBJID %in% distinct_pos_subjid_adcevd
adsl_selected_data$HASPOSADSYMPT <- adsl_selected_data$SUBJID %in% distinct_pos_subjid_adsympt
adsl_selected_data$HASPOSFACE <- adsl_selected_data$SUBJID %in% distinct_pos_subjid_face

## Converts the flags to 'Y' and 'N'
adsl_selected_data$HASPOSADCEVD <- ifelse(adsl_selected_data$HASPOSADCEVD, 'Y', 'N')
adsl_selected_data$HASPOSADSYMPT <- ifelse(adsl_selected_data$HASPOSADSYMPT, 'Y', 'N')
adsl_selected_data$HASPOSFACE <- ifelse(adsl_selected_data$HASPOSFACE, 'Y', 'N')

# Prints the updated adsl_selected_data
print(adsl_selected_data)
write.csv(adsl_selected_data, "symptoms_through_files_synthesis_by_subject.csv", row.names = FALSE)

print(adsympt_pos_data)
print(face_pos_data)
print(adcevd_pos_data)

# Splits the CELNKGRP column and create the new columns for adcevd_pos_data
adcevd_pos_data$VAXSTAGE <- sapply(strsplit(adcevd_pos_data$CELNKGRP, "-"), `[`, 1)
adcevd_pos_data$AENAME <- sapply(strsplit(adcevd_pos_data$CELNKGRP, "-"), `[`, 2)
# Splits the FALNKGRP column and create the new columns for face_pos_data
face_pos_data$VAXSTAGE <- sapply(strsplit(face_pos_data$FALNKGRP, "-"), `[`, 1)
face_pos_data$AENAME <- sapply(strsplit(face_pos_data$FALNKGRP, "-"), `[`, 2)

# Isolates the phase 1 subjects, and excludes them from the 3 tables.
phase_2_3_subjects <- adsl_selected_data[adsl_selected_data$PHASE != 'Phase 1', ]
phase_2_3_subjects <- phase_2_3_subjects[phase_2_3_subjects$ARM != 'SCREEN FAILURE' & phase_2_3_subjects$ARM != 'NOT ASSIGNED', ]
print(phase_2_3_subjects)

# Filters the dataframes to only include subjects in phase_2_3_subjects
face_pos_data <- face_pos_data[face_pos_data$SUBJID %in% phase_2_3_subjects$SUBJID, ]
adcevd_pos_data <- adcevd_pos_data[adcevd_pos_data$SUBJID %in% phase_2_3_subjects$SUBJID, ]
adsympt_pos_data <- adsympt_pos_data[adsympt_pos_data$SUBJID %in% phase_2_3_subjects$SUBJID, ]

# Counts the unique SUBJID for each AENAME in each of the datasets.
adsympt_pos_data <- rename(adsympt_pos_data, AENAME = PARAM)
face_pos_data_aes_counts <- face_pos_data %>%
  group_by(AENAME) %>%
  summarize(FACEUNIQSUBJS = n_distinct(SUBJID))
adcevd_pos_data_aes_counts <- adcevd_pos_data %>%
  group_by(AENAME) %>%
  summarize(ADCEVDUNIQSUBJS = n_distinct(SUBJID))
adsympt_pos_data_aes_counts <- adsympt_pos_data %>%
  group_by(AENAME) %>%
  summarize(ADSYMPTUNIQSUBJS = n_distinct(SUBJID))

# Merges the three dataframes
merged_aes_counts <- full_join(face_pos_data_aes_counts, 
                               adcevd_pos_data_aes_counts, 
                               by = "AENAME", 
                               suffix = c("_FACE", "_ADCEVD"))
merged_aes_counts <- full_join(merged_aes_counts,
                               adsympt_pos_data_aes_counts,
                               by = "AENAME",
                               suffix = c("", "_ADSYMPT"))
# Prints the results
print(merged_aes_counts, n = 100)

distinct_arms  <- unique(phase_2_3_subjects$ARM)
print(distinct_arms)
distinct_phases  <- unique(phase_2_3_subjects$PHASE)
print(distinct_phases)
print(face_pos_data, n = 100)

# Replaces "BNT162b2 Phase 2/3 (30 mcg)" with "BNT162b2"
phase_2_3_subjects$ARM[phase_2_3_subjects$ARM == "BNT162b2 Phase 2/3 (30 mcg)"] <- "BNT162b2"

# Counts the unique SUBJID for each AENAME in each of the datasets, grouped by ARM
face_pos_data_aes_counts_by_arm <- left_join(face_pos_data, phase_2_3_subjects[, c("SUBJID", "ARM")], by = "SUBJID") %>%
  group_by(AENAME, ARM) %>%
  summarize(FACEUNIQSUBJS = n_distinct(SUBJID))

adcevd_pos_data_aes_counts_by_arm <- left_join(adcevd_pos_data, phase_2_3_subjects[, c("SUBJID", "ARM")], by = "SUBJID") %>%
  group_by(AENAME, ARM) %>%
  summarize(ADCEVDUNIQSUBJS = n_distinct(SUBJID))

adsympt_pos_data_aes_counts_by_arm <- left_join(adsympt_pos_data, phase_2_3_subjects[, c("SUBJID", "ARM")], by = "SUBJID") %>%
  group_by(AENAME, ARM) %>%
  summarize(ADSYMPTUNIQSUBJS = n_distinct(SUBJID))

# Merges the three dataframes by AENAME and ARM
merged_aes_counts_by_arm <- full_join(face_pos_data_aes_counts_by_arm, 
                                     adcevd_pos_data_aes_counts_by_arm, 
                                     by = c("AENAME", "ARM"))
merged_aes_counts_by_arm <- full_join(merged_aes_counts_by_arm,
                                     adsympt_pos_data_aes_counts_by_arm,
                                     by = c("AENAME", "ARM"))

# Prints the results
print(merged_aes_counts_by_arm, n = 100)
write.csv(merged_aes_counts_by_arm, "merged_aes_counts_by_arm.csv", row.names = FALSE)

# Reshapes the merged_aes_counts_by_arm dataframe to the desired format
merged_aes_counts_by_arm_final <- merged_aes_counts_by_arm %>%
  pivot_wider(names_from = ARM, values_from = c(FACEUNIQSUBJS, ADCEVDUNIQSUBJS, ADSYMPTUNIQSUBJS))

# Prints the results
print(merged_aes_counts_by_arm_final, n = 100)
write.csv(merged_aes_counts_by_arm_final, "merged_aes_counts_by_arm_final.csv", row.names = FALSE)

# Combines the 3 datasets into one
all_pos_data <- bind_rows(face_pos_data, adcevd_pos_data, adsympt_pos_data)

# Joins the all_pos_data with phase_2_3_subjects to get the ARM information
combined_pos_data <- left_join(all_pos_data, phase_2_3_subjects, by = "SUBJID")

# Counts the unique SUBJID in each ARM that have reported at least one AENAME
unique_subjs_by_arms <- combined_pos_data %>%
  distinct(SUBJID, ARM, AENAME) %>%
  group_by(AENAME, ARM) %>%
  summarize(total_unique_subjid = n_distinct(SUBJID)) %>%
  pivot_wider(names_from = ARM, values_from = total_unique_subjid)

print(unique_subjs_by_arms, n = 100)
write.csv(unique_subjs_by_arms, "unique_subjs_by_arms.csv", row.names = FALSE)


