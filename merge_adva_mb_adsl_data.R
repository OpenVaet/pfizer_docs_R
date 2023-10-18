library(readr)
library(dplyr)

# Reading the input CSV file for ADSL data
adsl_data <- read_csv("csv_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.csv")

# Filtering and selecting necessary columns
adsl_data <- adsl_data %>%
  filter(RANDDT <= '2021-03-15') %>%
  select(SUBJID, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)

# Convert SUBJID to character type
adsl_data$SUBJID <- as.character(adsl_data$SUBJID)

# Reading the MB file
mb_data <- read_csv("csv_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.csv")

# Processing MB data
mb_data <- mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), MBDTC <= '2021-03-15', !is.na(VISIT), MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2', 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(SUBJID = substr(USUBJID, 15, 24),
         test_date = substr(MBDTC, 1, 10),
         test_type = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', 'PCR Local', 'PCR Central')) %>%
  select(SUBJID, MBDTC, VISIT, test_type, MBORRES)

# Reading the ADVA file
adva_data <- read_csv("csv_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.csv")

# Processing ADVA data
adva_data <- adva_data %>%
  filter(ADT <= '2021-03-15', PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(test_type = 'N-Binding') %>%
  select(SUBJID, ADT, VISIT, test_type, AVALC)

# Merging ADSL and MB data
merged_mb_data <- merge(adsl_data, mb_data, by = "SUBJID")

# Renaming columns for consistency
names(merged_mb_data)[names(merged_mb_data) == 'SUBJID'] <- 'subject_id'
names(merged_mb_data)[names(merged_mb_data) == 'RANDNO'] <- 'randomization_number'
names(merged_mb_data)[names(merged_mb_data) == 'AGETR01'] <- 'age_years'
names(merged_mb_data)[names(merged_mb_data) == 'SITEID'] <- 'site_id'
names(merged_mb_data)[names(merged_mb_data) == 'UNBLNDDT'] <- 'unblinding_date'
names(merged_mb_data)[names(merged_mb_data) == 'RANDDT'] <- 'randomization_date'
names(merged_mb_data)[names(merged_mb_data) == 'RFICDT'] <- 'screening_date'
names(merged_mb_data)[names(merged_mb_data) == 'VAX101DT'] <- 'dose_1_date'
names(merged_mb_data)[names(merged_mb_data) == 'VAX102DT'] <- 'dose_2_date'
names(merged_mb_data)[names(merged_mb_data) == 'VAX201DT'] <- 'dose_3_date'
names(merged_mb_data)[names(merged_mb_data) == 'VAX202DT'] <- 'dose_4_date'
names(merged_mb_data)[names(merged_mb_data) == 'MBDTC'] <- 'test_date'
names(merged_mb_data)[names(merged_mb_data) == 'VISIT'] <- 'test_visit'
names(merged_mb_data)[names(merged_mb_data) == 'MBORRES'] <- 'test_result'

# Merging ADSL and ADVA data
merged_adva_data <- merge(adsl_data, adva_data, by = "SUBJID")
names(merged_adva_data)[names(merged_adva_data) == 'SUBJID'] <- 'subject_id'
names(merged_adva_data)[names(merged_adva_data) == 'RANDNO'] <- 'randomization_number'
names(merged_adva_data)[names(merged_adva_data) == 'AGETR01'] <- 'age_years'
names(merged_adva_data)[names(merged_adva_data) == 'SITEID'] <- 'site_id'
names(merged_adva_data)[names(merged_adva_data) == 'UNBLNDDT'] <- 'unblinding_date'
names(merged_adva_data)[names(merged_adva_data) == 'RANDDT'] <- 'randomization_date'
names(merged_adva_data)[names(merged_adva_data) == 'RFICDT'] <- 'screening_date'
names(merged_adva_data)[names(merged_adva_data) == 'VAX101DT'] <- 'dose_1_date'
names(merged_adva_data)[names(merged_adva_data) == 'VAX102DT'] <- 'dose_2_date'
names(merged_adva_data)[names(merged_adva_data) == 'VAX201DT'] <- 'dose_3_date'
names(merged_adva_data)[names(merged_adva_data) == 'VAX202DT'] <- 'dose_4_date'
names(merged_adva_data)[names(merged_adva_data) == 'ADT'] <- 'test_date'
names(merged_adva_data)[names(merged_adva_data) == 'AVALC'] <- 'test_result'
names(merged_adva_data)[names(merged_adva_data) == 'VISIT'] <- 'test_visit'
print(colnames(merged_adva_data))
print(colnames(merged_mb_data))

# Combining both datasets
final_data <- rbind(merged_mb_data, merged_adva_data)

# Writing the final data to a CSV file
write.csv(final_data, file = "subjects_test_data.csv", row.names = FALSE)
