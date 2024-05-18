library(readr)
library(dplyr)

# Reading the input CSV file for ADSL data
adsl_data <- read_csv("csv_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.csv")

# Filtering and selecting necessary columns
adsl_data <- adsl_data %>%
  filter(RANDDT <= '2021-03-15') %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)
print(adsl_data)

# Convert SUBJID to character type
adsl_data$SUBJID <- as.character(adsl_data$SUBJID)

# Reading the MB file
mb_data <- read_csv("csv_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.csv")

# Processing MB data
mb_data <- mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), MBDTC < '2021-03-15', !is.na(VISIT), MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2', 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(SUBJID = substr(USUBJID, 15, 24),
         TESTDATE = substr(MBDTC, 1, 10),
         TESTTYPE = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', 'PCR Local', 'PCR Central'),
         MBORRES = case_when(
           MBORRES == 'INDETERMINATE' ~ 'IND',
           MBORRES == 'POSITIVE' ~ 'POS',
           MBORRES == 'NEGATIVE' ~ 'NEG',
           TRUE ~ MBORRES
         )) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, MBORRES)
print(mb_data)

# Reading the ADVA file
adva_data <- read_csv("csv_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.csv")
print(adva_data)

# Processing ADVA data
adva_data <- adva_data %>%
  filter(ADT < '2021-03-15', PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(TESTTYPE = 'N-Binding',
         TESTDATE = substr(ADT, 1, 10)) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, AVALC)

# Merging ADSL and MB data
merged_mb_data <- merge(adsl_data, mb_data, by = "SUBJID")
print(merged_mb_data)

# Renaming columns for consistency
names(merged_mb_data)[names(merged_mb_data) == 'MBORRES'] <- 'TESTRESULT'

# Merging ADSL and ADVA data
merged_adva_data <- merge(adsl_data, adva_data, by = "SUBJID")
names(merged_adva_data)[names(merged_adva_data) == 'AVALC'] <- 'TESTRESULT'

# Combining both datasets
final_data <- rbind(merged_mb_data, merged_adva_data)

print(final_data)

# Writing the final data to a CSV file
write.csv(final_data, file = "subjects_test_data.csv", row.names = FALSE)
