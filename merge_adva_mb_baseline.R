library(readr)
library(dplyr)

# Reading the input CSV file for ADSL data
adsl_data <- read_csv("csv_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.csv")

# Filtering and selecting necessary columns
adsl_data <- adsl_data %>%
  filter(RANDDT <= '2021-03-15') %>%
  select(SUBJID)

# Convert SUBJID to character type
adsl_data$SUBJID <- as.character(adsl_data$SUBJID)

# Reading the MB file
mb_data <- read_csv("csv_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.csv")

# Processing MB data
mb_data <- mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), MBDTC < '2021-03-15', !is.na(VISIT), MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2', 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2'), VISIT == 'V1_DAY1_VAX1_L') %>%
  mutate(SUBJID = substr(USUBJID, 15, 24),
         test_date = substr(MBDTC, 1, 10),
         test_type = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', 'PCR Local', 'PCR Central'),
         MBORRES = case_when(
           MBORRES == 'INDETERMINATE' ~ 'IND',
           MBORRES == 'POSITIVE' ~ 'POS',
           MBORRES == 'NEGATIVE' ~ 'NEG',
           TRUE ~ MBORRES
         )) %>%
  select(SUBJID, test_date, VISIT, test_type, MBORRES)

# Reading the ADVA file
adva_data <- read_csv("csv_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.csv")

# Processing ADVA data
adva_data <- adva_data %>%
  filter(ADT < '2021-03-15', PARAM == 'N-binding antibody - N-binding Antibody Assay', VISIT == 'V1_DAY1_VAX1_L') %>%
  mutate(test_type = 'N-Binding',
         test_date = substr(ADT, 1, 10)) %>%
  select(SUBJID, test_date, VISIT, test_type, AVALC)

# Merging ADSL and MB data
merged_mb_data <- merge(adsl_data, mb_data, by = "SUBJID")

# Renaming columns for consistency
names(merged_mb_data)[names(merged_mb_data) == 'SUBJID'] <- 'subject_id'
names(merged_mb_data)[names(merged_mb_data) == 'VISIT'] <- 'test_visit' 
names(merged_mb_data)[names(merged_mb_data) == 'MBORRES'] <- 'test_result'

# Merging ADSL and ADVA data
merged_adva_data <- merge(adsl_data, adva_data, by = "SUBJID")
names(merged_adva_data)[names(merged_adva_data) == 'SUBJID'] <- 'subject_id'
names(merged_adva_data)[names(merged_adva_data) == 'AVALC'] <- 'test_result'
names(merged_adva_data)[names(merged_adva_data) == 'VISIT'] <- 'test_visit'

# Combining both datasets
final_data <- rbind(merged_mb_data, merged_adva_data)

# Writing the final data to a CSV file
write.csv(final_data, file = "subjects_test_data_baseline.csv", row.names = FALSE)