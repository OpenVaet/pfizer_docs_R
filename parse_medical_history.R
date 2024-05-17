library(haven)
library(dplyr)
library(lubridate)

# Loading medical history file.
medical_history <- read_xpt('xpt_data/FDA-CBER-2021-5683-1053274-1058198_125742_S1_M5_C4591001-S-D-mh.xpt')
print(colnames(medical_history))
print(medical_history)

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
medical_history$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", medical_history$USUBJID)

medical_history_filtered <- medical_history %>%
  filter(SUBJID %in% c("10071087", "10011061", "11281119", "12313161", "12641025"))
print(unique(medical_history$MHDECOD))
print(medical_history_filtered, n=30)
write.csv(unique(medical_history$MHDECOD), 'medical_history_decod.csv')
