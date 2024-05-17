library(haven)
library(dplyr)
library(lubridate)

# Loading medical history file.
medical_history <- read_xpt('xpt_data/FDA-CBER-2021-5683-1053274-1058198_125742_S1_M5_C4591001-S-D-mh.xpt')
print(colnames(medical_history))
print(medical_history)

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
medical_history$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", medical_history$USUBJID)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
randomized_pop <- randomized_pop %>%
  filter(ARM == 'BNT162b2 Phase 2/3 (30 mcg)' & SEX == 'F')
print(randomized_pop)
medical_history_filtered <- medical_history[medical_history$SUBJID %in% randomized_pop$SUBJID, ]
print(medical_history_filtered)

medical_history_with_bleedings <- medical_history_filtered %>%
  filter(MHDECOD %in% c(
    "Menorrhagia"
  ))
medical_history_with_potential_bleedings <- medical_history_filtered %>%
  filter(MHDECOD %in% c(
    "Menorrhagia", "Dysfunctional uterine bleeding",
    "Ovarian haemorrhage", "Vaginal cyst",
    "Vulvovaginal pain", "Haemorrhage",
    "Ovarian cyst", "Ovarian rupture",
    "Ovarian cyst ruptured", "Ovarian cancer",
    "Benign ovarian tumour", "Ovarian mass"
  ))
subjects_with_bleedings <- length((unique(medical_history_with_bleedings$SUBJID)))
print(subjects_with_bleedings)
print(length(subjects_with_bleedings))

print(unique(medical_history_with_bleedings$MHENTPT)) 

# Loads the Phase 3 population randomized.
women_randomized_pop <- randomized_pop %>%
  filter(AGE >= 16 & AGE <= 39 & SEX == 'F')
total_bnt <- nrow(women_randomized_pop)

# Calculates the rates or Menorrhagia among past history.
menorrhagia_rate <- subjects_with_bleedings * 100 / total_bnt
print(paste('Rates of Menorrhagia / 100.000 : ', menorrhagia_rate))

# Calculates the rates of other troubles whihc could be used to classify Menorrhagia
subjects_with_potential_bleedings <- length((unique(medical_history_with_potential_bleedings$SUBJID)))
menorrhagia_and_others_rate <- subjects_with_potential_bleedings * 100 / total_bnt
print(paste('Rates of Menorrhagia & other bleeding causes / 100.000 : ', menorrhagia_and_others_rate))
