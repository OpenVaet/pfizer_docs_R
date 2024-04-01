library(readr)
library(dplyr)

# Loading ADVA data.
adva_file <- 'csv_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.csv'

load_adva <- function() {
  data <- read_csv(adva_file)
  
  # Filters data
  data <- data %>%
    filter(ADT <= as.Date('2020-11-15')) %>%
    filter(PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
    mutate(Arm = ifelse(ACTARM == 'Placebo', 'Placebo', 
                        ifelse(ACTARM == 'BNT162b2 Phase 2/3 (30 mcg)', 'BNT162b2', NA))) %>%
    filter(!is.na(Arm)) %>%
    mutate(Condition = ifelse((VISIT == 'V1_DAY1_VAX1_L' & AVALC == 'NEG') | 
                              (VISIT == 'V3_MONTH1_POSTVAX2_L' & AVALC == 'POS'), 1, NA)) %>%
    filter(!is.na(Condition)) %>%
    group_by(Arm, SUBJID) %>%
    summarise(Count = n()) %>%
    filter(Count == 2)
  
  return(data)
}

subjects <- load_adva()

# Prints the results
cat("Placebo: ", nrow(filter(subjects, Arm == 'Placebo')), "\n")
cat("Vaccinated: ", nrow(filter(subjects, Arm == 'BNT162b2')), "\n")
