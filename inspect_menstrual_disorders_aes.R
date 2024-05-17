# Extracts ZIP to XPT.
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "ARM", "VPHASE", "AREL", "AERELTXT", "AESTDTC", "AEENDTC", "AEDECOD")]
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

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
women_randomized_pop <- randomized_pop %>%
  filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")
women_randomized_pop <- women_randomized_pop %>%
  filter(AGE >= 16 & AGE <= 39 & SEX == 'F')
print(women_randomized_pop)

total_bnt_menorrhagia <- nrow(bnt_adae_data)
total_bnt <- nrow(women_randomized_pop)
print(total_bnt_menorrhagia)
print(total_bnt)
menorrhagia_rate <- total_bnt_menorrhagia * 100 / total_bnt
print(paste('Rates of Menorrhagia / 100.000 : ', menorrhagia_rate))

      