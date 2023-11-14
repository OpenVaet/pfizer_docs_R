library(readr)
library(stringr)
library(dplyr)

adsl_file <- "csv_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.csv"

column_types <- cols(
  .default = col_character(),
  SUBJID = col_double(),
  PHASEN = col_double(),
  AGEGR1N = col_double(),
  SAFFL = col_character(),
  MULENRFL = col_character(),
  HIVFL = col_character(),
  TRT01A = col_character()
)

data <- read_csv(adsl_file, na = c("", "NA"), col_types = column_types)

filtered_data <- data %>%
  filter(
    !is.na(PHASEN),
    !is.na(AGEGR1N),
    !is.na(SAFFL),
    !is.na(HIVFL),
    !is.na(TRT01A),
    PHASEN > 1,
    AGEGR1N > 1,
    SAFFL == "Y",
    HIVFL != "Y",
    TRT01A != "",
    is.na(MULENRFL) | MULENRFL != "Y"
  )

stats <- filtered_data %>%
  group_by(COVBLST) %>%
  summarise(n = n())
stats <- stats %>%
  rename(`Total Subjects` = n)

write.table(stats, "covblst_status_distribution.csv", row.names = FALSE, quote = FALSE, sep = ";")

total_subjects <- sum(stats$n)
print(paste('Total Subjects : ', total_subjects))


print('COVBLST Distribution : ')
print(stats)
