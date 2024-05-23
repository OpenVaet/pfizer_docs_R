library(haven)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Loads the official EUA cases.
official_cases_file <- 'eua_official_cases.csv'
official_cases <- read.csv(official_cases_file)

# Loads the randomized population data & filters it on the official cases.
print(official_cases)
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
print(randomized_pop)
cases_randomized_data <- randomized_pop[randomized_pop$SUBJID %in% official_cases$SUBJID, ]
print(cases_randomized_data)
print(colnames(cases_randomized_data))
cases_randomized_data <- cases_randomized_data %>% 
  mutate(DAYSBETWEENDOSE = as.Date(VAX102DT) - as.Date(VAX101DT))

# Prints the cases who weren't within the protocol planned window.
cases_randomized_data %>% 
  filter(DAYSBETWEENDOSE > 23) %>% 
  print()

# Loads the deviations & filters them on EUA cases.
deviations_file <- 'deviations.csv'
deviations <- read.csv(deviations_file)
deviations_data <- deviations[c("SUBJID", "ARM", "DVSTDTC", "DVCAT", "EPOCH", "DVDECOD", "CONCATTERM")]
deviations_data <- deviations_data[deviations_data$SUBJID %in% official_cases$SUBJID, ]

# Filtering deviations mentioned.
deviations_data <- deviations_data %>%
  filter(CONCATTERM == 'Dosing/administration error, subject did not receive correct dose of vaccine ' |
           CONCATTERM == 'Receipt of blood/plasma products or immunoglobulins within 60 days before enrollment through conclusion of the study. ')

print(deviations_data)

write.csv(deviations_data, "deviations_data_eua.csv", row.names = FALSE)




