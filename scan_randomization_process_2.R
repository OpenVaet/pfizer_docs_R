library(readr)
library(dplyr)

# Loading previously identified ee8493 subjects.
ee8493_subjects_file <- 'ee8493_subjects.csv'
ee8493_subjects <- read_csv(ee8493_subjects_file, col_names = FALSE)
ee8493_subjects <- setNames(ee8493_subjects, c("subjectId"))
ee8493_subjects <- ee8493_subjects[-1,]

# Loading ADSL subjects, picking only subjects with randomization numbers between 400000 and 499999.
adsl_file <- 'csv_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.csv'
adsl_subjects <- read_csv(adsl_file)
adsl_subjects <- adsl_subjects %>% filter(RANDNO >= 400000, RANDNO <= 499999)

# Verifies that all the non-placebo subjects within this randomization range are indeed P2.
adsl_subjects <- adsl_subjects %>% mutate(ARM = ifelse(ARM != "Placebo" & !SUBJID %in% ee8493_subjects$subjectId, NA, ARM))

# Extract recruitment site id
adsl_subjects <- adsl_subjects %>% mutate(recruitment_site_id = substr(SUBJID, 1, 4))

# Count by arms and by sites
stats_by_arms <- adsl_subjects %>% group_by(ARM) %>% summarise(value = n()) %>% mutate(category = "by_arms", label = ARM)
stats_by_sites <- adsl_subjects %>% group_by(recruitment_site_id) %>% summarise(value = n()) %>% mutate(category = "by_sites", label = recruitment_site_id)

# Ensure the columns are in the same order
stats_by_arms <- stats_by_arms[, c("category", "label", "value")]
stats_by_sites <- stats_by_sites[, c("category", "label", "value")]

# Printing short report.
write.csv(rbind(stats_by_arms, stats_by_sites), file = "process_2_randomisation_analysis.csv", row.names = FALSE)
