library(MASS)
library(readr)
library(haven)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
print(randomized_pop)

# Loads Process 2 subjects.
process_2_subjects <- read_csv("process_2_recipients_by_randomization_numbers.csv")
print(process_2_subjects)

# Loads ADSL.
adsl_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt')
adsl_data <- adsl_data[adsl_data$SUBJID %in% randomized_pop$SUBJID, ]
adsl_selected_data <- adsl_data[c("SUBJID", "COMBODFL", "BMICATN", "OBESEFL", "COUNTRY")]
print(colnames(adsl_data))
print(adsl_data)

# Loads AEs.
adae_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt')
adae_selected_data <- adae_data[c("SUBJID", "AESTDTC", "AEDECOD")]
adae_selected_data <- adae_selected_data[adae_selected_data$SUBJID %in% randomized_pop$SUBJID, ]
print(adae_selected_data)

# Merge adsl_data with randomized_pop on SUBJID
print(adsl_selected_data)
merged_data <- merge(randomized_pop, adsl_selected_data, by = "SUBJID")

# Add COMORBIDITIES, OBESE, MALE & PROCESS2 column based on the specified conditions
merged_data$COMORBIDITIES <- ifelse(merged_data$COMBODFL == "Y", 1, 0)
merged_data$OBESE <- ifelse(merged_data$BMICATN == 4 & merged_data$AGETR01 >= 16, 1, 0)
merged_data$MALE <- ifelse(merged_data$SEX == "M", 1, 0)
merged_data$PROCESS2 <- ifelse(merged_data$SUBJID %in% process_2_subjects$SUBJID, 1, 0)
merged_data$AECOUNT <- sapply(merged_data$SUBJID, function(x) sum(adae_selected_data$SUBJID == x))
merged_data$VAX101DT <- as.Date(merged_data$VAX101DT)
merged_data$UNBLNDDT <- as.Date(merged_data$UNBLNDDT)

merged_data$UNBLINDEXPOSURE <- ifelse(is.na(merged_data$UNBLNDDT), 
                                      as.numeric(as.Date("2021-03-13") - merged_data$VAX101DT), 
                                      as.numeric(merged_data$UNBLNDDT - merged_data$VAX101DT))
merged_data$UNBLINDEXPOSURE <- as.numeric(merged_data$UNBLINDEXPOSURE)
print(merged_data)

# Filters the population to sustain only US citizen & subjects under 56
filtered_data <- merged_data[merged_data$COUNTRY == "USA" & merged_data$AGE <= 55 & merged_data$ARM == 'BNT162b2 Phase 2/3 (30 mcg)', ]
print(filtered_data)

# Compared to all others treatment subjects
model1 <- glm(AECOUNT ~ PROCESS2 + MALE + OBESE + COMORBIDITIES, 
              family = poisson(link = "log"), 
              data = filtered_data, 
              offset = log(filtered_data$UNBLINDEXPOSURE))
summary(model1)

# Calculates the percentage change in AECOUNT for a one-unit increase in PROCESS2
process2_effect <- exp(coef(model1)[2]) - 1
cat("The effect of PROCESS2 on AECOUNT is a", round(process2_effect * 100, 2), "% increase.\n")

# Calculates the ratio of the coefficient estimate for PROCESS2 to the coefficient estimate for MALE
male_ratio <- coef(model1)[2] / coef(model1)[3]
cat("The effect of PROCESS2 on AECOUNT is", round(male_ratio, 2), "times larger than the effect of MALE.\n")

# Calculates the ratio of the coefficient estimate for PROCESS2 to the coefficient estimate for OBESE
obese_ratio <- coef(model1)[2] / coef(model1)[4]
cat("The effect of PROCESS2 on AECOUNT is", round(obese_ratio, 2), "times larger than the effect of OBESE.\n")

# Calculates the ratio of the coefficient estimate for PROCESS2 to the coefficient estimate for COMORBIDITIES
comorbidities_ratio <- coef(model1)[2] / coef(model1)[5]
cat("The effect of PROCESS2 on AECOUNT is", round(comorbidities_ratio, 2), "times larger than the effect of COMORBIDITIES.\n")

# Compared to all others male, non obese treatment subjects
non_obese_males_data <- filtered_data[filtered_data$MALE == 1 & filtered_data$OBESE == 0, ]
print(non_obese_males_data)
table(non_obese_males_data$PROCESS2)
model2 <- glm(AECOUNT ~ PROCESS2 + MALE + OBESE + COMORBIDITIES, 
              family = poisson(link = "log"), 
              data = non_obese_males_data, 
              offset = log(non_obese_males_data$UNBLINDEXPOSURE))
summary(model2)

# Calculates the percentage change in AECOUNT for a one-unit increase in PROCESS2
process2_effect <- exp(coef(model2)[2]) - 1
cat("The effect of PROCESS2 on AECOUNT is a", round(process2_effect * 100, 2), "% increase.\n")

# Calculates the ratio of the coefficient estimate for PROCESS2 to the coefficient estimate for COMORBIDITIES
comorbidities_ratio <- coef(model2)[2] / coef(model2)[5]
cat("The effect of PROCESS2 on AECOUNT is", round(comorbidities_ratio, 2), "times larger than the effect of COMORBIDITIES.\n")

