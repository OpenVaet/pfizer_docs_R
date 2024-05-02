library(haven)
library(dplyr)
library(lubridate)
library(stats)

protocol_devs_analysis_file <- 'xpt_data/FDA-CBER-2021-5683-0065774-to-0066700_125742_S1_M5_c4591001-A-D-addv.xpt'
protocol_devs_sup_file <- 'xpt_data/FDA-CBER-2021-5683-0174607 to -0178318_125742_S1_M5_c4591001-S-D-suppdv.xpt'

# Reads the primary XPT files
protocol_devs_analysis <- read_xpt(protocol_devs_analysis_file)
print(colnames(protocol_devs_analysis))
print(protocol_devs_analysis)

# Reads the supplementary XPT file.
protocol_devs_sup <- read_xpt(protocol_devs_sup_file)
print(protocol_devs_sup)

# Extracts the DVSEQ from the IDVARVAL column
protocol_devs_sup <- protocol_devs_sup %>%
  mutate(DVSEQ = as.numeric(gsub("\\D", "", IDVARVAL)))

# Flattens the QNAM and QVAL columns in the protocol_devs_sup dataset
flattened_sup <- protocol_devs_sup %>%
  group_by(USUBJID, DVSEQ) %>%
  summarize(
    SOURCE = paste(QVAL[QNAM == "SOURCE"], collapse = ", ")
  ) %>%
  ungroup()

# Joins the flattened_sup dataset to the protocol_devs_analysis dataset
flattened_data <- left_join(protocol_devs_analysis, flattened_sup, by = c("USUBJID", "DVSEQ"))

# Prints the flattened dataset
print(flattened_data)

# Filters out rows where DVSTDTC is after 2021-03-13
flattened_data <- flattened_data %>%
  filter(ymd(DVSTDTC) <= ymd("2021-03-13"))

# Creates the CONCATTERM column
flattened_data <- flattened_data %>%
  mutate(CONCATTERM = ifelse(!is.na(DVTERM1), paste(DVTERM, DVTERM1), DVTERM))

# Creates the SUBJID column
flattened_data <- flattened_data %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"))
print(flattened_data)

# Writes the flattened_data to a CSV file
write.csv(flattened_data, "deviations.csv", row.names = FALSE)

# Lists all the unique entries for "ARM"
unique_arms <- unique(flattened_data$ARM)
print(unique_arms)

# Filters the flattened_data dataframe
filtered_data <- flattened_data %>%
  filter(ARM %in% c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo"))

# Prints the filtered dataframe
print(filtered_data)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)

# Filter the filtered_data to only include subjects in the randomized population
filtered_data <- filtered_data[filtered_data$SUBJID %in% randomized_pop$SUBJID, ]

# Calculate the total of SUBJID in randomized_pop for each ARM
arm_counts <- randomized_pop %>%
  group_by(ARM) %>%
  summarize(total_subjects = n_distinct(SUBJID))

print(arm_counts)

# Counts the unique SUBJID for each CONCATTERM and ARM
deviation_counts <- filtered_data %>%
  distinct(SUBJID, CONCATTERM, ARM) %>%
  group_by(CONCATTERM) %>%
  summarize(
    BNT_SUBJECTS = sum(ARM == "BNT162b2 Phase 2/3 (30 mcg)"),
    PLACEBO_SUBJECTS = sum(ARM == "Placebo")
  ) %>%
  ungroup() %>%
  mutate(TOTAL_SUBJECTS = BNT_SUBJECTS + PLACEBO_SUBJECTS)


# Filters out deviations with under 100 TOTAL_SUBJECTS
deviation_counts <- deviation_counts %>%
  filter(TOTAL_SUBJECTS >= 100)

# Writes the result to a CSV file
write.csv(deviation_counts, "deviations.csv", row.names = FALSE)
print(deviation_counts, n=120)
