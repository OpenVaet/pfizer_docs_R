library(haven)
library(dplyr)
library(lubridate)
library(tidyr)

# Reads the XPT file
adsl_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt')

# Converts RFICDT column to Date format
adsl_data$RFICDT <- ymd(adsl_data$RFICDT)
adsl_data$RANDDT <- ymd(adsl_data$RANDDT)
adsl_data$SITEID <- as.integer(adsl_data$SITEID)
adsl_data$SUBJID <- as.integer(adsl_data$SUBJID)
print(adsl_data)

fa <- read.csv("csv_data/discontinued_subjects_fa.csv")
m6 <- read.csv("csv_data/discontinued_subjects_m6.csv")
print(fa)
print(m6)

# Count the total number of subjects screened and randomized for each SITEID
site_summary <- adsl_data %>%
  group_by(SITEID) %>%
  summarize(
    total_screened = sum(!is.na(RFICDT)),
    total_randomized = sum(!is.na(RANDDT)),
    total_discontinued = sum(!is.na(EOSDCDT))
  )

# Print the summary
print(site_summary)

# Define the cutoff date
cutoff_date <- as.Date("2020-11-19")

# Filter the data up to the cutoff date and calculate totals per SITEID
site_summary_cutoff <- adsl_data %>%
  filter(as.Date(RFICDT) <= cutoff_date | as.Date(RANDDT) <= cutoff_date) %>%
  group_by(SITEID) %>%
  summarize(
    total_screened = sum(!is.na(RFICDT) & as.Date(RFICDT) <= cutoff_date),
    total_randomized = sum(!is.na(RANDDT) & as.Date(RANDDT) <= cutoff_date)
  )

# Print the summary limited to the cutoff date
print(site_summary_cutoff, n=200)


# Join site_summary and site_summary_cutoff by SITEID
comparison <- site_summary %>%
  inner_join(site_summary_cutoff, by = "SITEID", suffix = c("_total", "_cutoff"))

# Filter for SITEIDs where both total_screened and total_randomized haven't changed
unchanged_sites <- comparison %>%
  filter(total_screened_total == total_screened_cutoff, total_randomized_total == total_randomized_cutoff) %>%
  select(SITEID)

# Print the list of SITEIDs where totals haven't changed
print(unchanged_sites, n=150)







# Merge the m6 data, keeping only matching subjects in rando. pop from m6
merged_data_m6 <- m6 %>%
  inner_join(adsl_data, by = c("M6_SUBJID" = "SUBJID"))

# Calculate the summary by SITEID
m6_summary <- merged_data_m6 %>%
  group_by(SITEID) %>%
  summarize(
    total_subjects = n(),
    total_screened = sum(!is.na(RFICDT)),
    total_randomized = sum(!is.na(RANDDT))
  )

# Print the summary
print(m6_summary, n=200)

# Merge the fa data, keeping only matching subjects in rando. pop from fa
merged_data_fa <- fa %>%
  inner_join(adsl_data, by = c("FA_SUBJID" = "SUBJID"))

# Calculate the summary by SITEID
fa_summary <- merged_data_fa %>%
  group_by(SITEID) %>%
  summarize(
    total_subjects = n(),
    total_screened = sum(!is.na(RFICDT)),
    total_randomized = sum(!is.na(RANDDT))
  )

# Print the summary
print(fa_summary, n=200)

# Merge summaries and calculate the difference in total_randomized by SITEID (M6 - FA)
randomized_diff_summary <- m6_summary %>%
  select(SITEID, total_randomized_M6 = total_randomized) %>%
  full_join(
    fa_summary %>% select(SITEID, total_randomized_FA = total_randomized),
    by = "SITEID"
  ) %>%
  mutate(total_randomized_diff = total_randomized_M6 - total_randomized_FA)

# Print the difference summary
print(randomized_diff_summary, n = 200)

write.csv(randomized_diff_summary, "csv_data/discontinued_between_fa_and_m6.csv", row.names = FALSE)

offsets_rando <- read.csv("offset_randomization_between_fa_m6.csv")
print(offsets_rando)

# Rename offsets_rando.Trial.Site.ID to SITEID
offsets_rando <- offsets_rando %>%
  rename(SITEID = Trial.Site.ID)
offsets_rando$Offset_Investigator <- abs(offsets_rando$Offset.M6.FA)
offsets_rando$M6_investig_randomized <- abs(offsets_rando$M6)

# Create a new dataframe containing SITEID, Offset.M6.FA, and randomized_diff_summary.total_randomized_diff
result_summary <- offsets_rando %>%
  select(SITEID, Offset_Investigator) %>%
  inner_join(
    randomized_diff_summary %>%
      select(SITEID, Offset_Discontinuations = total_randomized_diff),
    by = "SITEID"
  ) %>%
  inner_join(
    site_summary %>%
      select(SITEID, ADSL_randomized = total_randomized),
    by = "SITEID"
  ) %>%
  inner_join(
    offsets_rando %>%
      select(SITEID, M6_investig_randomized = M6_investig_randomized),
    by = "SITEID"
  )

# Calculate offset_ADSL_Investigator and Offset_of_offsets
result_summary <- result_summary %>%
  mutate(
    offset_ADSL_Investigator = ADSL_randomized - M6_investig_randomized,
    Offset_of_offsets = Offset_Investigator - offset_ADSL_Investigator
  )


# Print the result summary
print(result_summary)

write.csv(result_summary, "csv_data/offset_randomization_between_fa_m6_invesig_and_discontinuations.csv", row.names = FALSE)


