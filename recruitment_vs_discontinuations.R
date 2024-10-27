library(dplyr)

m6 <- read.csv("discontinued_subjects_m6.csv")
print(m6)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
print(randomized_pop)

# Count the total number of subjects screened and randomized for each SITEID
site_summary <- randomized_pop %>%
  group_by(SITEID) %>%
  summarize(
    total_screened = sum(!is.na(RFICDT)),
    total_randomized = sum(!is.na(RANDDT))
  )

# Print the summary
print(site_summary)

# Define the cutoff date
cutoff_date <- as.Date("2020-11-19")

# Filter the data up to the cutoff date and calculate totals per SITEID
site_summary_cutoff <- randomized_pop %>%
  filter(as.Date(RFICDT) <= cutoff_date | as.Date(RANDDT) <= cutoff_date) %>%
  group_by(SITEID) %>%
  summarize(
    total_screened = sum(!is.na(RFICDT) & as.Date(RFICDT) <= cutoff_date),
    total_randomized = sum(!is.na(RANDDT) & as.Date(RANDDT) <= cutoff_date)
  )

# Print the summary limited to the cutoff date
print(site_summary_cutoff, n=200)



# Merge the data, keeping only matching subjects from m6
merged_data <- m6 %>%
  inner_join(randomized_pop, by = c("M6_SUBJID" = "SUBJID"))

# Calculate the summary by SITEID
m6_summary <- merged_data %>%
  group_by(SITEID) %>%
  summarize(
    total_subjects = n(),
    total_screened = sum(!is.na(RFICDT)),
    total_randomized = sum(!is.na(RANDDT))
  )

# Print the summary
print(m6_summary, n=200)


# Join site_summary and site_summary_cutoff by SITEID
comparison <- site_summary %>%
  inner_join(site_summary_cutoff, by = "SITEID", suffix = c("_total", "_cutoff"))

# Filter for SITEIDs where both total_screened and total_randomized haven't changed
unchanged_sites <- comparison %>%
  filter(total_screened_total == total_screened_cutoff, total_randomized_total == total_randomized_cutoff) %>%
  select(SITEID)

# Print the list of SITEIDs where totals haven't changed
print(unchanged_sites, n=150)
