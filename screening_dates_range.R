# Load necessary packages
library(haven)
library(lubridate)
library(dplyr)
library(tidyr)

# Reads the XPT file
adsl_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt')

# Converts RFICDT column to Date format
adsl_data$RFICDT <- ymd(adsl_data$RFICDT)

# Extracts recruitment site id and convert 4444 to 1231
adsl_data <- adsl_data %>%
  mutate(ORISITEID = substr(SUBJID, 1, 4)) %>%
  mutate(ORISITEID = ifelse(ORISITEID == "4444", "1231", ORISITEID))

# Filters out subjects without a proper RFICDT date
adsl_data <- adsl_data %>% filter(!is.na(RFICDT))

# Creates a data frame with all weeks between the earliest and latest date
all_weeks <- data.frame(
  year_week = seq(from = floor_date(min(adsl_data$RFICDT), "week"),
                  to = ceiling_date(max(adsl_data$RFICDT), "week"),
                  by = "week")
) %>%
  mutate(year_week = paste0(year(year_week), "-", isoweek(year_week)))

# Groups by year and week number, then summarise
weekly_recruitment <- adsl_data %>%
  mutate(year_week = paste0(year(RFICDT), "-", isoweek(RFICDT))) %>%
  group_by(year_week) %>%
  summarise(subjects_recruited = n(), .groups = 'drop')

# Joins all_weeks with weekly_recruitment to include weeks with zero recruitment
weekly_recruitment <- all_weeks %>%
  left_join(weekly_recruitment, by = "year_week") %>%
  replace_na(list(subjects_recruited = 0))

# Writes the data to a CSV file
write.csv(weekly_recruitment, "subjects_screened_weekly.csv", row.names = FALSE)

# Fetches the earliest and latest date of screening
earliest_date <- min(adsl_data$RFICDT, na.rm = TRUE)
latest_date <- max(adsl_data$RFICDT, na.rm = TRUE)

# Outputs the earliest and latest date
print(paste("Earliest date of screening: ", earliest_date))
print(paste("Latest date of screening: ", latest_date))
