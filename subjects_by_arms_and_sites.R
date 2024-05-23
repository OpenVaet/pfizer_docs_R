# Load necessary package
library(haven)
library(dplyr)
library(readr)
library(jsonlite)
library(ggplot2)

# Loads trial sites data.
trial_sites_file <- 'trial_site_data.json'
trial_sites_data <- fromJSON(trial_sites_file, simplifyDataFrame = TRUE)
print(trial_sites_data)

# Reads the XPT file
adsl_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt')

# Extracts recruitment site id and convert 4444 to 1231
adsl_data <- adsl_data %>% 
  mutate(ORISITEID = substr(SUBJID, 1, 4)) %>%
  mutate(ORISITEID = ifelse(ORISITEID == "4444", "1231", ORISITEID))

# Counts the total of subjects screened, by site
subjects_screened_by_site <- adsl_data %>%
  group_by(ORISITEID) %>%
  summarise(Subjects_Screened = n(), .groups = 'drop')
print(subjects_screened_by_site)

# Joins the trial sites data with the subjects screened by site
# Makes sure that the column names used for joining are correct and present in both data frames
sites_frame <- merge(subjects_screened_by_site, trial_sites_data, by.x = "ORISITEID", by.y = "trial_site_id", all.x = TRUE)

# Filters columns
sites_frame <- sites_frame %>%
  select(ORISITEID, Country = country, Site_Name = name, Investigator = investigator, Latitude = latitude, Longitude = longitude, Subjects_Screened)

# Writes the data to a CSV file
write.csv(sites_frame, "subjects_screened_by_sites.csv", row.names = FALSE)

# Counts the total of unique entries in "SITEID"
unique_siteid_count <- length(unique(adsl_data$SITEID))
print(paste("Total unique SITEID entries: ", unique_siteid_count))

# Counts the number of subjects in each arm
arm_counts <- table(adsl_data$ARM)
print(arm_counts)

# Counts the total of subjects by country
subjects_by_country <- sites_frame %>%
  group_by(Country) %>%
  summarise(Total_Subjects = sum(Subjects_Screened))
print(subjects_by_country)

# Writes the data to a CSV file
write.csv(subjects_by_country, "subjects_by_country.csv", row.names = FALSE)

# Creates the plot
ggplot(subjects_by_country, aes(x = reorder(Country, -Total_Subjects), y = Total_Subjects)) +
  geom_bar(stat = "identity", fill = "#C8C8C8") +
  geom_text(aes(label = format(Total_Subjects, big.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 5,
            color = "black") +
  labs(x = "Country", y = "Total Subjects Screened", title = "C4591001 - Subjects Screened by Country") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(angle = 90, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16))

# Saves the plot as a PNG
ggsave("subjects_by_country.png", width = 8, height = 6, dpi = 300)