# Load the required libraries
library(tidyverse)

# Read the CSV files
file1 <- "process_2_recipients_by_randomization_numbers.csv"
file2 <- "process_2_recipients_by_date_and_sites.csv"

data1 <- read_csv(file1)
data2 <- read_csv(file2)

# Check if both files have the same number of rows
if (nrow(data1) == nrow(data2)) {
  print("Both files have the same number of rows.")
} else {
  print("The files have a different number of rows.")
}

# Check if both files contain the same unique SUBJID entries
unique_subjid_1 <- unique(data1$SUBJID)
unique_subjid_2 <- unique(data2$SUBJID)

if (all(unique_subjid_1 %in% unique_subjid_2) && all(unique_subjid_2 %in% unique_subjid_1)) {
  print("Both files contain the same unique SUBJID entries.")
} else {
  print("The files do not contain the same unique SUBJID entries.")
}

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)

# Filter data to only include ARM = BNT162b2 Phase 2/3 (30 mcg)
data1_filtered <- data1 %>% filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")
randomized_pop_filtered <- randomized_pop %>% filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")

# Count the number of subjects recruited per day for each dataset
recruitment_by_day <- data.frame(RANDDT = unique(c(as.character(data1_filtered$RANDDT), as.character(randomized_pop_filtered$RANDDT))))

# Count the number of subjects for each dataset
process_2_subjects <- data1_filtered %>%
  mutate(RANDDT = as.character(RANDDT)) %>%
  group_by(RANDDT) %>%
  summarize(count = n())

process_1_subjects <- randomized_pop_filtered %>%
  mutate(RANDDT = as.character(RANDDT)) %>%
  group_by(RANDDT) %>%
  summarize(count = n())

# Merge the counts into the recruitment_by_day dataframe
recruitment_by_day <- left_join(recruitment_by_day, process_2_subjects, by = "RANDDT")
recruitment_by_day <- left_join(recruitment_by_day, process_1_subjects, by = "RANDDT")

# Rename the count columns
names(recruitment_by_day)[names(recruitment_by_day) == "count.x"] <- "Process 2 Subjects"
names(recruitment_by_day)[names(recruitment_by_day) == "count.y"] <- "Process 1 Subjects"

# Fill in missing values with 0
recruitment_by_day$`Process 2 Subjects` <- replace_na(recruitment_by_day$`Process 2 Subjects`, 0)
recruitment_by_day$`Process 1 Subjects` <- replace_na(recruitment_by_day$`Process 1 Subjects`, 0)
print(recruitment_by_day)

# Convert RANDDT to date type
recruitment_by_day$RANDDT <- as.Date(recruitment_by_day$RANDDT, format = "%Y-%m-%d")

# Write the comparison to a CSV file
write.csv(recruitment_by_day, "bnt162b2_recruitment_by_day.csv", row.names = FALSE)

# Create a new dataframe with the data aggregated by week
recruitment_by_week <- recruitment_by_day %>%
  mutate(week = format(RANDDT, "%Y-W%W")) %>%
  group_by(week) %>%
  summarize("Process 2 Subjects" = sum(`Process 2 Subjects`),
            "Process 1 Subjects" = sum(`Process 1 Subjects`))
print(recruitment_by_week, n=100)

# Create the stacked column plot
recruitment_by_week_long <- recruitment_by_week %>%
  pivot_longer(c(`Process 2 Subjects`, `Process 1 Subjects`), 
               names_to = "Process", values_to = "Subjects")
print(recruitment_by_week_long, n=100)

ggplot(recruitment_by_week_long, aes(x = week, y = Subjects, fill = Process)) +
  geom_col(position = "stack") +
  labs(x = "Week", y = "BNT162b2 Subjects Randomized", fill = "Process") +
  ggtitle("C4591001 - BNT162b2 Subjects randomized, Trial Process 1 & Commercial Process 2") +
  theme_classic() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("Process 2 Subjects" = "#708090", "Process 1 Subjects" = "#D3D3D3"))
