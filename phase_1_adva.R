# Loads necessary packages
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)

# Reads the phase 1 subjects data to get the list of subject IDs
phase_1_subjects <- read.csv("phase_1_subjects_adsl_data.csv")
phase_1_subjids <- phase_1_subjects$SUBJID
print(phase_1_subjects)

# Reads the new XPT file
adva_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.xpt')

# Removes PARAM = "N-binding antibody - N-binding Antibody Assay" from adva_data
adva_data <- adva_data %>%
  filter(PARAM != "N-binding antibody - N-binding Antibody Assay")
print(adva_data)

# Filters the ADVA data to include only subjects present in the phase 1 subjects data
adva_data_filtered <- adva_data %>% 
  filter(SUBJID %in% phase_1_subjids)

# Sustains only the relevant columns.
adva_data_selected <- adva_data_filtered[c("SUBJID", "ARM", "VISIT", "AVISIT", "ISDTC", "PARAM", "AVALC")]
print(adva_data_selected)

# Counts the total of unique SUBJID for each VISIT
visit_counts <- adva_data_selected %>%
  group_by(ARM, VISIT) %>%
  summarise(Unique_Subjects = n_distinct(SUBJID))

# Prints the visit counts to the console
print(visit_counts, n=200)

# Writes the visit counts to a CSV file
write.csv(visit_counts, "phase_1_visits_adva.csv", row.names = FALSE)

# Detects and print the "SUBJID - VISIT - ISDTC - PARAM" sequences with different results "AVALC"
different_results <- adva_data_selected %>%
  group_by(SUBJID, ARM, VISIT, ISDTC, PARAM) %>%
  filter(n_distinct(AVALC) > 1) %>%
  summarise(
    Result_A = first(AVALC[AVISIT != ""]),
    Result_B = first(AVALC[AVISIT == ""]),
    `Difference A - B` = as.numeric(first(AVALC[AVISIT != ""])) - as.numeric(first(AVALC[AVISIT == ""])),
    .groups = 'drop'
  )

# Checks if there are any different results and write them to a CSV file
if (nrow(different_results) > 0) {
  print('different_results : ')
  print(different_results)
  write.csv(different_results, "phase_1_different_adva_results.csv", row.names = FALSE)
} else {
  print("No different results found for the same 'SUBJID - VISIT - ISDTC - PARAM' sequence.")
}

# Filters data frame on tested arm & param.
unique_adva_data_bntb2_50 <- adva_data_selected %>%
  filter(PARAM == "SARS-CoV-2 serum neutralizing titer 50 (titer) - Virus Neutralization Assay",
         ARM  == "BNT162b2 Phase 1 (30 mcg)")

print(unique_adva_data_bntb2_50, n=100)

# Computes the average of all AVALC results when AVISIT != ""
avg_avisit_not_empty <- unique_adva_data_bntb2_50 %>%
  group_by(ARM, VISIT, PARAM) %>%
  summarize(avg_avalc_avisit_not_empty = mean(as.numeric(AVALC[AVISIT != ""])), .groups = 'drop')

print(avg_avisit_not_empty)

# Computes the average of all AVALC results when AVISIT == "" or AVISIT != "" if AVISIT == "" is not available
avg_avisit_empty_or_not_empty <- unique_adva_data_bntb2_50 %>%
  group_by(ARM, VISIT, PARAM) %>%
  summarize(
    avg_avalc_avisit_empty_or_not_empty = 
      ifelse(any(AVISIT == ""), 
             mean(as.numeric(AVALC[AVISIT == ""])),
             mean(as.numeric(AVALC[AVISIT != ""])))
    , .groups = 'drop')

print(avg_avisit_empty_or_not_empty)

# Merges the two data frames
comparison_results <- left_join(avg_avisit_not_empty, avg_avisit_empty_or_not_empty, by = c("ARM", "VISIT", "PARAM"))

# Prints the results
print(comparison_results)

# Renames the columns
comparison_results <- comparison_results %>%
  rename("Official measures" = avg_avalc_avisit_not_empty,
         "First measures" = avg_avalc_avisit_empty_or_not_empty)
print(comparison_results)
write.csv(comparison_results, "phase_1_comparison_avisit_by_visit.csv", row.names = FALSE)

# Reshapes the data into long format
comparison_results_long <- comparison_results %>%
  gather(key = "Measure", value = "Value", -VISIT, -PARAM, -ARM)
print(comparison_results_long)

# Creates the line chart
ggplot(comparison_results_long, aes(x = VISIT, y = Value, color = Measure, label = round(Value, 2))) +
  geom_point(aes(color = Measure)) +
  stat_summary(aes(group = Measure, linetype = Measure), 
               fun = "mean", 
               geom = "line",
               size = 1.5) +
  geom_text(data = comparison_results_long[comparison_results_long$Value %in% unique(comparison_results_long$Value), ],
            aes(y = Value * 1.02), # Adjust the vertical position
            vjust = 0, size = 6.5, color = "black") +
  scale_linetype_manual(values = c("Official measures" = "dashed", "First measures" = "solid")) +
  scale_color_manual(values = c("Official measures" = "#FF6B6B", "First measures" = "#4CAF50")) +
  labs(title = "C4591001 - Phase 1 - SARS-CoV-2 serum neutralizing titer 50% by Visit",
       x = "Visit",
       y = "Titer",
       color = "Measure") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

# Saves the plot to a file
ggsave("phase_1_comparison_avisit_by_visit.png", width = 8, height = 6, dpi = 300)

# Writes the updated data to a CSV file
write.csv(comparison_results_long, "phase_1_comparison_avisit_by_visit.csv", row.names = FALSE)

