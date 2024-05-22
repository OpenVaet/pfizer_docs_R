library(haven)
library(dplyr)
library(ggplot2)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
print(randomized_pop)

# Loads the ADSYMPT to AETERM correspondance.
adae_to_symptoms_file <- 'adae_to_covid_symptoms.csv'
adae_to_symptoms <- read.csv(adae_to_symptoms_file)
print(adae_to_symptoms)

# Loads the symptoms reported.
symptoms_file <- 'xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt'
symptoms <- read_xpt(symptoms_file)
print(colnames(symptoms))
print(symptoms)

# Filters the symptoms data to only include subjects in the randomized population
symptoms <- symptoms[symptoms$SUBJID %in% randomized_pop$SUBJID, ]

# Filters the symptoms data to only include symptoms reported, excluded "N" answers.
symptoms <- symptoms %>%
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE")
symptoms <- symptoms %>%
  filter(AVALC == "Y")
print(colnames(symptoms))
print(symptoms)

# Sustains only columns required for the current analysis.
symptoms_selected <- symptoms[c("SUBJID", "PARAM", "ADT")]
symptoms_selected <- symptoms_selected %>% 
  rename(SYMPTOM = PARAM)
symptoms_selected <- symptoms_selected %>% 
  rename(REPORTDATE = ADT)
print(symptoms_selected)

# Loads the ADAE reported.
adae_file <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
adae <- read_xpt(adae_file)
print(colnames(adae))
print(adae)

# Filters the ADAE data to only include subjects in the randomized population
adae <- adae[adae$SUBJID %in% randomized_pop$SUBJID, ]

# Mutates AEDECOD to upper cases, and adds the SYMPTOM corresponding.
adae_selected_data <- adae[c("SUBJID", "AESTDTC", "AEDECOD")]
adae_selected_data <- adae_selected_data %>%
  mutate(AEDECOD = toupper(AEDECOD))
adae_selected_merged <- adae_selected_data %>%
  left_join(adae_to_symptoms, by = c("AEDECOD" = "AEDECOD"), relationship = "many-to-many") %>%
  group_by(SUBJID, AESTDTC, AEDECOD)

# Filters out rows where SYMPTOM is NA (AE doesn't match a COVID symptom)
adae_selected_merged <- adae_selected_merged %>%
  filter(!is.na(SYMPTOM))
adae_selected_merged <- subset(adae_selected_merged, select = -AEDECOD)
adae_selected_merged <- adae_selected_merged %>% 
  rename(REPORTDATE = AESTDTC)
adae_selected_merged$REPORTDATE <- substr(adae_selected_merged$REPORTDATE, 1, 10)
adae_selected_merged$REPORTDATE <- as.Date(adae_selected_merged$REPORTDATE)
print(adae_selected_merged)

# Loads the FAcE data.
face_file <- 'xpt_data/FDA-CBER-2021-5683-0484461-0537913-125742_S1_M5_c4591001-S-D-face.xpt'
face <- read_xpt(face_file)
face <- face %>%
  filter(!is.na(FAORRES) & FAORRES != "N" & FAORRES != '')
face$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", face$USUBJID)
print(colnames(face))

# Filters & renames Face data.
face_merged <- face %>% 
  select(-STUDYID, -DOMAIN, -FASEQ, -USUBJID, -FAGRPID, -FAREFID, -FALNKID, 
         -FALNKGRP, -FATESTCD, -FATEST, -FACAT, -FASCAT, -FAORRESU, 
         -FASTRESC, -FASTRESN, -FASTRESU, -FASTAT, -FAREASND, -FALOC, 
         -FALAT, -FADRVFL, -FAEVAL, -VISITNUM, -VISIT, -EPOCH, -FADY, 
         -FATPT, -FATPTNUM, -FATPTREF, -FAEVLINT, -FAEVINTX, -FAENRTPT, 
         -FARFTDTC, -FAENTPT, -FAORRES)
face_merged$FADTC <- substr(face_merged$FADTC, 1, 10)
face_merged <- face_merged %>% 
  rename(SYMPTOM = FAOBJ)
face_merged <- face_merged %>% 
  rename(REPORTDATE = FADTC)
face_merged$REPORTDATE <- as.Date(face_merged$REPORTDATE)


print(adae_selected_merged)
print(symptoms_selected)
print(face_merged)

# Merging the tables.
merged_data <- rbind(adae_selected_merged, symptoms_selected, face_merged) %>% 
  distinct(SUBJID, SYMPTOM, REPORTDATE, .keep_all = TRUE)
merged_data %>%
  group_by(SUBJID, SYMPTOM) %>%
  arrange(REPORTDATE) %>%
  filter(lag(REPORTDATE, default = REPORTDATE[1]) + 4 <= REPORTDATE | row_number() == 1)

# Filtering data to prior cut-off.
merged_data <- merged_data[merged_data$REPORTDATE <= "2020-11-14", ]
merged_data <- merged_data %>%
  inner_join(randomized_pop %>% 
               mutate(SUBJID = as.character(SUBJID)) %>% 
               distinct(SUBJID, ARM), 
             by = "SUBJID")
print(merged_data)
merged_data_fil <- merged_data %>%
  filter(!is.na(ARM))
print(merged_data_fil)

# Writes the symptoms data merged to a CSV file
write.csv(merged_data_fil, "covid_symptoms_accross_datasets.csv", row.names = FALSE)

summary_df <- merged_data_fil %>%
  group_by(ARM, REPORTDATE) %>%
  summarise(Count = n_distinct(SUBJID), .groups = "drop") %>%
  arrange(ARM, REPORTDATE)

print(summary_df)


# Creates the line chart
ggplot(summary_df, aes(x = REPORTDATE, y = Count, color = ARM)) +
  geom_point() +
  geom_line(size = 1.4) +
  scale_color_manual(values = c("BNT162b2 Phase 2/3 (30 mcg)" = "#FF6B6B", "Placebo" = "black")) +
  labs(title = "C4591001 - Phase 2-3 Subjects Reporting COVID symptoms",
       x = "Date",
       y = "Subjects",
       color = "ARM") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) + 
  scale_x_date(date_breaks = "3 day", date_labels = "%Y-%m-%d")

# Saves the plot to a file
ggsave("subjects_with_covid_symptoms.png", width = 8, height = 6, dpi = 300)

# Writes the updated data to a CSV file
write.csv(summary_df, "subjects_with_covid_symptoms.csv", row.names = FALSE)

