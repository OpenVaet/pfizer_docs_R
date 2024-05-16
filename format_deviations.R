library(haven)
library(dplyr)
library(lubridate)
library(stats)
library(stringr)
library(flextable)
library(ggplot2)

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

# Filters the filtered_data to only include subjects in the randomized population
filtered_data <- filtered_data[filtered_data$SUBJID %in% randomized_pop$SUBJID, ]

print(filtered_data)

# Calculates the total of SUBJID in randomized_pop for each ARM
arm_counts <- randomized_pop %>%
  group_by(ARM) %>%
  summarize(total_subjects = n_distinct(SUBJID))

# Counts the unique SUBJID for each CONCATTERM and ARM
deviation_counts <- list()
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

# Performs chi-square tests for each CONCATTERM
for (i in 1:nrow(deviation_counts)) {
  bnt_subjects <- deviation_counts$BNT_SUBJECTS[i]
  placebo_subjects <- deviation_counts$PLACEBO_SUBJECTS[i]
  
  bnt_other <- arm_counts$total_subjects[arm_counts$ARM == "BNT162b2 Phase 2/3 (30 mcg)"] - bnt_subjects
  placebo_other <- arm_counts$total_subjects[arm_counts$ARM == "Placebo"] - placebo_subjects
  
  contingency_table <- matrix(c(bnt_subjects, bnt_other, placebo_subjects, placebo_other), nrow = 2, ncol = 2)
  
  chisq_result <- chisq.test(contingency_table)
  
  deviation_counts$chi_square[i] <- chisq_result$statistic
  deviation_counts$p_value[i] <- chisq_result$p.value
}
print(deviation_counts, n=120)
deviation_counts <- deviation_counts %>%
  filter(p_value <= 0.05)

deviation_counts <- deviation_counts %>% 
  mutate(p_value = case_when(
    p_value < 0.000001 ~ "<0.000001",
    p_value < 0.00001 ~ "<0.00001",
    p_value < 0.0001 ~ "<0.0001",
    p_value < 0.001 ~ "<0.001",
    p_value < 0.01 ~ "<0.01",
    TRUE ~ "<0.1"
  ))

# Writes the result to a CSV file
write.csv(deviation_counts, "deviations_statistics.csv", row.names = FALSE)

print(arm_counts)
print(deviation_counts, n=120)
deviation_counts <- deviation_counts %>% 
  select(-TOTAL_SUBJECTS, -chi_square)
print(deviation_counts, n=120)

# Converts CONCATTERM to a standard encoding
deviation_counts$CONCATTERM <- iconv(deviation_counts$CONCATTERM, from = "UTF-8", to = "ASCII", sub = "")

# Removes en dash characters from CONCATTERM
deviation_counts <- deviation_counts %>% 
  mutate(CONCATTERM = gsub("–", "-", CONCATTERM))

# Creates the formatted table
html_table <- flextable(deviation_counts) %>%
  set_header_labels(
    "CONCATTERM" = "Deviation",
    "BNT_SUBJECTS" = "BNT162b2",
    "PLACEBO_SUBJECTS" = "Placebo",
    "p_value" = "p-value"
  ) %>%
  align(align = "center", part = "all") %>%
  theme_zebra() %>%
  fontsize(size = 16, part = "all") %>%
  padding(padding = 3) %>%
  autofit() %>%
  set_caption("Table 2: Deviations significantly disbalanced")

# Saves the HTML table to a file
save_as_html(html_table, path = "imbalanced_deviations.html")

# Filters on the significantly imbalanced deviations.
filtered_data %>% 
  distinct(CONCATTERM)
imbalanced_deviations <- filtered_data %>%
  filter(CONCATTERM %in% c(
    "Assessment of acute reaction for protocol specified timeframe after study intervention administration not performed at the vaccination visits. ",
    "Nasal swab not collected by site staff prior to vaccination. ",
    "Nasal swab not collected for the visit where it is required ",
    "Procedure/Test not performed per protocol ",
    "Urine pregnancy test not performed. "
  ))
print(imbalanced_deviations)
print(unique(imbalanced_deviations$CONCATTERM))

imbalanced_deviations_by_arms <- imbalanced_deviations %>%
  group_by(SITEID, ARM, CONCATTERM) %>%
  summarise(
    total_devs = n(),
    .groups = "drop"
  )
print(imbalanced_deviations_by_arms)

# Filter imbalanced_deviations_by_arms to only include SITEIDs with at least 20 dev
filtered_imbalanced_deviations_by_arms <- imbalanced_deviations_by_arms %>%
  group_by(SITEID, CONCATTERM) %>%
  filter(sum(total_devs) >= 20)
print(filtered_imbalanced_deviations_by_arms, n = 100)

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)

randomized_pop_by_arms <- randomized_pop %>%
  group_by(SITEID, ARM) %>%
  summarise(
    total_devs = n(),
    .groups = "drop"
  )

# Filters randomized_pop_by_arms to only include SITEIDs also in filtered_imbalanced_deviations_by_arms
randomized_pop_by_arms <- randomized_pop_by_arms %>%
  filter(SITEID %in% filtered_imbalanced_deviations_by_arms$SITEID)
print(randomized_pop_by_arms, n = 100)

# Initializes an empty dataframe to store the results
deviations_significant_results <- data.frame(
  SITEID = character(),
  CONCATTERM = character(),
  bnt162b2_deviations = numeric(),
  bnt162b2_no_deviations = numeric(),
  placebo_deviations = numeric(),
  placebo_no_deviations = numeric(),
  fisher_exact_pvalue = numeric()
)

# Loops through each SITEID
for (site_id in unique(filtered_imbalanced_deviations_by_arms$SITEID)) {
  for (dev_term in unique(filtered_imbalanced_deviations_by_arms$CONCATTERM)) {
    # Filters data for current SITEID
    site_data_imbalanced <- filtered_imbalanced_deviations_by_arms %>% 
      filter(SITEID == site_id, CONCATTERM == dev_term)
    site_data_randomized <- randomized_pop_by_arms %>% 
      filter(SITEID == site_id)
    
    # Retrieves deviations and totals for current SITEID
    bnt162b2_deviations <- sum(site_data_imbalanced %>% 
                                 filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)") %>% 
                                 pull(total_devs))
    placebo_deviations <- sum(site_data_imbalanced %>% 
                                filter(ARM == "Placebo") %>% 
                                pull(total_devs))
    bnt162b2_total <- sum(site_data_randomized %>% 
                            filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)") %>% 
                            pull(total_devs))
    placebo_total <- sum(site_data_randomized %>% 
                           filter(ARM == "Placebo") %>% 
                           pull(total_devs))
    
    bnt162b2_no_deviations <- bnt162b2_total - bnt162b2_deviations
    placebo_no_deviations <- placebo_total - placebo_deviations
    
    print(paste('Deviations BNT162b2 : ', bnt162b2_deviations, '/', bnt162b2_total))
    print(paste('Deviations Placebo : ', placebo_deviations, '/', placebo_total))
    
    # Creates a contingency table
    contingency_table <- matrix(c(bnt162b2_deviations, bnt162b2_no_deviations, placebo_deviations, placebo_no_deviations), nrow = 2, ncol = 2)
    colnames(contingency_table) <- c("Deviation", "No deviation")
    rownames(contingency_table) <- c("BNT", "Placebo")
    
    # Performs the Fisher's exact test
    fisher_exact_test <- fisher.test(contingency_table)
    
    print(paste('Site : ', site_id))
    print(contingency_table)
    print(fisher_exact_test)
    
    # Adds the results to the dataframe
    if (fisher_exact_test$p.value <= 0.05) {
      deviations_significant_results <- rbind(deviations_significant_results, data.frame(
        SITEID = site_id,
        CONCATTERM = dev_term,
        bnt162b2_deviations = bnt162b2_deviations,
        bnt162b2_no_deviations = bnt162b2_no_deviations,
        placebo_deviations = placebo_deviations,
        placebo_no_deviations = placebo_no_deviations,
        fisher_exact_pvalue = fisher_exact_test$p.value
      ))
    }
  }
}

deviations_significant_results <- deviations_significant_results %>% 
  mutate(fisher_exact_pvalue = case_when(
    fisher_exact_pvalue < 0.000001 ~ "<0.000001",
    fisher_exact_pvalue < 0.00001 ~ "<0.00001",
    fisher_exact_pvalue < 0.0001 ~ "<0.0001",
    fisher_exact_pvalue < 0.001 ~ "<0.001",
    fisher_exact_pvalue < 0.01 ~ "<0.01",
    TRUE ~ "<0.1"
  ))

# Creates the formatted table
html_table <- flextable(deviations_significant_results) %>%
  set_header_labels(
    "SITEID" = "Site Id",
    "CONCATTERM" = "Deviation",
    "bnt162b2_deviations" = "BNT162b2 Deviations",
    "bnt162b2_no_deviations" = "BNT162b2 No Deviations",
    "placebo_deviations" = "Placebo Deviations",
    "placebo_no_deviations" = "Placebo No Deviations",
    "fisher_exact_pvalue" = "p-value"
  ) %>%
  align(align = "center", part = "all") %>%
  theme_zebra() %>%
  fontsize(size = 16, part = "all") %>%
  padding(padding = 3) %>%
  autofit() %>%
  set_caption("Table 2: Deviations significantly disbalanced")

# Saves the HTML table to a file
save_as_html(html_table, path = "imbalanced_deviations_by_sites.html")

print(deviations_significant_results)

# Represents the site-staff related deviations weekly occurrences.
print(imbalanced_deviations)

# Adds YEARWEEK column
imbalanced_deviations <- imbalanced_deviations %>%
  mutate(YEARWEEK = paste0(year(ymd(DVSTDTC)), "-", sprintf("%02d", week(ymd(DVSTDTC)))))

# Creates a dataframe with the total rows for each YEARWEEK
imbalanced_yearweek_summary <- imbalanced_deviations %>%
  group_by(YEARWEEK) %>%
  summarize(total_deviations = n())

# Plots the deviations by YEARWEEK as a column chart
ggplot(imbalanced_yearweek_summary, aes(x = YEARWEEK, y = total_deviations)) +
  geom_col(fill = "#D3D3D3") +
  labs(title = "C4591001 - Total \"Staff-Related\" Imbalanced Deviations by Year-Week",
       x = "Year-Week",
       y = "Total Deviations") +
  theme_minimal() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Represents the "other nonstudy coronavirus vaccine"
other_vaccine_deviations <- filtered_data %>%
  filter(CONCATTERM == "Receipt of any other nonstudy coronavirus vaccine at any time prior to or during the study. ")
print(other_vaccine_deviations)

# Adds YEARWEEK column
other_vaccine_deviations <- other_vaccine_deviations %>%
  mutate(YEARWEEK = paste0(year(ymd(DVSTDTC)), "-", sprintf("%02d", week(ymd(DVSTDTC)))))

# Creates a dataframe with the total rows for each YEARWEEK
yearweek_summary <- other_vaccine_deviations %>%
  group_by(YEARWEEK) %>%
  summarize(total_deviations = n())

# Plots the deviations by YEARWEEK as a column chart
ggplot(yearweek_summary, aes(x = YEARWEEK, y = total_deviations)) +
  geom_col() +
  labs(title = "C4591001 - Total \"Other Vaccine\" Deviations by Year-Week",
       x = "Year-Week",
       y = "Total Deviations") +
  theme_minimal() +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
