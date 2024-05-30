# Extracts ZIP to XPT.
library(tools)
library(haven)
library(dplyr)

# Verifies if the requireed files have been properly retrieved.
# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "AESPID", "VPHASE", "AREL", "AESER", "AESTDTC", "AEENDTC", "AEDECOD")]

# 10551139
adae_10551139_data <- adae_selected_data %>% 
  filter(SUBJID == 10551139)
print(adae_10551139_data)

# Creates the formatted table
html_table_10551139 <- flextable(adae_10551139_data) %>%
  set_header_labels(
    "SUBJID" = "Subject ID",
    "AESPID" = "AESPID",
    "VPHASE" = "VPHASE",
    "AREL" = "Relation",
    "AESER" = "Serious",
    "AESTDTC" = "Start Date",
    "AEENDTC" = "End Date",
    "AEDECOD" = "AEDECOD"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table_10551139, path = "aes_10551139.html")

# 11401282
adae_11401282_data <- adae_selected_data %>% 
  filter(SUBJID == 11401282)
print(adae_11401282_data)

# Creates the formatted table
html_table_11401282 <- flextable(adae_11401282_data) %>%
  set_header_labels(
    "SUBJID" = "Subject ID",
    "AESPID" = "AESPID",
    "VPHASE" = "VPHASE",
    "AREL" = "Relation",
    "AESER" = "Serious",
    "AESTDTC" = "Start Date",
    "AEENDTC" = "End Date",
    "AEDECOD" = "AEDECOD"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table_11401282, path = "aes_11401282.html")

# 12411347
adae_12411347_data <- adae_selected_data %>% 
  filter(SUBJID == 12411347)
print(adae_12411347_data)

# Creates the formatted table
html_table_12411347 <- flextable(adae_12411347_data) %>%
  set_header_labels(
    "SUBJID" = "Subject ID",
    "AESPID" = "AESPID",
    "VPHASE" = "VPHASE",
    "AREL" = "Relation",
    "AESER" = "Serious",
    "AESTDTC" = "Start Date",
    "AEENDTC" = "End Date",
    "AEDECOD" = "AEDECOD"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: Sites with negative screening results")

save_as_html(html_table_12411347, path = "aes_12411347.html")

# List of specific SUBJID values
no_ae_crf_subjid_list <- c(10081603, 10371141, 10371214, 10771137, 10811102, 10851018, 10901492, 10961181, 
                 10961355, 11071196, 11101164, 11101165, 11171079, 11261244, 11281241, 11281267, 
                 11331317, 11341327, 11351257, 11471230, 11501153, 11561131, 12314335, 12321299, 
                 12411269, 12411493, 12411829, 12411930, 12412055, 12511029, 12511031, 12511033, 
                 12511072, 12511145, 44441035)

# Filter the dataframe based on SUBJID values
no_ae_crf_filtered_data <- adae_selected_data %>% 
  filter(SUBJID %in% no_ae_crf_subjid_list)

# Arrange the dataframe by SUBJID and AESPID
no_ae_crf_arranged_data <- no_ae_crf_filtered_data %>% 
  arrange(SUBJID, AESPID)

# Save the arranged dataframe to a CSV file
write.csv(no_ae_crf_arranged_data, "no_adae_in_crf.csv", row.names = FALSE)

# List of specific SUBJID values
miss_ae_crf_subjid_list <- c(10071441, 10081152, 10131229, 10131386, 10131718, 10161087, 10191021, 10191145,
                  10211081, 10211084, 10281083, 10281205, 10381050, 10441163, 10471012, 10551128,
                  10551139, 10551145, 10551182, 10791183, 10801013, 10811179, 10831050, 10831173,
                  10841219, 10841538, 10871266, 10901140, 10911213, 10911247, 10911297, 10931067,
                  10931128, 11071191, 11091036, 11091074, 11091164, 11091204, 11101236, 11311140,
                  11331537, 11401066, 11401244, 11401282, 11401285, 11411143, 11461181, 11461302,
                  11521053, 11521095, 11521316, 11561001, 11781257, 11951017, 11951023, 12261477,
                  12312660, 12315186, 12315579, 12411053, 12411117, 12411347, 12411410, 12411561,
                  12411568, 12412191, 12412218, 12511060, 12541109, 12541189, 12601018, 12601108,
                  44441422)

# Filter the dataframe based on SUBJID values
miss_ae_crf_filtered_data <- adae_selected_data %>% 
  filter(SUBJID %in% miss_ae_crf_subjid_list)

# Arrange the dataframe by SUBJID and AESPID
miss_ae_crf_arranged_data <- miss_ae_crf_filtered_data %>% 
  arrange(SUBJID, AESPID)

# Save the arranged dataframe to a CSV file
write.csv(miss_ae_crf_arranged_data, "missing_adae_in_crf.csv", row.names = FALSE)
