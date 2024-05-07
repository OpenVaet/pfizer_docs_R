library(haven)
library(dplyr)

file_path <- 'xpt_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt'

# Reads the XPT file
data <- read_xpt(file_path)
print(data, n=1)

# Filters out the rows where SUBJID is in the list
excluded_subjids <- c(10561101, 11331382, 11101123, 11331405, 11491117, 12691090, 12691070, 11351357, 11341006, 10891112, 11231105, 10711213)
filtered_data <- data[!(data$SUBJID %in% excluded_subjids), ]
print(filtered_data, n=1)

# Filters out the phase 1.
filtered_data <- filtered_data[filtered_data$PHASE != "Phase 1", ]
print(filtered_data, n=1)

# Filters out the rows where AGE < 16
filtered_data <- filtered_data[as.numeric(filtered_data$AGETR01) >= 16, ]
print(filtered_data, n=1)

# Filters out the rows where RANDNO == ""
filtered_data <- filtered_data[filtered_data$RANDNO != "", ]
print(filtered_data, n=1)

# Selects the desired columns
selected_data <- filtered_data[c(
  "SUBJID", "SITEID", "RFICDT", "ARM", "PHASE",
  "AGE", "AGETR01", "RANDDT", "RANDNO", "AGEGR1",
  "AGEGR2", "AGEGR3", "UNBLNDDT", "SEX", "DTHDT",
  "VAX101DT", "VAX102DT", "VAX201DT", "VAX202DT", "RACE",
  "ETHNIC"
)]

print(selected_data)

# Writes the result to a CSV file
write.csv(selected_data, "phase_3_randomized_pop.csv", row.names = FALSE)

# Calculates the demographic statistics
total_rows <- nrow(selected_data)

# 1. AGETR01 >= 16
agetr01_ge_16 <- nrow(selected_data[selected_data$AGETR01 >= 16, ])
agetr01_ge_16_pct <- round(agetr01_ge_16 / total_rows * 100, 2)

# 1. AGETR01 >= 16 by ARM
agetr01_ge_16_placebo <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$ARM == "Placebo", ])
agetr01_ge_16_bnt162b2 <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 2. AGETR01 >= 16 && AGETR01 <= 55
agetr01_ge_16_le_55 <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 55, ])
agetr01_ge_16_le_55_pct <- round(agetr01_ge_16_le_55 / total_rows * 100, 2)

# 2. AGETR01 >= 16 && AGETR01 <= 55 by ARM
agetr01_ge_16_le_55_placebo <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 55 & selected_data$ARM == "Placebo", ])
agetr01_ge_16_le_55_bnt162b2 <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 55 & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 3. AGETR01 > 55
agetr01_gt_55 <- nrow(selected_data[selected_data$AGETR01 > 55, ])
agetr01_gt_55_pct <- round(agetr01_gt_55 / total_rows * 100, 2)

# 3. AGETR01 > 55 by ARM
agetr01_gt_55_placebo <- nrow(selected_data[selected_data$AGETR01 > 55 & selected_data$ARM == "Placebo", ])
agetr01_gt_55_bnt162b2 <- nrow(selected_data[selected_data$AGETR01 > 55 & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 4. AGETR01 >= 16 && AGETR01 <= 17
agetr01_ge_16_le_17 <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 17, ])
agetr01_ge_16_le_17_pct <- round(agetr01_ge_16_le_17 / total_rows * 100, 2)

# 4. AGETR01 >= 16 && AGETR01 <= 17 by ARM
agetr01_ge_16_le_17_placebo <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 17 & selected_data$ARM == "Placebo", ])

# 4. AGETR01 >= 16 && AGETR01 <= 17 by ARM
agetr01_ge_16_le_17_placebo <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 17 & selected_data$ARM == "Placebo", ])
agetr01_ge_16_le_17_bnt162b2 <- nrow(selected_data[selected_data$AGETR01 >= 16 & selected_data$AGETR01 <= 17 & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 5. SEX = M
sex_m <- nrow(selected_data[selected_data$SEX == "M", ])
sex_m_pct <- round(sex_m / total_rows * 100, 2)

# 5. SEX = M by ARM
sex_m_placebo <- nrow(selected_data[selected_data$SEX == "M" & selected_data$ARM == "Placebo", ])
sex_m_bnt162b2 <- nrow(selected_data[selected_data$SEX == "M" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 6. SEX = F 
sex_f <- nrow(selected_data[selected_data$SEX == "F", ])
sex_f_pct <- round(sex_f / total_rows * 100, 2)

# 6. SEX = F by ARM
sex_f_placebo <- nrow(selected_data[selected_data$SEX == "F" & selected_data$ARM == "Placebo", ])
sex_f_bnt162b2 <- nrow(selected_data[selected_data$SEX == "F" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 7. ETHNIC = "HISPANIC OR LATINO"
ethnic_hispanic <- nrow(selected_data[selected_data$ETHNIC == "HISPANIC OR LATINO", ])
ethnic_hispanic_pct <- round(ethnic_hispanic / total_rows * 100, 2)

# 7. ETHNIC = "HISPANIC OR LATINO" by ARM
ethnic_hispanic_placebo <- nrow(selected_data[selected_data$ETHNIC == "HISPANIC OR LATINO" & selected_data$ARM == "Placebo", ])
ethnic_hispanic_bnt162b2 <- nrow(selected_data[selected_data$ETHNIC == "HISPANIC OR LATINO" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 8. ETHNIC = "NOT HISPANIC OR LATINO"
ethnic_not_hispanic <- nrow(selected_data[selected_data$ETHNIC == "NOT HISPANIC OR LATINO", ])
ethnic_not_hispanic_pct <- round(ethnic_not_hispanic / total_rows * 100, 2)

# 8. ETHNIC = "NOT HISPANIC OR LATINO" by ARM
ethnic_not_hispanic_placebo <- nrow(selected_data[selected_data$ETHNIC == "NOT HISPANIC OR LATINO" & selected_data$ARM == "Placebo", ])
ethnic_not_hispanic_bnt162b2 <- nrow(selected_data[selected_data$ETHNIC == "NOT HISPANIC OR LATINO" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 9. ETHNIC = "NOT REPORTED"
ethnic_not_reported <- nrow(selected_data[selected_data$ETHNIC == "NOT REPORTED", ])
ethnic_not_reported_pct <- round(ethnic_not_reported / total_rows * 100, 2)

# 9. ETHNIC = "NOT REPORTED" by ARM
ethnic_not_reported_placebo <- nrow(selected_data[selected_data$ETHNIC == "NOT REPORTED" & selected_data$ARM == "Placebo", ])
ethnic_not_reported_bnt162b2 <- nrow(selected_data[selected_data$ETHNIC == "NOT REPORTED" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 10. RACE = "WHITE"
race_white <- nrow(selected_data[selected_data$RACE == "WHITE", ])
race_white_pct <- round(race_white / total_rows * 100, 2)

# 10. RACE = "WHITE" by ARM
race_white_placebo <- nrow(selected_data[selected_data$RACE == "WHITE" & selected_data$ARM == "Placebo", ])
race_white_bnt162b2 <- nrow(selected_data[selected_data$RACE == "WHITE" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 11. RACE = "BLACK OR AFRICAN AMERICAN"
race_black <- nrow(selected_data[selected_data$RACE == "BLACK OR AFRICAN AMERICAN", ])
race_black_pct <- round(race_black / total_rows * 100, 2)

# 11. RACE = "BLACK OR AFRICAN AMERICAN" by ARM
race_black_placebo <- nrow(selected_data[selected_data$RACE == "BLACK OR AFRICAN AMERICAN" & selected_data$ARM == "Placebo", ])
race_black_bnt162b2 <- nrow(selected_data[selected_data$RACE == "BLACK OR AFRICAN AMERICAN" & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# 12. RACE != "WHITE" && RACE != "BLACK OR AFRICAN AMERICAN"
race_other <- nrow(selected_data[!(selected_data$RACE %in% c("WHITE", "BLACK OR AFRICAN AMERICAN")), ])
race_other_pct <- round(race_other / total_rows * 100, 2)

# 12. RACE != "WHITE" && RACE != "BLACK OR AFRICAN AMERICAN" by ARM
race_other_placebo <- nrow(selected_data[!(selected_data$RACE %in% c("WHITE", "BLACK OR AFRICAN AMERICAN")) & selected_data$ARM == "Placebo", ])
race_other_bnt162b2 <- nrow(selected_data[!(selected_data$RACE %in% c("WHITE", "BLACK OR AFRICAN AMERICAN")) & selected_data$ARM == "BNT162b2 Phase 2/3 (30 mcg)", ])

# Writes the statistics to a CSV file
output_file <- "phase_3_randomized_pop_stats.csv"

stats_data <- data.frame(
  Statistic = c(
    "AGETR01 >= 16", 
    "AGETR01 >= 16 & AGETR01 <= 55",
    "AGETR01 > 55",
    "AGETR01 >= 16 & AGETR01 <= 17",
    "SEX = M",
    "SEX = F",
    "ETHNIC = 'HISPANIC OR LATINO'",
    "ETHNIC = 'NOT HISPANIC OR LATINO'", 
    "ETHNIC = 'NOT REPORTED'",
    "RACE = 'WHITE'",
    "RACE = 'BLACK OR AFRICAN AMERICAN'",
    "RACE != 'WHITE' & RACE != 'BLACK OR AFRICAN AMERICAN'"
  ),
  BNT162b2 = c(
    agetr01_ge_16_bnt162b2,
    agetr01_ge_16_le_55_bnt162b2,
    agetr01_gt_55_bnt162b2,
    agetr01_ge_16_le_17_bnt162b2,
    sex_m_bnt162b2,
    sex_f_bnt162b2,
    ethnic_hispanic_bnt162b2,
    ethnic_not_hispanic_bnt162b2,
    ethnic_not_reported_bnt162b2,
    race_white_bnt162b2,
    race_black_bnt162b2,
    race_other_bnt162b2
  ),
  Placebo = c(
    agetr01_ge_16_placebo,
    agetr01_ge_16_le_55_placebo,
    agetr01_gt_55_placebo,
    agetr01_ge_16_le_17_placebo,
    sex_m_placebo,
    sex_f_placebo,
    ethnic_hispanic_placebo,
    ethnic_not_hispanic_placebo,
    ethnic_not_reported_placebo,
    race_white_placebo,
    race_black_placebo,
    race_other_placebo
  ),
  Total = c(
    agetr01_ge_16,
    agetr01_ge_16_le_55,
    agetr01_gt_55,
    agetr01_ge_16_le_17,
    sex_m,
    sex_f,
    ethnic_hispanic,
    ethnic_not_hispanic,
    ethnic_not_reported,
    race_white,
    race_black,
    race_other
  )
)

write.csv(stats_data, output_file, row.names = FALSE)
