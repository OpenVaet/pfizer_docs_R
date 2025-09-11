# =====================================================================
# C4591001 Protocol Deviations: Identification of process 2 recipients.
# =====================================================================
# This script analyzes the process 2 recipients from two angles :
# - Process 2 16 to 55 recipients on sites 1133, 1135, 1146, and 1170 after October 19, 2020
#    * Date of October 19, 2020 & age range identified in UK MHRA FOI 23/510
#    * Lots identified in https://www.tga.gov.au/sites/default/files/2022-08/foi-3659-04.pdf
#    * Sites identified in https://phmpt.org/wp-content/uploads/2022/06/125742_S1_M5_5351_c4591001-fa-interim-patient-batches.pdf 
# =====================================================================
# This script requires that:
# 1 - download_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/download_full_prod.R
# 2 - extract_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/extract_full_prod.R
# ... have both been executed first
# ---------------------------------------------------------------------
# 1. SETUP AND CONFIGURATION
# ---------------------------------------------------------------------
# Load required libraries
library(haven)      # For reading XPT files
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling
library(furrr)      # For parallel processing
library(stringr)    # For string manipulation
library(ggplot2)    # For creating plots
library(base64enc)  # For embedding plots in HTML
library(tidyverse)  # For replace_na function on days without recruitment

# Define file paths
DATA_PATH <- "xpt_data/"

# Primary data files
FILES <- list(
  adsl = paste0(DATA_PATH, "FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt")
)

# Analysis parameters
EXCLUDED_SUBJECTS <- c(10561101, 11331382, 11101123, 11331405, 11491117,
                       12691090, 12691070, 11351357, 11341006, 10891112,
                       11231105, 10711213)
MIN_AGE <- 16

# Helpers
safe_parse_date <- function(x) {
  # Robustly parse character/numeric/labelled to Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "labelled")) x <- haven::as_factor(x)
  x <- as.character(x)
  if (length(x) == 0) return(as.Date(character()))
  suppressWarnings({
    dt <- parse_date_time(x, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", "Ymd HMS", "Ymd HM", "Ymd H", "Ymd"))
  })
  as.Date(dt)
}

# ---------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL PROCESSING
# ---------------------------------------------------------------------
cat("Loading and processing ADSL data...\n")
adsl_data <- read_xpt(FILES$adsl)
cat(sprintf("Initial ADSL records: %d\n", nrow(adsl_data)))

randomized_pop <- adsl_data %>%
  filter(!(SUBJID %in% EXCLUDED_SUBJECTS)) %>%
  filter(PHASE != "Phase 1") %>%
  filter(as.numeric(AGETR01) >= MIN_AGE) %>%
  filter(RANDNO != "") %>%
  select(SUBJID, SITEID, COUNTRY, RFICDT, ARM, PHASE,
         AGE, AGETR01, RANDDT, RANDNO, AGEGR1,
         AGEGR2, AGEGR3, UNBLNDDT, SEX, DTHDT,
         VAX101DT, VAX102DT, VAX201DT, VAX202DT, RACE, ETHNIC)

cat(sprintf("Randomized population after filtering: %d\n", nrow(randomized_pop)))

# ---------------------------------------------------------------------
# 3. IDENTIFYING SUBJECTS BY SITES, AGE & DATE.
# ---------------------------------------------------------------------
# Extracts the ORISITEID (the site which actually recruited the subject in the study - not the current trial site)
randomized_pop$ORISITEID <- as.numeric(sub("(....)....", "\\1", randomized_pop$SUBJID))

site_date_filtered_data <- randomized_pop %>%
  filter(VAX101DT >= "2020-10-19", 
         ORISITEID %in% c(1133, 1135, 1146, 1170))

# Writes site_date_filtered_data to a new CSV file
write.csv(site_date_filtered_data, "process_2_recipients_by_date_and_sites.csv", row.names = FALSE)

# Visualizes results
site_date_agegr1_counts <- site_date_filtered_data %>%
  count(ARM, AGEGR1)

# ---------------------------------------------------------------------
# 4. IDENTIFYING SUBJECTS BY RANDOMIZATION NUMBERS
# ---------------------------------------------------------------------
randomized_pop$RANDNO <- as.numeric(randomized_pop$RANDNO)
rando_filtered_data <- randomized_pop %>% filter(RANDNO >= 400000, RANDNO <= 499999)

# Write rando_filtered_data to a new CSV file
write.csv(rando_filtered_data, "process_2_recipients_by_randomization_numbers.csv", row.names = FALSE)

# Visualizes results
rando_agegr1_counts <- rando_filtered_data %>%
  count(ARM, AGEGR1)

# ---------------------------------------------------------------------
# 5. PRINTING RESULTS SUMMARIES
# ---------------------------------------------------------------------
print(site_date_agegr1_counts)
print(rando_agegr1_counts)

# ---------------------------------------------------------------------
# 6. COMPARING RESULTS
# ---------------------------------------------------------------------
# Check if both files have the same number of rows
if (nrow(rando_filtered_data) == nrow(site_date_filtered_data)) {
  print("Both files have the same number of rows.")
} else {
  print("The files have a different number of rows.")
}

# Check if both files contain the same unique SUBJID entries
unique_subjid_1 <- unique(rando_filtered_data$SUBJID)
unique_subjid_2 <- unique(site_date_filtered_data$SUBJID)
if (all(unique_subjid_1 %in% unique_subjid_2) && all(unique_subjid_2 %in% unique_subjid_1)) {
  print("Both files contain the same unique SUBJID entries.")
} else {
  print("The files do not contain the same unique SUBJID entries.")
}

# Filter data to only include ARM = BNT162b2 Phase 2/3 (30 mcg)
rando_filtered_data_filtered <- rando_filtered_data %>% filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")
randomized_pop_filtered <- randomized_pop %>% filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)")

# Count the number of subjects recruited per day for each dataset
recruitment_by_day <- data.frame(RANDDT = unique(c(as.character(rando_filtered_data_filtered$RANDDT), as.character(randomized_pop_filtered$RANDDT))))

# Count the number of subjects for each dataset
process_2_subjects <- rando_filtered_data_filtered %>%
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

# ---------------------------------------------------------------------
# 7) STACKED WEEKLY PLOT
# ---------------------------------------------------------------------
# --- Build plotting frame (stacked weekly) ---
# ---------------------------------------------------------------------
# 7) STACKED WEEKLY PLOT (house style + placebo/bnt tones)
# ---------------------------------------------------------------------

# Ensure stacking & legend order: Process 1 bottom, Process 2 top
plot_df <- recruitment_by_week_long %>%
  mutate(Process = factor(Process, levels = c("Process 1 Subjects", "Process 2 Subjects"))) %>%
  group_by(week) %>%
  arrange(Process, .by_group = TRUE) %>%
  mutate(total = sum(Subjects)) %>%
  ungroup()

# text sizes
text_scale   <- 1.6
s_base       <- 12 * text_scale
s_title      <- 16 * text_scale
s_subtitle   <- 12 * text_scale
s_axis       <- 12 * text_scale
s_axis_title <- 12 * text_scale
s_legend     <- 12 * text_scale
s_inbar_lab  <- 3.6 * text_scale
s_total_lab  <- 4.0 * text_scale
s_caption    <- 14 * text_scale 

# Placebo & BNT162b2 tones
pal <- c("Process 1 Subjects" = "#0d132d",   # Placebo tone
         "Process 2 Subjects" = "#a1082c")   # BNT162b2 tone

y_top <- max(plot_df$total, na.rm = TRUE) + 8 * text_scale

p_proc <- ggplot(plot_df, aes(x = week, y = Subjects, fill = Process)) +
  geom_col(width = 0.75) +

  # GREY labels near top of each segment
  geom_text(
    data = dplyr::filter(plot_df, Subjects >= 3),
    aes(label = format(Subjects, big.mark = ",")),
    position = position_stack(vjust = 0.96),
    color = "#D0D0D0", fontface = "bold", size = s_inbar_lab
  ) +

  # Single BLACK total above stack
  geom_text(
    data = dplyr::distinct(plot_df, week, total),
    aes(x = week, y = total, label = format(total, big.mark = ",")),
    inherit.aes = FALSE, vjust = -0.6, color = "black",
    fontface = "bold", size = s_total_lab
  ) +

  scale_fill_manual(
    values = pal,
    breaks = names(pal),                         # keep legend stable
    labels = c("Process 1 Subjects", "Process 2 Subjects"),
    name   = "Process"
  ) +
  scale_y_continuous(limits = c(0, y_top), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "C4591001 — BNT162b2 Subjects Randomized by Week - Process 1 vs Commercial Process 2",
    subtitle = "Grey labels show in-stack counts, black labels show weekly totals",
    caption  = "Figure 5",
    x = "ISO Year–Week", y = "Subjects randomized"
  ) +
  theme_minimal(base_size = s_base) +
  theme(
    plot.title       = element_text(size = s_title, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = s_subtitle, hjust = 0.5, color = "gray35",
                                    margin = margin(b = 6 * text_scale)),
    axis.text        = element_text(size = s_axis),
    axis.title       = element_text(size = s_axis_title, face = "bold"),
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5),

    legend.position  = "bottom",
    legend.title     = element_text(size = s_legend, face = "bold"),
    legend.text      = element_text(size = s_legend),
    legend.key.size  = grid::unit(12 * text_scale, "pt"),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),

    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),

    plot.margin      = margin(t = 12 * text_scale, r = 10 * text_scale,
                              b = 14 * text_scale, l = 10 * text_scale),
    plot.caption          = element_text(size = s_caption,
                                     hjust = 1, color = "black",
                                     margin = margin(t = 6 * text_scale)),
    plot.caption.position = "plot",
  ) +
  coord_cartesian(clip = "off")

print(p_proc)

# Save like the others
ggsave("process_weekly_stack.png", p_proc, width = 12, height = 6.5, dpi = 300)