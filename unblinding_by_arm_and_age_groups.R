# Loads required packages
library(readr)
library(dplyr)
library(ggplot2)

# Loads the Phase 3 population randomized
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)

# Filters out the subjects who haven't received dose 1 prior December 14, 2020
randomized_pop <- randomized_pop %>%
  filter(VAX101DT <= as.Date("2020-12-14"))

print(paste('Population pool : ', nrow(randomized_pop)))

# Converts UNBLNDDT to Date type
randomized_pop <- randomized_pop %>%
  mutate(UNBLNDDT = as.Date(UNBLNDDT))

# Calculates the cumulative percentage of subjects unblinded by date and age group
randomized_pop_cumulative <- randomized_pop %>%
  group_by(AGEGR1, UNBLNDDT) %>%
  summarise(n = n()) %>%
  mutate(cumulative_percent = cumsum(n) / sum(n) * 100) %>%
  arrange(UNBLNDDT)
randomized_pop_cumulative <- randomized_pop_cumulative %>%
  filter(UNBLNDDT >= as.Date("2020-12-13"))

print(randomized_pop_cumulative)

# Plots the chart
ggplot(randomized_pop_cumulative, aes(x = UNBLNDDT, y = cumulative_percent, color = AGEGR1)) + 
  geom_line(size = 1.5) + 
  labs(x = "Date of Unblinding", y = "Cumulative Percentage of Subjects Unblinded", color = "Age Group", 
       title = "C4591001 - Percent cumulative unblinding among phase 2-3 subjects recipient of dose 1 prior December 14, 2020") + 
  theme_classic() + 
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") + 
  scale_color_manual(values = c("#415178", "#BEBEBE")) # customize line colors


