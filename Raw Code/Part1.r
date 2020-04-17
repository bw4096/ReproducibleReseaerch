# Reading in activity.csv file

df <- read.csv("activity.csv")
library(tidyverse)
steps_per_day <- df %>% group_by(date) %>%
    summarize(Steps = sum(steps))
steps_per_day

# Histogram of steps per day

steps_per_day %>% ggplot(aes(Steps)) +
    geom_histogram(binwidth = 1000, col = "Black", fill = "Orange") +
    ggtitle("Histogram of Steps per Day") + ylab("Frequency") +
    theme(plot.title = element_text(hjust = 0.5))

# Mean and median calculation

mean_steps_per_day <- steps_per_day %>% summarize(mean = mean(Steps, na.rm = TRUE))
mean_steps_per_day

median_steps_per_day <- steps_per_day %>% summarize(median = median(Steps,na.rm = TRUE))
median_steps_per_day
