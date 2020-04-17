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

# Average steps per time of day
avg <- df %>% group_by(interval) %>% summarize(Mean = mean(steps, na.rm = TRUE)) 
avg %>% ggplot(aes(interval, Mean)) +
    geom_line(col = "Purple") +
    xlab("Time of Day (24hr time)") +
    ylab("Average Steps") +
    ggtitle("Average Number of Steps per Time of Day") +
    theme(plot.title = element_text(hjust = 0.5))

# Interal with maximum avg steps
avg$interval[which(avg$Mean == max(avg$Mean))]

# Number of NAs and imputing missing values

sum(is.na(df$steps))

# Replacing NAs with 5 minute interval average
df1 <- transform(df, steps = ifelse(is.na(steps), avg$Mean,steps))

steps_per_day <- df1 %>% group_by(date) %>%
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

# Panel plot 
#changing date to Date class

df1$date <- as.Date(df1$date)

df1$day <- ifelse(weekdays(df1$date) == "Saturday" | weekdays(df1$date) == "Sunday", "weekend", "weekday")

avg1 <- df1 %>% group_by(interval, day) %>% summarize(Mean = mean(steps, na.rm = TRUE)) 
avg1 %>% ggplot(aes(interval, Mean)) +
    geom_line(col = "Purple") +
    facet_wrap(vars(day),nrow = 2) +
    xlab("Time of Day (24hr time)") +
    ylab("Average Steps") +
    ggtitle("Average Number of Steps per Time of Day") +
    theme(plot.title = element_text(hjust = 0.5))