# TITLE: HOBO paired sensor data analysis
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the HOBO_data folder in the shared Google drive
# DATA OUTPUT: TBD
# PROJECT: warmXtrophic
# DATE: July 2020

# clear all existing data
rm(list=ls())

# load in packages and set working directory
for (package in c("tidyverse", "plotrix")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# load in the data
KBS_1 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair1_L1.csv")
KBS_2 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair2_L1.csv")
KBS_3 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair3_L1.csv")

# merge the data
KBS <- rbind(KBS_1, KBS_2, KBS_3)

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
KBS_season <- KBS
KBS_season$month <- format(KBS_season$Date_Time,format="%m")
KBS_season$year <- format(KBS_season$Date_Time,format="%y")
KBS_season$hour <- format(KBS_season$Date_Time, format="%H")

KBS_season <- KBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# average warmed and ambient air temps for each year + standard error
KBS_avg <- KBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

ggplot(KBS_avg, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  ylim(0, 30) +
  scale_fill_manual(values=c('grey90','grey60'))+
  theme_minimal() +
  labs(x="Year", y = "Average Temperature (°C)")



