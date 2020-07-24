# TITLE: HOBO paired sensor data analysis
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the HOBO_data folder in the shared Google drive
# DATA OUTPUT: HOBO data plots - shown in the HOBO_plot.pdf on Github
# PROJECT: warmXtrophic
# DATE: July 2020

# clear all existing data
rm(list=ls())

# load in packages and set working directory
for (package in c("tidyverse", "plotrix", "ggpubr")) {
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

UMBS_1 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair1_L1.csv")
UMBS_2 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair2_L1.csv")
UMBS_3 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair3_L1.csv")

#### KBS ####
## air temperature ##
# merge the data
KBS <- rbind(KBS_1, KBS_2, KBS_3)

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
KBS_season <- KBS
KBS_season$month <- format(KBS_season$Date_Time,format="%m")
KBS_season$year <- format(KBS_season$Date_Time,format="%Y")
KBS_season$hour <- format(KBS_season$Date_Time, format="%H")

KBS_season <- KBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
KBS_avg_month <- KBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

KBS_avg_year <- KBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

# plot the data - k_month shows the individual monthly averages over time, while k_year shows overall averages per year
k_month <- ggplot(KBS_avg_month, aes(x = month, y = average_temp, fill = treatment)) + 
  facet_grid(.~year) +
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(x = NULL, y = NULL, fill = "Treatment") +
  scale_x_discrete(labels = rep("", 5), breaks = 1:5)

k_year <- ggplot(KBS_avg_year, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "KBS", x=NULL, y = NULL, fill = "Treatment")


#### UMBS ####
## air temperature ##
# merge the data
UMBS <- rbind(UMBS_1, UMBS_2, UMBS_3)

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
UMBS_season <- UMBS
UMBS_season$month <- format(UMBS_season$Date_Time,format="%m")
UMBS_season$year <- format(UMBS_season$Date_Time,format="%Y")
UMBS_season$hour <- format(UMBS_season$Date_Time, format="%H")

UMBS_season <- UMBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
UMBS_avg_month <- UMBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

UMBS_avg_year <- UMBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

# plot the data - u_month shows the individual monthly averages over time, while u_year shows overall averages per year
u_month <- ggplot(UMBS_avg_month, aes(x = month, y = average_temp, fill = treatment)) + 
  facet_grid(.~year) +
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(x="Month", y = NULL, fill = "Treatment")

u_year <- ggplot(UMBS_avg_year, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "UMBS", x="Year", y = NULL, fill = "Treatment")


# arrange the data to show both KBS and UMBS plots on the same figure
final_year <- ggarrange(k_year, u_year, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_year,
                left = text_grob("Average Temperature (°C)", color = "black", rot = 90))

final_month <- ggarrange(k_month, u_month, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_month,
                left = text_grob("Average Temperature (°C)", color = "black", rot = 90),
                top = text_grob("Year"))








## soil temperatures ##
# can't do this until KBS_1 has values fixed for the XU_warmed_soil_temp column — many values are >100
# create a new data frame
KBS_season_soil <- KBS
KBS_season_soil$month <- format(KBS_season_soil$Date_Time,format="%m")
KBS_season_soil$year <- format(KBS_season_soil$Date_Time,format="%y")
KBS_season_soil$hour <- format(KBS_season_soil$Date_Time, format="%H")

KBS_season_soil <- KBS_season_soil %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm)

# average warmed and ambient air temps for each year + standard error
KBS_avg_soil <- KBS_season_soil %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

ggplot(KBS_avg_soil, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('grey90','grey60'))+
  theme_minimal() +
  labs(title = "KBS", x="Year", y = "Average Soil Temperature (°C)", fill = "Treatment") +
  theme(legend.position = "none")

## soil moisture ##
# can't do this until 2015 warmed and ambient soil moisture is fixed
# create a new data frame
KBS_season_moist <- KBS
KBS_season_moist$month <- format(KBS_season_moist$Date_Time,format="%m")
KBS_season_moist$year <- format(KBS_season_moist$Date_Time,format="%y")
KBS_season_moist$hour <- format(KBS_season_moist$Date_Time, format="%H")

KBS_season_moist <- KBS_season_moist %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_soil_moisture_5cm, XH_ambient_soil_moisture_5cm)

# average warmed and ambient air temps for each year + standard error
KBS_avg_moist <- KBS_season_moist %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

ggplot(KBS_avg_moist, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('grey90','grey60'))+
  theme_minimal() +
  labs(title = "KBS", x="Year", y = "Average Soil Moisture", fill = "Treatment") +
  theme(legend.position = "none")

