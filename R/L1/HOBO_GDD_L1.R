# TITLE: warmXtrophic: OTC GDD calculations
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the L1 hobo folder in the shared Google drive
# DATA OUTPUT: Dataframe with a column for cumulative GDD and average temps for each site + year
# PROJECT: warmXtrophic
# DATE: Nov 2022

# Clear all existing data
rm(list=ls())

# read in packages
library(tidyverse)

# read in HOBO data
L1_dir<-Sys.getenv("L1DIR")
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_dataremoved_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_dataremoved_L1.csv"))

# date is a character column - convert to date format
KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d %H:%M")

# add in columns with date information
KBS_dates <- KBS
KBS_dates$month <- format(KBS_dates$Date_Time,format="%m")
KBS_dates$day <- format(KBS_dates$Date_Time,format="%d")
KBS_dates$year <- format(KBS_dates$Date_Time,format="%Y")
KBS_dates$hour <- format(KBS_dates$Date_Time, format="%H")
KBS_dates$julian <- format(KBS_dates$Date_Time, "%j")

# remove all columns except 1m air temps + date info
KBS_dates <-dplyr::select(KBS_dates, -c('X', 'X.1',
                                        'XH_warmed_soil_moisture_5cm','XH_ambient_soil_moisture_5cm',
                                        'XH_warmed_RH_1m','XH_ambient_RH_1m',
                                        'XU_warmed_air_10cm','XU_ambient_air_10cm',
                                        'XU_warmed_soil_temp_5cm','XU_ambient_soil_temp_5cm',
                                        'site'))

# take calculations for each year+treatment
KBS_GDD_calc <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -day, -hour, -julian, -Date_Time, -sensor) %>%
        group_by(year, julian, treatment) %>%
        summarize(max_temp = max(temp, na.rm = TRUE),
                  min_temp = min(temp, na.rm=TRUE),
                  mean_GDD_temp = (max_temp+min_temp)/2, # using these guidelines: https://www.canr.msu.edu/uploads/files/Research_Center/NW_Mich_Hort/General/CalculatingGrowingDegreeDays.pdf
                  GDD_base_10 = mean_GDD_temp-10) %>% # using a base temp of 10C, commonly used for plants
        na.omit()

# setting all negative GDD to 0
KBS_GDD_calc$GDD_base_10 <- ifelse(KBS_GDD_calc$GDD_base_10 < 0, 0, KBS_GDD_calc$GDD_base_10)

# column for cumulative GDD per year
KBS_GDD <- KBS_GDD_calc %>%
        group_by(year, treatment) %>%
        mutate(GDD_cumulative = cumsum(GDD_base_10))


# new dataframe for temp mean, median, and max for each year + treatment
KBS_avg <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -day, -hour, -julian, -Date_Time, -sensor) %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
