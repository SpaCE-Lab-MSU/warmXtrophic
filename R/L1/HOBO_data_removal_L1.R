# TITLE:          HOBO paired sensor data removal
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT:     Data imported as clean L1 files from the Google drive
# DATA OUTPUT:    Clean L1 files are exported with bad data removed
#                 Outliers were removed in the clean-up script, but here I'm removing larger chunks
#                 of data that are still bad (based on what I found in the clean-up and data_checks R script)
#                 A note is added for each data removal as to why that data is being removed
# PROJECT:        warmXtrophic
# DATE:           May 2022

# clear all existing data
rm(list=ls())

# load in the data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.csv"))

# date is a character column - convert to date format
KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d %H:%M")

# making year and month columns
KBS$year <- format(KBS$Date_Time,format="%Y")
UMBS$year <- format(UMBS$Date_Time,format="%Y")
UMBS$month <- format(UMBS$Date_Time,format="%m")

## Notes on bad data that is being removed because it is missing too much data or due to failures

# overall removal:
# - 2015 (sensors not installed until midway through the summer)
KBS <- KBS[!(KBS$year == "2015"),]
UMBS <- UMBS[!(UMBS$year == "2015"),]
# - sensor 1 KBS (failed for 2021)
KBS <- KBS[!(KBS$sensor == 1 & KBS$year =="2021"),] 

# KBS 10cm air temps:
# - sensor 1 all years
KBS$XU_warmed_air_10cm[KBS$sensor == 1] <- NA
KBS$XU_ambient_air_10cm[KBS$sensor == 1] <- NA
# - sensor 3 has some records, but none span a full year or summer, all years removed
KBS$XU_warmed_air_10cm[KBS$sensor == 3] <- NA
KBS$XU_ambient_air_10cm[KBS$sensor == 3] <- NA
# - sensor 2 2018 (there is no 10cm data for 2018 this way)
KBS$XU_ambient_air_10cm[KBS$sensor == 2 & KBS$year == "2018"] <- NA
KBS$XU_warmed_air_10cm[KBS$sensor == 2 & KBS$year == "2018"] <- NA

# KBS 5cm soil temps:
# - sensor 1 all years
KBS$XU_warmed_soil_temp_5cm[KBS$sensor == 1] <- NA
KBS$XU_ambient_soil_temp_5cm[KBS$sensor == 1] <- NA
# - sensor 3 2018
KBS$XU_ambient_soil_temp_5cm[KBS$sensor == 3 & KBS$year == "2018"] <- NA
KBS$XU_warmed_soil_temp_5cm[KBS$sensor == 3 & KBS$year == "2018"] <- NA
# - sensor 2 2018
KBS$XU_ambient_soil_temp_5cm[KBS$sensor == 2 & KBS$year == "2018"] <- NA
KBS$XU_warmed_soil_temp_5cm[KBS$sensor == 2 & KBS$year == "2018"] <- NA

# UMBS:
# - sensor 1 July-Nov 2021 (wasp nest was on sensor)
UMBS <- UMBS[!(UMBS$sensor == 1 & UMBS$year =="2021" & UMBS$month == "07"),] 
UMBS <- UMBS[!(UMBS$sensor == 1 & UMBS$year =="2021" & UMBS$month == "08"),]
UMBS <- UMBS[!(UMBS$sensor == 1 & UMBS$year =="2021" & UMBS$month == "09"),]
UMBS <- UMBS[!(UMBS$sensor == 1 & UMBS$year =="2021" & UMBS$month == "10"),]
UMBS <- UMBS[!(UMBS$sensor == 1 & UMBS$year =="2021" & UMBS$month == "11"),]

# write new data files to the google drive
write.csv(KBS, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_dataremoved_L1.csv"))
write.csv(UMBS, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_dataremoved_L1.csv"))

write.csv(KBS, "KBS_pairedsensors_dataremoved_L1.csv")
write.csv(UMBS, "UMBS_pairedsensors_dataremoved_L1.csv")

