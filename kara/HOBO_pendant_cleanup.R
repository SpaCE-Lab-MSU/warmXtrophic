# TITLE: HOBO pendant data cleanup
# AUTHORS: Nina Lany (original), Kara Dobson (edited June 2020)
# COLLABORATORS: Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young, Kathryn Schmidt
# DATA INPUT: Data imported as csv files from shared Google drive
# DATA OUTPUT: Makes the following datasets:
    ## pendk:        KBS HOBO pendant data from 2015-2020
    ## pendu:        UMBS HOBO pendant data from 2015-2020
    ## pend1520:     Combined HOBO pendant data from KBS and UMBS from 2015-2020
# PROJECT: warmXtrophic
# DATE: 2018
    ## KD edit June 2020: Updated script to insert 2019 and 2020 data


# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Load packages
for (package in c("tidyverse", "weathermetrics")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}
#************************
####** DATA IMPORT **####
#************************

### ***KBS*** ###
# Read in KBS HOBO data from all years
pend4P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_4P_09012017.csv", skip=1, header =T)
pend5P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_5P_09012017.csv", skip=1, header =T)
pend6P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_6P_09012017.csv", skip=1, header =T)
pend7P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_7P_09012017.csv", skip=1, header =T)
pend8P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_8P_09012017.csv", skip=1, header =T)
pend9P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_9P_09012017.csv", skip=1, header =T)
pend10P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_10P_09012017.csv", skip=1, header =T)
pend11P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_11P_09012017.csv", skip=1, header =T)
pend12P_17k<-read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_12P_09012017.csv", skip=1, header =T)

pend4P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_4P_09202018.csv", skip=1, header =T)[ ,1:4]
pend5P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_5P_09202018.csv", skip=1, header =T)[ ,1:4]
pend6P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_6P_09202018.csv", skip=1, header =T)[ ,1:4]
pend7P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_7P_09202018.csv", skip=1, header =T)[ ,1:4]
pend8P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_8P_09202018.csv", skip=1, header =T)[ ,1:4]
pend10P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_10P_09202018.csv", skip=1, header =T)[ ,1:4]
pend11P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_11P_09202018.csv", skip=1, header =T)[ ,1:4]
pend12P_18k<-read.csv("L0/KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_12P_09202018.csv", skip=1, header =T)[ ,1:4]

pend4P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_4P_09232019.csv", skip=1, header =T)[ ,1:4]
pend5P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_5P_09232019.csv", skip=1, header =T)[ ,1:4]
pend6P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_6P_09232019.csv", skip=1, header =T)[ ,1:4]
pend7P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBs_7P_09232019.csv", skip=1, header =T)[ ,1:4]
pend8P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_8P_09232019.csv", skip=1, header =T)[ ,1:4]
pend10P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_10P_09232019.csv", skip=1, header =T)[ ,1:4]
pend11P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_11P_09232019.csv", skip=1, header =T)[ ,1:4]
pend12P_19k<-read.csv("L0/KBS/sensor_data/2019/09_23_2019/KBS_12P_09232019.csv", skip=1, header =T)[ ,1:4]

pend4P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_4P_04052020.csv", skip=1, header=T)[ ,1:4]
pend5P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_5P_04052020.csv", skip=1, header=T)[ ,1:4]
pend6P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_6P_04052020.csv", skip=1, header=T)[ ,1:4]
pend7P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_7P_04052020.csv", skip=1, header=T)[ ,1:4]
pend8P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_8P_04052020.csv", skip=1, header=T)[ ,1:4]
pend10P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_10P_04052020.csv", skip=1, header=T)[ ,1:4]
pend11P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_11P_04052020.csv", skip=1, header=T)[ ,1:4]
pend12P_20k<-read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_12P_04052020.csv", skip=1, header=T)[ ,1:4]


# Change class of Date.Time column from factor to POSIXct date 
pend4P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend4P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend5P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend6P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend7P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend8P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend9P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend9P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend10P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend11P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend12P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

pend4P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_18k$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_18k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

pend4P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_19k$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_19k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

pend4P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_20k$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_20k$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

# Rename columns and remove unnecessary rows.
pend4P_17k$X.<-NULL
names(pend4P_17k)[names(pend4P_17k)=="Temp...F..LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_17k)[names(pend4P_17k)=="Intensity..lum.ft...LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_17k)[names(pend4P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend5P_17k$X.<-NULL
names(pend5P_17k)[names(pend5P_17k)=="Temp...F..LGR.S.N..10747440..SEN.S.N..10747440..LBL..5P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_17k)[names(pend5P_17k)=="Intensity..lum.ft...LGR.S.N..10747440..SEN.S.N..10747440..LBL..5P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_17k)[names(pend5P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend6P_17k$X.<-NULL
names(pend6P_17k)[names(pend6P_17k)=="Temp...F..LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_17k)[names(pend6P_17k)=="Intensity..lum.ft...LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_17k)[names(pend6P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend7P_17k$X.<-NULL
names(pend7P_17k)[names(pend7P_17k)=="Temp...F..LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_17k)[names(pend7P_17k)=="Intensity..lum.ft...LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_17k)[names(pend7P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend8P_17k$X.<-NULL
names(pend8P_17k)[names(pend8P_17k)=="Temp...F..LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_17k)[names(pend8P_17k)=="Intensity..lum.ft...LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_17k)[names(pend8P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend9P_17k$X.<-NULL
names(pend9P_17k)[names(pend9P_17k)=="Temp...F..LGR.S.N..10747448..SEN.S.N..10747448..LBL..9P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend9P_17k)[names(pend9P_17k)=="Intensity..lum.ft...LGR.S.N..10747448..SEN.S.N..10747448..LBL..9P_warmed_light_1.."] <- "Intensity_lum_ft_XP_light_1m"
names(pend9P_17k)[names(pend9P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend10P_17k$X.<-NULL
names(pend10P_17k)[names(pend10P_17k)=="Temp...F..LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_17k)[names(pend10P_17k)=="Intensity..lum.ft...LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_17k)[names(pend10P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend11P_17k$X.<-NULL
names(pend11P_17k)[names(pend11P_17k)=="Temp...F..LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_17k)[names(pend11P_17k)=="Intensity..lum.ft...LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_17k)[names(pend11P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"
pend12P_17k$X.<-NULL
names(pend12P_17k)[names(pend12P_17k)=="Temp...F..LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_17k)[names(pend12P_17k)=="Intensity..lum.ft...LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_17k)[names(pend12P_17k)=="Date.Time..GMT.05.00"] <- "Date_Time"

pend4P_18k$X.<-NULL
names(pend4P_18k)[names(pend4P_18k)=="Temp...F..LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_18k)[names(pend4P_18k)=="Intensity..lum.ft...LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_18k)[names(pend4P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_18k$X.<-NULL
names(pend5P_18k)[names(pend5P_18k)=="Temp...F..LGR.S.N..10747440..SEN.S.N..10747440..LBL..5P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_18k)[names(pend5P_18k)=="Intensity..lum.ft...LGR.S.N..10747440..SEN.S.N..10747440..LBL..5P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_18k)[names(pend5P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_18k$X.<-NULL
names(pend6P_18k)[names(pend6P_18k)=="Temp...F..LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_18k)[names(pend6P_18k)=="Intensity..lum.ft...LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_18k)[names(pend6P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_18k$X.<-NULL
names(pend7P_18k)[names(pend7P_18k)=="Temp...F..LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_18k)[names(pend7P_18k)=="Intensity..lum.ft...LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_18k)[names(pend7P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_18k$X.<-NULL
names(pend8P_18k)[names(pend8P_18k)=="Temp...F..LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_18k)[names(pend8P_18k)=="Intensity..lum.ft...LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_18k)[names(pend8P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_18k$X.<-NULL
names(pend10P_18k)[names(pend10P_18k)=="Temp...F..LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_18k)[names(pend10P_18k)=="Intensity..lum.ft...LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_18k)[names(pend10P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_18k$X.<-NULL
names(pend11P_18k)[names(pend11P_18k)=="Temp...F..LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_18k)[names(pend11P_18k)=="Intensity..lum.ft...LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_18k)[names(pend11P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_18k$X.<-NULL
names(pend12P_18k)[names(pend12P_18k)=="Temp...F..LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_18k)[names(pend12P_18k)=="Intensity..lum.ft...LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_18k)[names(pend12P_18k)=="Date.Time..GMT.04.00"] <- "Date_Time"

pend4P_19k$X.<-NULL
names(pend4P_19k)[names(pend4P_19k)=="Temp...F..LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_19k)[names(pend4P_19k)=="Intensity..lum.ft...LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_19k)[names(pend4P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_19k$X.<-NULL
names(pend5P_19k)[names(pend5P_19k)=="Temp...F..LGR.S.N..10747440..SEN.S.N..10747440..LBL..5P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_19k)[names(pend5P_19k)=="Intensity..lum.ft...LGR.S.N..10747440..SEN.S.N..10747440..LBL..5P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_19k)[names(pend5P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_19k$X.<-NULL
names(pend6P_19k)[names(pend6P_19k)=="Temp...F..LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_19k)[names(pend6P_19k)=="Intensity..lum.ft...LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_19k)[names(pend6P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_19k$X.<-NULL
names(pend7P_19k)[names(pend7P_19k)=="Temp...F..LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_19k)[names(pend7P_19k)=="Intensity..lum.ft...LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_19k)[names(pend7P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_19k$X.<-NULL
names(pend8P_19k)[names(pend8P_19k)=="Temp...F..LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_19k)[names(pend8P_19k)=="Intensity..lum.ft...LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_19k)[names(pend8P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_19k$X.<-NULL
names(pend10P_19k)[names(pend10P_19k)=="Temp...F..LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_19k)[names(pend10P_19k)=="Intensity..lum.ft...LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_19k)[names(pend10P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_19k$X.<-NULL
names(pend11P_19k)[names(pend11P_19k)=="Temp...F..LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_19k)[names(pend11P_19k)=="Intensity..lum.ft...LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_19k)[names(pend11P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_19k$X.<-NULL
names(pend12P_19k)[names(pend12P_19k)=="Temp...F..LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_19k)[names(pend12P_19k)=="Intensity..lum.ft...LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_19k)[names(pend12P_19k)=="Date.Time..GMT.04.00"] <- "Date_Time"

pend4P_20k$X.<-NULL
names(pend4P_20k)[names(pend4P_20k)=="Temp...C..LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_20k)[names(pend4P_20k)=="Intensity..Lux..LGR.S.N..10747447..SEN.S.N..10747447..LBL..4P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_20k)[names(pend4P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_20k$X.<-NULL
names(pend5P_20k)[names(pend5P_20k)=="Temp...C..LGR.S.N..10747438..SEN.S.N..10747438..LBL..5P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_20k)[names(pend5P_20k)=="Intensity..Lux..LGR.S.N..10747438..SEN.S.N..10747438..LBL..5P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_20k)[names(pend5P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_20k$X.<-NULL
names(pend6P_20k)[names(pend6P_20k)=="Temp...C..LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_20k)[names(pend6P_20k)=="Intensity..Lux..LGR.S.N..10747438..SEN.S.N..10747438..LBL..6P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_20k)[names(pend6P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_20k$X.<-NULL
names(pend7P_20k)[names(pend7P_20k)=="Temp...C..LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_20k)[names(pend7P_20k)=="Intensity..Lux..LGR.S.N..10747439..SEN.S.N..10747439..LBL..7P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_20k)[names(pend7P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_20k$X.<-NULL
names(pend8P_20k)[names(pend8P_20k)=="Temp...C..LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_20k)[names(pend8P_20k)=="Intensity..Lux..LGR.S.N..10747449..SEN.S.N..10747449..LBL..8P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_20k)[names(pend8P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_20k$X.<-NULL
names(pend10P_20k)[names(pend10P_20k)=="Temp...C..LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_20k)[names(pend10P_20k)=="Intensity..Lux..LGR.S.N..10747502..SEN.S.N..10747502..LBL..10P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_20k)[names(pend10P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_20k$X.<-NULL
names(pend11P_20k)[names(pend11P_20k)=="Temp...C..LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_20k)[names(pend11P_20k)=="Intensity..Lux..LGR.S.N..10747503..SEN.S.N..10747503..LBL..11P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_20k)[names(pend11P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_20k$X.<-NULL
names(pend12P_20k)[names(pend12P_20k)=="Temp...C..LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_20k)[names(pend12P_20k)=="Intensity..Lux..LGR.S.N..10747504..SEN.S.N..10747504..LBL..12P_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_20k)[names(pend12P_20k)=="Date.Time..GMT.04.00"] <- "Date_Time"


# Add in column "Pendant_ID"
pend4P_17k$Pendant_ID<-"4P"
pend5P_17k$Pendant_ID<-"5P"
pend6P_17k$Pendant_ID<-"6P"
pend7P_17k$Pendant_ID<-"7P"
pend8P_17k$Pendant_ID<-"8P"
pend9P_17k$Pendant_ID<-"9P"
pend10P_17k$Pendant_ID<-"10P"
pend11P_17k$Pendant_ID<-"11P"
pend12P_17k$Pendant_ID<-"12P"

pend4P_18k$Pendant_ID<-"4P"
pend5P_18k$Pendant_ID<-"5P"
pend6P_18k$Pendant_ID<-"6P"
pend7P_18k$Pendant_ID<-"7P"
pend8P_18k$Pendant_ID<-"8P"
pend10P_18k$Pendant_ID<-"10P"
pend11P_18k$Pendant_ID<-"11P"
pend12P_18k$Pendant_ID<-"12P"

pend4P_19k$Pendant_ID<-"4P"
pend5P_19k$Pendant_ID<-"5P"
pend6P_19k$Pendant_ID<-"6P"
pend7P_19k$Pendant_ID<-"7P"
pend8P_19k$Pendant_ID<-"8P"
pend10P_19k$Pendant_ID<-"10P"
pend11P_19k$Pendant_ID<-"11P"
pend12P_19k$Pendant_ID<-"12P"

pend4P_20k$Pendant_ID<-"4P"
pend5P_20k$Pendant_ID<-"5P"
pend6P_20k$Pendant_ID<-"6P"
pend7P_20k$Pendant_ID<-"7P"
pend8P_20k$Pendant_ID<-"8P"
pend10P_20k$Pendant_ID<-"10P"
pend11P_20k$Pendant_ID<-"11P"
pend12P_20k$Pendant_ID<-"12P"

# Combine KBS pendant files
pend17k<-rbind(pend4P_17k,pend5P_17k,pend6P_17k,pend7P_17k,pend8P_17k,pend9P_17k,pend10P_17k,pend11P_17k,pend12P_17k)
pend18k<-rbind(pend4P_18k,pend5P_18k,pend6P_18k,pend7P_18k,pend8P_18k,pend10P_18k,pend11P_18k,pend12P_18k)
pend19k<-rbind(pend4P_19k,pend5P_19k,pend6P_19k,pend7P_19k,pend8P_19k,pend10P_19k,pend11P_19k,pend12P_19k)
pend20k<-rbind(pend4P_20k,pend5P_20k,pend6P_20k,pend7P_20k,pend8P_20k,pend10P_20k,pend11P_20k,pend12P_20k)
pend17k$Site<-"KBS"
pend18k$Site<-"KBS"
pend19k$Site<-"KBS"
pend20k$Site<-"KBS"

#Convert C to F for 2020 data
pend20k$Temp_F_XP_air_1m <- celsius.to.fahrenheit(pend20k$Temp_F_XP_air_1m)

# Merge KBS HOBO data from all years
pendk<-rbind(pend17k,pend18k,pend19k,pend20k)
head(pendk) # don't appear to be duplicates
str(pendk) 
dim(pendk) # dimensions = 294808 rows,  5 columns
new.pendk<-unique( pendk[ , 1:5 ] ) #seems to have no duplicate data
dim(pendk) # dimensions = 294808 rows,  5 columns (same dataset)
str(new.pendk)
setdiff(pendk,new.pendk) # no difference
# not sure this code below is necessary
# dplyr::anti_join(new.pendk, pendk)

ggplot(new.pendk, aes(x = Date_Time, y = Temp_F_XP_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ .) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  ylim(-100,200)+
  theme_gray() + theme(legend.position = "bottom")



#### ** U/H stations included here - should they be in this script?** ###
# Add in station data #
Pend1P_1520k<-read.csv("L0/KBS/sensor_data/2015_2016/KBS_1.csv", header =T)
Pend2P_1520k<-read.csv("L0/KBS/sensor_data/2015_2016/KBS_2.csv", header =T)
Pend3P_1520k<-read.csv("L0/KBS/sensor_data/2015_2016/KBS_3.csv", header =T)

# remove HOBO ID's from coloumn and add 'Pendant_ID'
Pend1P_1520k$Pendant_ID<-"1"
Pend2P_1520k$Pendant_ID<-"2"
Pend3P_1520k$Pendant_ID<-"3"

# Extract only necessary columns
Pend1P_ambient_1520k <-  Pend1P_1520k[,c(2,7,13)]
names(Pend1P_ambient_1520k)[names(Pend1P_ambient_1520k)== "X1H_ambient_air_1m"] <- "XH_air_1m"
Pend1P_ambient_1520k$State<-"ambient"
Pend1P_warmed_1520k <-  Pend1P_1520k[,c(2,5,13)]
names(Pend1P_warmed_1520k)[names(Pend1P_warmed_1520k)== "X1H_warmed_air_1m"] <- "XH_air_1m"
Pend1P_warmed_1520k$State<-"warmed"
Pend2P_warmed_1520k <-  Pend2P_1520k[,c(2,7,13)]
names(Pend2P_warmed_1520k)[names(Pend2P_warmed_1520k)== "X2H_warmed_air_1m"] <- "XH_air_1m"
Pend2P_warmed_1520k$State<-"warmed"
Pend2P_ambient_1520k <-  Pend2P_1520k[,c(2,5,13)]
names(Pend2P_ambient_1520k)[names(Pend2P_ambient_1520k)== "X2H_ambient_air_1m"] <- "XH_air_1m"
Pend2P_ambient_1520k$State<-"ambient"
Pend3P_warmed_1520k <-  Pend3P_1520k[,c(2,5,13)]
names(Pend3P_warmed_1520k)[names(Pend3P_warmed_1520k)== "X3H_warmed_air_1m"] <- "XH_air_1m"
Pend3P_warmed_1520k$State<-"warmed"
Pend3P_ambient_1520k <-  Pend3P_1520k[,c(2,7,13)]
names(Pend3P_ambient_1520k)[names(Pend3P_ambient_1520k)== "X3H_ambient_air_1m"] <- "XH_air_1m"
Pend3P_ambient_1520k$State<-"ambient"

# Merge dataframes
New.Pend1P_1520k<-rbind(Pend1P_warmed_1520k,Pend1P_ambient_1520k)
New.Pend2P_1520k<-rbind(Pend2P_warmed_1520k,Pend2P_ambient_1520k)
New.Pend3P_1520k<-rbind(Pend3P_warmed_1520k,Pend3P_ambient_1520k)
Pend13_1520k<-rbind(New.Pend1P_1520k,New.Pend2P_1520k,New.Pend3P_1520k) 
Pend13_1520k$Site<-"KBS"

# # Add column of warmed -ambient temp
# # For each value in "Date_Time" want to subtract warmed, XH_air_1m value by ambient, XH_air_1m value
# new.Pend13_1518k<-Pend13_1518k
# new.Pend13_1518k$Temp.diff<-new.Pend13_1518k$XH_air_1m
# tail(new.Pend13_1518k)
# new.Pend13_1518k %>%  
#   group_by(Date_Time) %>%  
#   mutate(Temp.diff=XH_air_1m[State=="warmed"]-XH_air_1m[State=="ambient"])

# Change class of Datet.Time column from factor to POSIXct date 
Pend13_1520k$Date_Time <- as.POSIXct(Pend13_1520k$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

a1 <- ggplot(Pend13_1520k, aes(x = Date_Time, y = XH_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ State) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  ylim(-100,200) +
  theme_gray() + 
  theme(legend.position = "bottom")
a1

a2 <- ggplot(Pend13_1520k, aes(x = Date_Time, y = XH_air_1m, color = State)) +
  facet_grid(Pendant_ID ~ .) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  ylim(-100,200)+
  theme_gray() + theme(legend.position = "bottom")
a2


### ***UMBS*** ###
# Read in UMBS HOBO data from all years
pend4P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_4P_08152017.csv", skip=1, header =T)
pend5P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_5P_08152017.csv", skip=1, header =T)
pend6P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_6P_08152017.csv", skip=1, header =T)
pend7P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_7P_08152017.csv", skip=1, header =T)
pend8P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_8P_08152017.csv", skip=1, header =T)
pend9P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_9P_08152017.csv", skip=1, header =T)
pend10P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_10P_08152017.csv", skip=1, header =T)
pend11P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_11P_08152017.csv", skip=1, header =T)
pend12P_17u<-read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_12P_08152017.csv", skip=1, header =T)

pend4P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_4P_09192018.csv", skip=1, header =T)
pend5P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_5P_09192018.csv", skip=1, header =T)
pend6P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_6P_09192018.csv", skip=1, header =T)
pend7P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_7P_09192018.csv", skip=1, header =T)
pend8P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_8P_09192018.csv", skip=1, header =T)
pend9P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_9P_09192018.csv", skip=1, header =T)
pend10P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_10P_09192018.csv", skip=1, header =T)
pend11P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_11P_09192018.csv", skip=1, header =T)
pend12P_18u<-read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_12P_09192018.csv", skip=1, header =T)

pend4P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_4P_09052019.csv", skip=1, header =T)[ ,1:4]
pend5P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_5P_09052019.csv", skip=1, header =T)[ ,1:4]
pend6P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_6P_09052019.csv", skip=1, header =T)[ ,1:4]
pend7P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_7P_09052019.csv", skip=1, header =T)[ ,1:4]
pend8P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_8P_09052019.csv", skip=1, header =T)[ ,1:4]
pend9P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_9P_09052019.csv", skip=1, header =T)[ ,1:4]
pend10P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_10P_09052019.csv", skip=1, header =T)[ ,1:4]
pend11P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_11P_09052019.csv", skip=1, header =T)[ ,1:4]
pend12P_19u<-read.csv("L0/UMBS/sensor_data/2019/09_05_2019/UMBS_12P_09052019.csv", skip=1, header =T)[ ,1:4]

pend4P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_4P_06122020.csv", skip=1, header =T)[ ,1:4]
pend5P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_5P_06122020.csv", skip=1, header =T)[ ,1:4]
pend6P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_6P_06122020.csv", skip=1, header =T)[ ,1:4]
pend7P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_7P_06122020.csv", skip=1, header =T)[ ,1:4]
pend8P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_8P_06122020.csv", skip=1, header =T)[ ,1:4]
pend9P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_9P_06122020.csv", skip=1, header =T)[ ,1:4]
pend10P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_10P_06122020.csv", skip=1, header =T)[ ,1:4]
pend11P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_11P_06122020.csv", skip=1, header =T)[ ,1:4]
pend12P_20u<-read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_12P_06122020.csv", skip=1, header =T)[ ,1:4]

# Change class of Datet.Time column from factor to POSIXct date 
pend4P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend9P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend9P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_17u$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_17u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

pend4P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend9P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend9P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_18u$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_18u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

pend4P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend9P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend9P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_19u$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_19u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

pend4P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend4P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend5P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend6P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend7P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend8P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend9P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend9P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend10P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend11P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_20u$Date.Time..GMT.04.00 <- as.POSIXct(pend12P_20u$Date.Time..GMT.04.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

# Rename columns and remove unnecessary rows.
pend4P_17u$X.<-NULL
names(pend4P_17u)[names(pend4P_17u)=="Temp...F..LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_17u)[names(pend4P_17u)=="Intensity..lum.ft...LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_17u)[names(pend4P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_17u$X.<-NULL
names(pend5P_17u)[names(pend5P_17u)=="Temp...F..LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_17u)[names(pend5P_17u)=="Intensity..lum.ft...LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_17u)[names(pend5P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_17u$X.<-NULL
names(pend6P_17u)[names(pend6P_17u)=="Temp...F..LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_17u)[names(pend6P_17u)=="Intensity..lum.ft...LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_17u)[names(pend6P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_17u$X.<-NULL
names(pend7P_17u)[names(pend7P_17u)=="Temp...F..LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_17u)[names(pend7P_17u)=="Intensity..lum.ft...LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_17u)[names(pend7P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_17u$X.<-NULL
names(pend8P_17u)[names(pend8P_17u)=="Temp...F..LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_17u)[names(pend8P_17u)=="Intensity..lum.ft...LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_17u)[names(pend8P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend9P_17u$X.<-NULL
names(pend9P_17u)[names(pend9P_17u)=="Temp...F..LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend9P_17u)[names(pend9P_17u)=="Intensity..lum.ft...LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend9P_17u)[names(pend9P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_17u$X.<-NULL
names(pend10P_17u)[names(pend10P_17u)=="Temp...F..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_17u)[names(pend10P_17u)=="Intensity..lum.ft...LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_17u)[names(pend10P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_17u$X.<-NULL
names(pend11P_17u)[names(pend11P_17u)=="Temp...F..LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_17u)[names(pend11P_17u)=="Intensity..lum.ft...LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_17u)[names(pend11P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_17u$X.<-NULL
names(pend12P_17u)[names(pend12P_17u)=="Temp...F..LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_17u)[names(pend12P_17u)=="Intensity..lum.ft...LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_17u)[names(pend12P_17u)=="Date.Time..GMT.04.00"] <- "Date_Time"

pend4P_18u$X.<-NULL
names(pend4P_18u)[names(pend4P_18u)=="Temp...F..LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_18u)[names(pend4P_18u)=="Intensity..lum.ft...LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_18u)[names(pend4P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_18u$X.<-NULL
names(pend5P_18u)[names(pend5P_18u)=="Temp...F..LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_18u)[names(pend5P_18u)=="Intensity..lum.ft...LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_18u)[names(pend5P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_18u$X.<-NULL
names(pend6P_18u)[names(pend6P_18u)=="Temp...F..LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_18u)[names(pend6P_18u)=="Intensity..lum.ft...LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_18u)[names(pend6P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_18u$X.<-NULL
names(pend7P_18u)[names(pend7P_18u)=="Temp...F..LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_18u)[names(pend7P_18u)=="Intensity..lum.ft...LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_18u)[names(pend7P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_18u$X.<-NULL
names(pend8P_18u)[names(pend8P_18u)=="Temp...F..LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_18u)[names(pend8P_18u)=="Intensity..lum.ft...LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_18u)[names(pend8P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend9P_18u$X.<-NULL
names(pend9P_18u)[names(pend9P_18u)=="Temp...F..LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend9P_18u)[names(pend9P_18u)=="Intensity..lum.ft...LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend9P_18u)[names(pend9P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_18u$X.<-NULL
names(pend10P_18u)[names(pend10P_18u)=="Temp...F..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_18u)[names(pend10P_18u)=="Intensity..lum.ft...LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_18u)[names(pend10P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_18u$X.<-NULL
names(pend11P_18u)[names(pend11P_18u)=="Temp...F..LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_18u)[names(pend11P_18u)=="Intensity..lum.ft...LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_18u)[names(pend11P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_18u$X.<-NULL
names(pend12P_18u)[names(pend12P_18u)=="Temp...F..LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_18u)[names(pend12P_18u)=="Intensity..lum.ft...LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_18u)[names(pend12P_18u)=="Date.Time..GMT.04.00"] <- "Date_Time"

pend4P_19u$X.<-NULL
names(pend4P_19u)[names(pend4P_19u)=="Temp...F..LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_19u)[names(pend4P_19u)=="Intensity..lum.ft...LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_19u)[names(pend4P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_19u$X.<-NULL
names(pend5P_19u)[names(pend5P_19u)=="Temp...F..LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_19u)[names(pend5P_19u)=="Intensity..lum.ft...LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_19u)[names(pend5P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_19u$X.<-NULL
names(pend6P_19u)[names(pend6P_19u)=="Temp...F..LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_19u)[names(pend6P_19u)=="Intensity..lum.ft...LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_19u)[names(pend6P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_19u$X.<-NULL
names(pend7P_19u)[names(pend7P_19u)=="Temp...F..LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_19u)[names(pend7P_19u)=="Intensity..lum.ft...LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_19u)[names(pend7P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_19u$X.<-NULL
names(pend8P_19u)[names(pend8P_19u)=="Temp...F..LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_19u)[names(pend8P_19u)=="Intensity..lum.ft...LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_19u)[names(pend8P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend9P_19u$X.<-NULL
names(pend9P_19u)[names(pend9P_19u)=="Temp...F..LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend9P_19u)[names(pend9P_19u)=="Intensity..lum.ft...LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend9P_19u)[names(pend9P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_19u$X.<-NULL
names(pend10P_19u)[names(pend10P_19u)=="Temp...F..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_19u)[names(pend10P_19u)=="Intensity..lum.ft...LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_19u)[names(pend10P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_19u$X.<-NULL
names(pend11P_19u)[names(pend11P_19u)=="Temp...F..LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_19u)[names(pend11P_19u)=="Intensity..lum.ft...LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_19u)[names(pend11P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_19u$X.<-NULL
names(pend12P_19u)[names(pend12P_19u)=="Temp...F..LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_19u)[names(pend12P_19u)=="Intensity..lum.ft...LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_19u)[names(pend12P_19u)=="Date.Time..GMT.04.00"] <- "Date_Time"

pend4P_20u$X.<-NULL
names(pend4P_20u)[names(pend4P_20u)=="Temp...C..LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend4P_20u)[names(pend4P_20u)=="Intensity..Lux..LGR.S.N..10747446..SEN.S.N..10747446..LBL..C1_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend4P_20u)[names(pend4P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend5P_20u$X.<-NULL
names(pend5P_20u)[names(pend5P_20u)=="Temp...C..LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend5P_20u)[names(pend5P_20u)=="Intensity..Lux..LGR.S.N..10747507..SEN.S.N..10747507..LBL..A2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend5P_20u)[names(pend5P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend6P_20u$X.<-NULL
names(pend6P_20u)[names(pend6P_20u)=="Temp...C..LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend6P_20u)[names(pend6P_20u)=="Intensity..Lux..LGR.S.N..10747505..SEN.S.N..10747505..LBL..B2_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend6P_20u)[names(pend6P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend7P_20u$X.<-NULL
names(pend7P_20u)[names(pend7P_20u)=="Temp...C..LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend7P_20u)[names(pend7P_20u)=="Intensity..Lux..LGR.S.N..10747506..SEN.S.N..10747506..LBL..B3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend7P_20u)[names(pend7P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend8P_20u$X.<-NULL
names(pend8P_20u)[names(pend8P_20u)=="Temp...C..LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend8P_20u)[names(pend8P_20u)=="Intensity..Lux..LGR.S.N..10747444..SEN.S.N..10747444..LBL..D3_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend8P_20u)[names(pend8P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend9P_20u$X.<-NULL
names(pend9P_20u)[names(pend9P_20u)=="Temp...C..LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend9P_20u)[names(pend9P_20u)=="Intensity..Lux..LGR.S.N..10747445..SEN.S.N..10747445..LBL..A4_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend9P_20u)[names(pend9P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend10P_20u$X.<-NULL
names(pend10P_20u)[names(pend10P_20u)=="Temp...C..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_20u)[names(pend10P_20u)=="Intensity..Lux..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_20u)[names(pend10P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend11P_20u$X.<-NULL
names(pend11P_20u)[names(pend11P_20u)=="Temp...C..LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend11P_20u)[names(pend11P_20u)=="Intensity..Lux..LGR.S.N..10747443..SEN.S.N..10747443..LBL..C6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend11P_20u)[names(pend11P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"
pend12P_20u$X.<-NULL
names(pend12P_20u)[names(pend12P_20u)=="Temp...C..LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_air_1m."] <- "Temp_F_XP_air_1m"
names(pend12P_20u)[names(pend12P_20u)=="Intensity..Lux..LGR.S.N..10747442..SEN.S.N..10747442..LBL..D6_warmed_light_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend12P_20u)[names(pend12P_20u)=="Date.Time..GMT.04.00"] <- "Date_Time"

# Add in column "Pendant_ID"
pend4P_17u$Pendant_ID<-"4P"
pend5P_17u$Pendant_ID<-"5P"
pend6P_17u$Pendant_ID<-"6P"
pend7P_17u$Pendant_ID<-"7P"
pend8P_17u$Pendant_ID<-"8P"
pend9P_17u$Pendant_ID<-"9P"
pend10P_17u$Pendant_ID<-"10P"
pend11P_17u$Pendant_ID<-"11P"
pend12P_17u$Pendant_ID<-"12P"

pend4P_18u$Pendant_ID<-"4P"
pend5P_18u$Pendant_ID<-"5P"
pend6P_18u$Pendant_ID<-"6P"
pend7P_18u$Pendant_ID<-"7P"
pend8P_18u$Pendant_ID<-"8P"
pend9P_18u$Pendant_ID<-"9P"
pend10P_18u$Pendant_ID<-"10P"
pend11P_18u$Pendant_ID<-"11P"
pend12P_18u$Pendant_ID<-"12P"

pend4P_19u$Pendant_ID<-"4P"
pend5P_19u$Pendant_ID<-"5P"
pend6P_19u$Pendant_ID<-"6P"
pend7P_19u$Pendant_ID<-"7P"
pend8P_19u$Pendant_ID<-"8P"
pend9P_19u$Pendant_ID<-"9P"
pend10P_19u$Pendant_ID<-"10P"
pend11P_19u$Pendant_ID<-"11P"
pend12P_19u$Pendant_ID<-"12P"

pend4P_20u$Pendant_ID<-"4P"
pend5P_20u$Pendant_ID<-"5P"
pend6P_20u$Pendant_ID<-"6P"
pend7P_20u$Pendant_ID<-"7P"
pend8P_20u$Pendant_ID<-"8P"
pend9P_20u$Pendant_ID<-"9P"
pend10P_20u$Pendant_ID<-"10P"
pend11P_20u$Pendant_ID<-"11P"
pend12P_20u$Pendant_ID<-"12P"


# combine UMBS pendant files for 2017 and 2018
pend17u<-rbind(pend4P_17u,pend5P_17u,pend6P_17u,pend7P_17u,pend8P_17u,pend9P_17u,pend10P_17u,pend11P_17u,pend12P_17u) 
pend17u$Site<-"UMBS"

pend18u<-rbind(pend4P_18u,pend5P_18u,pend6P_18u,pend7P_18u,pend8P_18u,pend9P_18u,pend10P_18u,pend11P_18u,pend12P_18u)
pend18u$Site<-"UMBS"

pend19u<-rbind(pend4P_19u,pend5P_19u,pend6P_19u,pend7P_19u,pend8P_19u,pend9P_19u,pend10P_19u,pend11P_19u,pend12P_19u)
pend19u$Site<-"UMBS"

pend20u<-rbind(pend4P_20u,pend5P_20u,pend6P_20u,pend7P_20u,pend8P_20u,pend9P_20u,pend10P_20u,pend11P_20u,pend12P_20u)
pend20u$Site<-"UMBS"

#Convert C to F for 2020 data
pend20u$Temp_F_XP_air_1m <- celsius.to.fahrenheit(pend20u$Temp_F_XP_air_1m)

# merge UMBS HOBO data from all years
pendu<-rbind(pend17u,pend18u,pend19u,pend20u)
str(pendu)
new.pendu<-unique( pendu[ , 1:5 ] ) #want to delete overlapping data from HOBO files, but don't know best way
str(new.pendu)
setdiff(pendu,new.pendu)
dplyr::anti_join(new.pendu, pendu)
head(new.pendu)

### Add in station data ###
Pend1P_1520u<-read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_1.csv", header =T)
Pend2P_1520u<-read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_2.csv", header =T)
Pend3P_1520u<-read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_3.csv", header =T)
head(Pend1P_1520u)

# remove HOBO ID's from coloumn and add 'Pendant_ID'
Pend1P_1520u$Pendant_ID<-"1"
Pend2P_1520u$Pendant_ID<-"2"
Pend3P_1520u$Pendant_ID<-"3"

# Extract only necessary columns
Pend1P_ambient_1520u <-  Pend1P_1520u[,c(2,7,13)]
names(Pend1P_ambient_1520u)[names(Pend1P_ambient_1520u)== "X1H_ambient_air_1m"] <- "XH_air_1m"
Pend1P_ambient_1520u$State<-"ambient"
Pend1P_warmed_1520u <-  Pend1P_1520u[,c(2,5,13)]
names(Pend1P_warmed_1520u)[names(Pend1P_warmed_1520u)== "X1H_warmed_air_1m"] <- "XH_air_1m"
Pend1P_warmed_1520u$State<-"warmed"
Pend2P_warmed_1520u <-  Pend2P_1520u[,c(2,7,13)]
names(Pend2P_warmed_1520u)[names(Pend2P_warmed_1520u)== "X2H_warmed_air_1m"] <- "XH_air_1m"
Pend2P_warmed_1520u$State<-"warmed"
Pend2P_ambient_1520u <-  Pend2P_1520u[,c(2,5,13)]
names(Pend2P_ambient_1520u)[names(Pend2P_ambient_1520u)== "X2H_ambient_air_1m"] <- "XH_air_1m"
Pend2P_ambient_1520u$State<-"ambient"
Pend3P_warmed_1520u <-  Pend3P_1520u[,c(2,5,13)]
names(Pend3P_warmed_1520u)[names(Pend3P_warmed_1520u)== "X3H_warmed_air_1m"] <- "XH_air_1m"
Pend3P_warmed_1520u$State<-"warmed"
Pend3P_ambient_1520u <-  Pend3P_1520u[,c(2,7,13)]
names(Pend3P_ambient_1520u)[names(Pend3P_ambient_1520u)== "X3H_ambient_air_1m"] <- "XH_air_1m"
Pend3P_ambient_1520u$State<-"ambient"

# Merge dataframes
New.Pend1P_1520u<-rbind(Pend1P_warmed_1520u,Pend1P_ambient_1520u)
New.Pend2P_1520u<-rbind(Pend2P_warmed_1520u,Pend2P_ambient_1520u)
New.Pend3P_1520u<-rbind(Pend3P_warmed_1520u,Pend3P_ambient_1520u)
Pend13_1520u<-rbind(New.Pend1P_1520u,New.Pend2P_1520u,New.Pend3P_1520u) 
Pend13_1520u$Site<-"UMBS"
#Tempdiff <- aggregate( XH_air_1m~Date_Time+Pendant_ID, data=Pend13_1518u , diff )
#head(Tempdiff)

# Change class of Date.Time column from factor to POSIXct date 
Pend13_1520u$Date_Time <- as.POSIXct(Pend13_1520u$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
a3<- ggplot(Pend13_1520u, aes(x = Date_Time, y = XH_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ State) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  theme_gray() + theme(legend.position = "bottom")
a3
a4<- ggplot(Pend13_1520u, aes(x = Date_Time, y = XH_air_1m, color = State)) +
  facet_grid(Pendant_ID ~ .) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  theme_gray() + theme(legend.position = "bottom")
a4
#grid.arrange(a1,a2,ncol=2)


###### ***DATA ANALYSIS*** ########
## plot it
ggplot(new.pendu, aes(x = Date_Time, y = Temp_F_XP_air_1m, color = Pendant_ID)) +
  facet_grid(. ~ Pendant_ID) +
  # geom_errorbar(aes(ymin=mean_RFU-sd, ymax=mean_RFU+sd))+
  geom_point(alpha=.5, size = 2) +
  # geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

#  take the mean temperature per month by year. 
# UMBS average chamber temp per month, by year
test.pendu<- new.pendu
test.pendu$Month <- months(test.pendu$Date_Time)
test.pendu$Year <- format(test.pendu$Date_Time,format="%y")
mean_monthlyu <- aggregate( Temp_F_XP_air_1m ~ Month + Year , test.pendu , mean )
mean_monthlyu$Month <- factor(mean_monthlyu$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthlyu)
ggplot(mean_monthlyu, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")
# mean temp of the plot, per month, per year 
test2.pendu<- new.pendu
test2.pendu$Month <- months(test.pendu$Date_Time)
test2.pendu$Year <- format(test.pendu$Date_Time,format="%y")
mean_monthly_plotu <- aggregate( Temp_F_XP_air_1m ~ Month + Year + Pendant_ID , test2.pendu , mean )
mean_monthly_plotu$Month <- factor(mean_monthly_plotu$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthly_plotu)
ggplot(mean_monthly_plotu, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

# KBS average chamber temp per month, by year
test.pendk<-pendk
head(test.pendk)
test.pendk$Month <- months(test.pendk$Date_Time)
test.pendk$Year <- format(test.pendk$Date_Time,format="%y")
mean_monthlyk <- aggregate( Temp_F_XP_air_1m ~ Month + Year , test.pendk , mean )
mean_monthlyk$Month <- factor(mean_monthlyk$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthlyk)
ggplot(mean_monthlyk, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")
# mean temp of the plot, per month, per year 
test2.pendk<- new.pendk
test2.pendk$Month <- months(test2.pendk$Date_Time)
test2.pendk$Year <- format(test2.pendk$Date_Time,format="%y")
mean_monthly_plotk <- aggregate( Temp_F_XP_air_1m ~ Month + Year + Pendant_ID , test2.pendk , mean )
mean_monthly_plotk$Month <- factor(mean_monthly_plotk$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthly_plotk)
ggplot(mean_monthly_plotk, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

# Compare ambient vs. warmed 
# Take out station ID's in columns
a <- Pend1P_1520u
colnames(a) <- gsub("X1U","XU", colnames(a))
colnames(a) <- gsub("X1H","XH", colnames(a))
colnames(a)
b <- Pend2P_1520u
colnames(b) <- gsub("X2U","XU", colnames(b))
colnames(b) <- gsub("X2H","XH", colnames(b))
colnames(b)
names(b)[names(b)== "XH_ambient_aim_1m"] <- "XH_ambient_air_1m"
c <- Pend3P_1520u
colnames(c) <- gsub("X3U","XU", colnames(c))
colnames(c) <- gsub("X3H","XH", colnames(c))
colnames(c)

# Merge cleaned station data
Merged.Pendstation_1520u<-rbind(a,b,c)
head(Merged.Pendstation_1520u)
Tempdiffu <- Merged.Pendstation_1520u %>%
  select(Date_Time,Pendant_ID,XH_warmed_air_1m,XH_ambient_air_1m) %>%
  group_by(Date_Time,Pendant_ID) %>%
  mutate(Tempdiff.air_1m=XH_warmed_air_1m-XH_ambient_air_1m)
head(Tempdiffu)
new.Pend13_1520k %>%  
  +   group_by(Date_Time) %>%  
  +   mutate(Temp.diff=XH_air_1m[State=="warmed"]-XH_air_1m[State=="ambient"])



