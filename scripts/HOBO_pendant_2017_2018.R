# **** This script makes the following datasets:
# pendk:        KBS HOBO pendant data from 2015-2018
# pendu:        UMBS HOBO pendant data from 2015-2018
# pend1518:     Combined HOBO pendant data from KBS and UMBS from 2015-2018

# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# set working directory (if you're not PLZ, change this to the correct path for your
# Google Drive directory. It should point to where we have /final_data
setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/")

## Edit below for any packages you'll be using
for (package in c("ggplot2", "gridExtra", "splines", "lme4", "plyr", "tidyr", "RLRsim","lmerTest", "lubridate")) {
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
pend4P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/4P_0.csv", skip=1, header =T)
pend5P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/5P_0.csv", skip=1, header =T)
pend6P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/6P_0.csv", skip=1, header =T)
pend7P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/7P_0.csv", skip=1, header =T)
pend8P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/8P_0.csv", skip=1, header =T)
pend9P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/9P.csv", skip=1, header =T)
pend10P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/10P_0.csv", skip=1, header =T)
pend11P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/11P_0.csv", skip=1, header =T)
pend12P_17k<-read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/12P_0.csv", skip=1, header =T)

# Change class of Datet.Time column from factor to POSIXct date 
pend4P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend4P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend5P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend5P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend6P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend6P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend7P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend7P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend8P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend8P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend9P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend9P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend10P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend10P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend11P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend11P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
pend12P_17k$Date.Time..GMT.05.00 <- as.POSIXct(pend12P_17k$Date.Time..GMT.05.00,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

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

# combine KBS pendant files for 2017
pend17k<-rbind(pend4P_17k,pend5P_17k,pend6P_17k,pend7P_17k,pend8P_17k,pend9P_17k,pend10P_17k,pend11P_17k,pend12P_17k)
pend17k$Site<-"KBS"


### ***UMBS*** ###
# Read in UMBS HOBO data from all years
pend4P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_4P_0.csv", skip=1, header =T)
pend5P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_5P_2.csv", skip=1, header =T)
pend6P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_6P_1.csv", skip=1, header =T)
pend7P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_7P_1.csv", skip=1, header =T)
pend8P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_8P.csv", skip=1, header =T)
pend9P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_9P_1.csv", skip=1, header =T)
pend10P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_10P.csv", skip=1, header =T)
pend11P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_11P.csv", skip=1, header =T)
pend12P_17u<-read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_12P.csv", skip=1, header =T)
pend4P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_4P_1.csv", skip=1, header =T)
pend5P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_5P.csv", skip=1, header =T)
pend6P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_6P_1.csv", skip=1, header =T)
pend7P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_7P_1.csv", skip=1, header =T)
pend8P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_8P_1.csv", skip=1, header =T)
pend9P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_9P_1.csv", skip=1, header =T)
pend10P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_10P_2.csv", skip=1, header =T)
pend11P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_11P_1.csv", skip=1, header =T)
pend12P_18u<-read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_12P_1.csv", skip=1, header =T)

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

# combine KBS pendant files for 2017 and 2018
pend17u<-rbind(pend4P_17u,pend5P_17u,pend6P_17u,pend7P_17u,pend8P_17u,pend9P_17u,pend10P_17u,pend11P_17u,pend12P_17u) 
pend17u$Site<-"UMBS"
pend18u<-rbind(pend4P_18u,pend5P_18u,pend6P_18u,pend7P_18u,pend8P_18u,pend9P_18u,pend10P_18u,pend11P_18u,pend12P_18u)
pend18u$Site<-"UMBS"

# merge UMBS HOBO data from all years
pend1618u<-rbind(pend17u,pend18u) #2017 dataset contains data from 2016
new.pend1618u<-unique( pend1618u[ , 1:5 ] )
setdiff(pend1618u, new.pend1618u)
dplyr::anti_join(new.pend1618u, pend1618u)
head(new.pend1618u)

### Add in station data ###
Pend1P_1518u<-read.csv("final_data/UMBS/sensor_data/Merged_UMBS_1.csv", header =T)
Pend2P_1518u<-read.csv("final_data/UMBS/sensor_data/Merged_UMBS_2.csv", header =T)
Pend3P_1518u<-read.csv("final_data/UMBS/sensor_data/Merged_UMBS_3.csv", header =T)
head(Pend1P_1518u)

# remove HOBO ID's from coloumn and add 'Pendant_ID'
Pend1P_1518u$Pendant_ID<-"1"
Pend2P_1518u$Pendant_ID<-"2"
Pend3P_1518u$Pendant_ID<-"3"

# Extract only necessary columns
Pend1P_ambient_1518u <-  Pend1P_1518u[,c(3,6,14)]
names(Pend1P_ambient_1518u)[names(Pend1P_ambient_1518u)== "X1H_ambient_air_1m"] <- "XH_air_1m"
Pend1P_ambient_1518u$State<-"ambient"
Pend1P_warmed_1518u <-  Pend1P_1518u[,c(3,8,14)]
names(Pend1P_warmed_1518u)[names(Pend1P_warmed_1518u)== "X1H_warmed_air_1m"] <- "XH_air_1m"
Pend1P_warmed_1518u$State<-"warmed"
Pend2P_warmed_1518u <-  Pend2P_1518u[,c(3,6,14)]
names(Pend2P_warmed_1518u)[names(Pend2P_warmed_1518u)== "X2H_warmed_air_1m"] <- "XH_air_1m"
Pend2P_warmed_1518u$State<-"warmed"
Pend2P_ambient_1518u <-  Pend2P_1518u[,c(3,8,14)]
names(Pend2P_ambient_1518u)[names(Pend2P_ambient_1518u)== "X2H_ambient_aim_1m"] <- "XH_air_1m"
Pend2P_ambient_1518u$State<-"ambient"
Pend3P_warmed_1518u <-  Pend3P_1518u[,c(3,6,14)]
names(Pend3P_warmed_1518u)[names(Pend3P_warmed_1518u)== "X3H_warmed_air_1m"] <- "XH_air_1m"
Pend3P_warmed_1518u$State<-"warmed"
Pend3P_ambient_1518u <-  Pend3P_1518u[,c(3,8,14)]
names(Pend3P_ambient_1518u)[names(Pend3P_ambient_1518u)== "X3H_ambient_air_1m"] <- "XH_air_1m"
Pend3P_ambient_1518u$State<-"ambient"

# Merge dataframes
New.Pend1P_1518u<-rbind(Pend1P_warmed_1518u,Pend1P_ambient_1518u)
New.Pend2P_1518u<-rbind(Pend2P_warmed_1518u,Pend2P_ambient_1518u)
New.Pend3P_1518u<-rbind(Pend3P_warmed_1518u,Pend3P_ambient_1518u)
Pend13_1518u<-rbind(New.Pend1P_1518u,New.Pend2P_1518u,New.Pend3P_1518u) 
Pend13_1518u$Site<-"UMBS"

# Change class of Datet.Time column from factor to POSIXct date 
Pend13_1518u$Date_Time <- as.POSIXct(Pend13_1518u$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
a1<- ggplot(Pend13_1518u, aes(x = Date_Time, y = XH_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ State) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  theme_gray() + theme(legend.position = "bottom")
a1
grid.arrange(a1,a2,ncol=2)

###### ***DATA ANALYSIS*** ########
## plot it
ggplot(new.pend1618u, aes(x = Date_Time, y = Temp_F_XP_air_1m, color = Pendant_ID)) +
  facet_grid(. ~ Pendant_ID) +
  #geom_errorbar(aes(ymin=mean_RFU-sd, ymax=mean_RFU+sd))+
  geom_point(alpha=.5, size = 2) +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

# take the mean temperature per month by year. 
test.pend1618u<- new.pend1618u
test.pend1618u$Month <- months(test.pend1618u$Date_Time)
test.pend1618u$Year <- format(test.pend1618u$Date_Time,format="%y")
mean_monthly <- aggregate( Temp_F_XP_air_1m ~ Month + Year , test.pend1618u , mean )
head(mean_monthly)
ggplot(mean_monthly, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")
