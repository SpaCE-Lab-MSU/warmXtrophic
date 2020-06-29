# TITLE: HOBO paired sensor cleanup
# AUTHORS: Nina Lany (original), Kathryn Schmidt (original), Kara Dobson (edited June 2020)
# COLLABORATORS: Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: Data imported as csv files from shared Google drive
# DATA OUTPUT: This script reads in HOBO data from raw_data, combines the data from the U21 and the H12 units for each pair, and writes a CSV file to the final_data folder.
    ## The KBS_sensors and UMBS_sensors scripts then take that data and summarize/analyze how the chambers affect the abiotic environment.
# PROJECT: warmXtrophic
# DATE: 2016-2017
    ## KS edit May 23, 2018: created merged files for UMBS; August 1, 2018: remove manual preparation step and add 2018 data from KBS and UMBS
    ## KD edit June 2020: Updated script to insert 2019 and 2020 data


#Load packages
for (package in c("tidyverse", "weathermetrics")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

# Clear all existing data
rm(list=ls())

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

############ KBS Pair 1
#Read in H
##1516 contains both H and U
KBS_1_1516 <- read.csv("L0/KBS/sensor_data/2015_2016/KBS_1.csv")
KBS_1H_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_1H_09012017.csv")
str(KBS_1H_2017)
summary(KBS_1H_2017)
tail(KBS_1H_2017)
KBS_1H_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_1H_07122018.csv", skip=1)
str(KBS_1H_2018)
summary(KBS_1H_2018)
KBS_1H_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_1H_10072019.csv", skip=1)
str(KBS_1H_2019)
summary(KBS_1H_2019)
KBS_1H_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_1H_04052020.csv", skip=1)
str(KBS_1H_2020)
summary(KBS_1H_2020)

#Read in U
KBS_1U_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_1U_09012017.csv")
str(KBS_1U_2017)
summary(KBS_1U_2017)
KBS_1U_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_1U_07122018.csv",skip=1)[ ,1:6]
head(KBS_1U_2018)
str(KBS_1U_2017)
summary(KBS_1U_2017)
KBS_1U_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_1U_10072019.csv",skip=1)[ ,1:6]
str(KBS_1U_2019)
summary(KBS_1U_2019)
KBS_1U_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_1U_04052020.csv",skip=1)[ ,1:6]
str(KBS_1U_2020)
summary(KBS_1U_2020)

# Merge H and U data
KBS_1_2017 <- merge(KBS_1H_2017, KBS_1U_2017, by="Date_Time", all.x=T, all.y=T)
str(KBS_1_2017)
summary(KBS_1_2017)
KBS_1_2018 <- merge(KBS_1H_2018, KBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_1_2018)
summary(KBS_1_2018)
KBS_1_2019 <- merge(KBS_1H_2019, KBS_1U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_1_2019)
summary(KBS_1_2019)
KBS_1_2020 <- merge(KBS_1H_2020, KBS_1U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_1_2020)
summary(KBS_1_2020)

# rename columns to match manually curated data
KBS_1_1516$X<-NULL
KBS_1_2017$X..x<-NULL
KBS_1_2017$X..y<-NULL
KBS_1_2018$X..x<-NULL
KBS_1_2018$X..y<-NULL
KBS_1_2019$X..x<-NULL
KBS_1_2019$X..y<-NULL
KBS_1_2020$X..x<-NULL
KBS_1_2020$X..y<-NULL

names(KBS_1_1516)[names(KBS_1_1516)=="X1U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
names(KBS_1_1516)[names(KBS_1_1516)=="X1U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(KBS_1_1516)[names(KBS_1_1516)=="X1H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(KBS_1_1516)[names(KBS_1_1516)=="X1H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(KBS_1_1516)[names(KBS_1_1516)=="X1H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(KBS_1_1516)[names(KBS_1_1516)=="X1H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(KBS_1_1516)[names(KBS_1_1516)=="X1H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(KBS_1_1516)[names(KBS_1_1516)=="X1H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(KBS_1_1516)[names(KBS_1_1516)=="X1U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(KBS_1_1516)[names(KBS_1_1516)=="X1U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(KBS_1_1516)

names(KBS_1_2017)[names(KBS_1_2017)=="X1U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
names(KBS_1_2017)[names(KBS_1_2017)=="X1U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(KBS_1_2017)[names(KBS_1_2017)=="X1H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(KBS_1_2017)[names(KBS_1_2017)=="X1H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(KBS_1_2017)[names(KBS_1_2017)=="X1H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(KBS_1_2017)[names(KBS_1_2017)=="X1H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(KBS_1_2017)[names(KBS_1_2017)=="X1H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(KBS_1_2017)[names(KBS_1_2017)=="X1H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(KBS_1_2017)[names(KBS_1_2017)=="X1U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(KBS_1_2017)[names(KBS_1_2017)=="X1U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(KBS_1_2017)

names(KBS_1_2018)[names(KBS_1_2018)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736060..LBL..1H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736062..LBL..1H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="RH.....LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="RH.....LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_1_2018)

names(KBS_1_2019)[names(KBS_1_2019)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736060..LBL..1H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_1_2019)[names(KBS_1_2019)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736062..LBL..1H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_1_2019)[names(KBS_1_2019)=="Temp...F..LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_1_2019)[names(KBS_1_2019)=="RH.....LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_1_2019)[names(KBS_1_2019)=="Temp...F..LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_1_2019)[names(KBS_1_2019)=="RH.....LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_1_2019)[names(KBS_1_2019)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_1_2019)[names(KBS_1_2019)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_1_2019)[names(KBS_1_2019)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_1_2019)[names(KBS_1_2019)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_1_2019)[names(KBS_1_2019)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_1_2019)

names(KBS_1_2020)[names(KBS_1_2020)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736060..LBL..1H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_1_2020)[names(KBS_1_2020)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736062..LBL..1H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_1_2020)[names(KBS_1_2020)=="Temp...C..LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_1_2020)[names(KBS_1_2020)=="RH.....LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_1_2020)[names(KBS_1_2020)=="Temp...C..LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_1_2020)[names(KBS_1_2020)=="RH.....LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_1_2020)[names(KBS_1_2020)=="Temp...C..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_1_2020)[names(KBS_1_2020)=="Temp...C..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_1_2020)[names(KBS_1_2020)=="Temp...C..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_1_2020)[names(KBS_1_2020)=="Temp...C..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_1_2020)[names(KBS_1_2020)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_1_2020)

#Convert to POSIXct date
KBS_1_1516$Date_Time <- as.POSIXct(KBS_1_1516$Date_Time,format="%m/%d/%y %I:%M", tz="UTC")
KBS_1_2017$Date_Time <- as.POSIXct(KBS_1_2017$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_1_2018$Date_Time <- as.POSIXct(KBS_1_2018$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_1_2019$Date_Time <- as.POSIXct(KBS_1_2019$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_1_2020$Date_Time <- as.POSIXct(KBS_1_2020$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

#Convert C to F for 2020
KBS_1_2020$XH_warmed_air_1m <- celsius.to.fahrenheit(KBS_1_2020$XH_warmed_air_1m)
KBS_1_2020$XH_ambient_air_1m <- celsius.to.fahrenheit(KBS_1_2020$XH_ambient_air_1m)

# #check any columns with NA values, if present
# #check to see if all days contain 24h.(only first and last days not expected to have 24h.)
# DateTime1 <- as.POSIXlt(KBS_1$Date_Time, format = "%m/%d/%y %H:%M")
# str(DateTime1)
# DateTime1_hours <- as.numeric(format(DateTime1, "%H"))
# str(DateTime1_hours)
# DateTime1_days <- format(DateTime1, "%m%d%y")
# str(DateTime1_days)
# testing1 <- tapply(DateTime1_hours, as.factor(DateTime1_days), length)
# subset(testing1, testing1<24)
# subset(testing1, testing1>24)

#Write csv files
write.csv(KBS_1_1516, file="L0/KBS/sensor_data/2015_2016/KBS_1_clean.csv")
write.csv(KBS_1_2017, file="L0/KBS/sensor_data/2017/KBS_1.csv")
write.csv(KBS_1_2018, file="L0/KBS/sensor_data/2018/KBS_1.csv")
write.csv(KBS_1_2019, file="L0/KBS/sensor_data/2019/KBS_1.csv")
write.csv(KBS_1_2020, file="L0/KBS/sensor_data/2019/KBS_1.csv")

#Merge data from all years
diff1617_1k <- anti_join(KBS_1_2017, KBS_1_1516, by = "Date_Time")
diff1718_1k <- anti_join(KBS_1_2018, KBS_1_2017, by = "Date_Time")
diff1819_1k <- anti_join(KBS_1_2019, KBS_1_2018, by = "Date_Time")
diff1920_1k <- anti_join(KBS_1_2020, KBS_1_2019, by = "Date_Time")

KBS_1 <- rbind(KBS_1_1516, diff1617_1k, diff1718_1k, diff1819_1k, diff1920_1k)
write.csv(KBS_1, file="L0/KBS/sensor_data/KBS_1.csv")

############## KBS Pair 2
#Read in H
KBS_2_1516 <- read.csv("L0/KBS/sensor_data/2015_2016/KBS_2.csv")
KBS_2H_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_2H_09012017.csv")
str(KBS_2H_2017)
summary(KBS_2H_2017)
KBS_2H_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_2H_07122018.csv", skip=1)
str(KBS_2H_2018)
summary(KBS_2H_2018)
KBS_2H_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_2H_10072019.csv", skip=1)
str(KBS_2H_2019)
summary(KBS_2H_2019)
KBS_2H_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_2H_04052020.csv", skip=1)
str(KBS_2H_2020)
summary(KBS_2H_2020)

#Read in U
KBS_2U_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_2U_09012017.csv")
str(KBS_2U_2017)
summary(KBS_2U_2017)
KBS_2U_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_2U_07122018.csv",skip=1)[ ,1:6]
str(KBS_2U_2018)
summary(KBS_2U_2018)
KBS_2U_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_2U_10072019.csv",skip=1)[ ,1:6]
str(KBS_2U_2019)
summary(KBS_2U_2019)
KBS_2U_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_2U_04052020.csv",skip=1)[ ,1:6]
str(KBS_2U_2020)
summary(KBS_2U_2020)

#Merge H and U
KBS_2_2017 <- merge(KBS_2H_2017, KBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
str(KBS_2_2017)
summary(KBS_2_2017)
KBS_2_2018 <- merge(KBS_2H_2018, KBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_2_2018)
summary(KBS_2_2018)
head(KBS_2_2018)
KBS_2_2019 <- merge(KBS_2H_2019, KBS_2U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_2_2019)
summary(KBS_2_2019)
head(KBS_2_2019)
KBS_2_2020 <- merge(KBS_2H_2020, KBS_2U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_2_2020)
summary(KBS_2_2020)
head(KBS_2_2020)

# rename columns to match manually curated data
KBS_2_1516$X<-NULL
KBS_2_2017$X..x<-NULL
KBS_2_2017$X..y<-NULL
KBS_2_2018$X..x<-NULL
KBS_2_2018$X..y<-NULL
KBS_2_2019$X..x<-NULL
KBS_2_2019$X..y<-NULL
KBS_2_2020$X..x<-NULL
KBS_2_2020$X..y<-NULL

names(KBS_2_1516)[names(KBS_2_1516)=="X2U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
names(KBS_2_1516)[names(KBS_2_1516)=="X2U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(KBS_2_1516)[names(KBS_2_1516)=="X2H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(KBS_2_1516)[names(KBS_2_1516)=="X2H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(KBS_2_1516)[names(KBS_2_1516)=="X2H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(KBS_2_1516)[names(KBS_2_1516)=="X2H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(KBS_2_1516)[names(KBS_2_1516)=="X2H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(KBS_2_1516)[names(KBS_2_1516)=="X2H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(KBS_2_1516)[names(KBS_2_1516)=="X2U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(KBS_2_1516)[names(KBS_2_1516)=="X2U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(KBS_2_1516)

names(KBS_2_2017)[names(KBS_2_2017)=="X2U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_ambient_soil_moist_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(KBS_2_2017)[names(KBS_2_2017)=="X2U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(KBS_2_2017)

names(KBS_2_2018)[names(KBS_2_2018)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736063..LBL..2H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="RH.....LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="RH.....LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_2_2018)

names(KBS_2_2019)[names(KBS_2_2019)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_2_2019)[names(KBS_2_2019)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736063..LBL..2H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_2_2019)[names(KBS_2_2019)=="Temp...F..LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_2_2019)[names(KBS_2_2019)=="RH.....LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_2_2019)[names(KBS_2_2019)=="Temp...F..LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_2_2019)[names(KBS_2_2019)=="RH.....LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_2_2019)[names(KBS_2_2019)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_2_2019)[names(KBS_2_2019)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_2_2019)[names(KBS_2_2019)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_2_2019)[names(KBS_2_2019)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_2_2019)[names(KBS_2_2019)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_2_2019)

names(KBS_2_2020)[names(KBS_2_2020)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_2_2020)[names(KBS_2_2020)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736063..LBL..2H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_2_2020)[names(KBS_2_2020)=="Temp...C..LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_2_2020)[names(KBS_2_2020)=="RH.....LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_2_2020)[names(KBS_2_2020)=="Temp...C..LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_2_2020)[names(KBS_2_2020)=="RH.....LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_2_2020)[names(KBS_2_2020)=="Temp...C..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_2_2020)[names(KBS_2_2020)=="Temp...C..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_2_2020)[names(KBS_2_2020)=="Temp...C..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_2_2020)[names(KBS_2_2020)=="Temp...C..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_2_2020)[names(KBS_2_2020)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_2_2020)

#Convert to POSIXct date
KBS_2_1516$Date_Time <- as.POSIXct(KBS_2_1516$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_2_2017$Date_Time <- as.POSIXct(KBS_2_2017$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_2_2018$Date_Time <- as.POSIXct(KBS_2_2018$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_2_2019$Date_Time <- as.POSIXct(KBS_2_2019$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_2_2020$Date_Time <- as.POSIXct(KBS_2_2020$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

#Convert C to F for 2020
KBS_2_2020$XH_warmed_air_1m <- celsius.to.fahrenheit(KBS_2_2020$XH_warmed_air_1m)
KBS_2_2020$XH_ambient_air_1m <- celsius.to.fahrenheit(KBS_2_2020$XH_ambient_air_1m)

#check any columns with NA values, if present
# which(is.na(KBS_2$X2H_ambient_air_1m))
# KBS_2[2538,]
# KBS_2[2634,]#2H not logged data 2015-06-15 17:00 through 2015-06-19 09:00.  Will not be a problem becasue it gets subsetted out later.
# which(is.na(KBS_2$X2U_ambient_air_10cm))
# KBS_2[51,]
# #check to see if all days contain 24h.(only first and last days not expected to have 24h.)
# DateTime2 <- as.POSIXlt(KBS_2$Date_Time, format = "%m/%d/%y %H:%M")
# str(DateTime2)
# DateTime2_hours <- as.numeric(format(DateTime2, "%H"))
# str(DateTime2_hours)
# DateTime2_days <- format(DateTime2, "%m%d%y")
# str(DateTime2_days)
# testing2 <- tapply(DateTime2_hours, as.factor(DateTime2_days), length)
# subset(testing2, testing2<24)
# subset(testing2, testing2>24)

#Write csv files
write.csv(KBS_2_1516, file="L0/KBS/sensor_data/2015_2016/KBS_2_clean.csv")
write.csv(KBS_2_2017, file="L0/KBS/sensor_data/2017/KBS_2.csv")
write.csv(KBS_2_2018, file="L0/KBS/sensor_data/2018/KBS_2.csv")
write.csv(KBS_2_2019, file="L0/KBS/sensor_data/2019/KBS_2.csv")
write.csv(KBS_2_2019, file="L0/KBS/sensor_data/2020/KBS_2.csv")

#Merge data from all years
diff1617_2k <- anti_join(KBS_2_2017, KBS_2_1516, by = "Date_Time")
diff1718_2k <- anti_join(KBS_2_2018, KBS_2_2017, by = "Date_Time")
diff1819_2k<- anti_join(KBS_2_2019, KBS_2_2018, by = "Date_Time")
diff1920_2k <- anti_join(KBS_2_2020, KBS_2_2019, by = "Date_Time")

KBS_2 <- rbind(KBS_2_1516, diff1617_2k, diff1718_2k, diff1819_2k, diff1920_2k)
write.csv(KBS_2, file="L0/KBS/sensor_data/KBS_2.csv")

############# KBS Pair 3
#Read in H
KBS_3_1516 <- read.csv("L0/KBS/sensor_data/2015_2016/KBS_3.csv")
KBS_3H_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_3H_09012017.csv")
str(KBS_3H_2017)
summary(KBS_3H_2017)
KBS_3H_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_3H_07122018.csv", skip=1)
str(KBS_3H_2018)
summary(KBS_3H_2018)
KBS_3H_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_3H_10072019.csv", skip=1)
str(KBS_3H_2019)
summary(KBS_3H_2019)
KBS_3H_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_3H_04052020.csv", skip=1)
str(KBS_3H_2020)
summary(KBS_3H_2020)

#Read in U
KBS_3U_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_3U_09012017.csv")
str(KBS_3U_2017)
summary(KBS_3U_2017)
KBS_3U_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_3U_07122018.csv",skip=1)[ ,1:6]
str(KBS_3U_2018)
summary(KBS_3U_2018)
KBS_3U_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_3U_10072019.csv",skip=1)[ ,1:6]
str(KBS_3U_2019)
summary(KBS_3U_2019)
KBS_3U_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_3U_04052020.csv",skip=1)[ ,1:6]
str(KBS_3U_2020)
summary(KBS_3U_2020)

#Merge H and U
KBS_3_2017 <- merge(KBS_3H_2017, KBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
str(KBS_3_2017)
summary(KBS_3_2017)
KBS_3_2018 <- merge(KBS_3H_2018, KBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_3_2018)
summary(KBS_3_2018)
head(KBS_3_2018)
KBS_3_2019 <- merge(KBS_3H_2019, KBS_3U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_3_2019)
summary(KBS_3_2019)
head(KBS_3_2019)
KBS_3_2020 <- merge(KBS_3H_2020, KBS_3U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_3_2020)
summary(KBS_3_2020)
head(KBS_3_2020)

# rename columns to match manually curated data
KBS_3_1516$X<-NULL
KBS_3_2017$X..x<-NULL
KBS_3_2017$X..y<-NULL
KBS_3_2018$X..x<-NULL
KBS_3_2018$X..y<-NULL
KBS_3_2019$X..x<-NULL
KBS_3_2019$X..y<-NULL
KBS_3_2020$X..x<-NULL
KBS_3_2020$X..y<-NULL

names(KBS_3_1516)[names(KBS_3_1516)=="X3U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
names(KBS_3_1516)[names(KBS_3_1516)=="X3U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(KBS_3_1516)[names(KBS_3_1516)=="X3H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(KBS_3_1516)[names(KBS_3_1516)=="X3H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(KBS_3_1516)[names(KBS_3_1516)=="X3H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(KBS_3_1516)[names(KBS_3_1516)=="X3H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(KBS_3_1516)[names(KBS_3_1516)=="X3H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(KBS_3_1516)[names(KBS_3_1516)=="X3H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(KBS_3_1516)[names(KBS_3_1516)=="X3U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(KBS_3_1516)[names(KBS_3_1516)=="X3U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(KBS_3_1516)

names(KBS_3_2017)[names(KBS_3_2017)=="X3U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(KBS_3_2017)[names(KBS_3_2017)=="X3U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_ambient_soil_moistire_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(KBS_3_2017)[names(KBS_3_2017)=="X3U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(KBS_3_2017)[names(KBS_3_2017)=="X3U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(KBS_3_2017)

names(KBS_3_2018)[names(KBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736053..LBL..3H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="RH.....LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="RH.....LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_3_2018)

names(KBS_3_2019)[names(KBS_3_2019)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736053..LBL..3H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_3_2019)[names(KBS_3_2019)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_3_2019)[names(KBS_3_2019)=="Temp...F..LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_3_2019)[names(KBS_3_2019)=="RH.....LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_3_2019)[names(KBS_3_2019)=="Temp...F..LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_3_2019)[names(KBS_3_2019)=="RH.....LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_3_2019)[names(KBS_3_2019)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_3_2019)[names(KBS_3_2019)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_3_2019)[names(KBS_3_2019)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_3_2019)[names(KBS_3_2019)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_3_2019)[names(KBS_3_2019)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_3_2019)

names(KBS_3_2020)[names(KBS_3_2020)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736053..LBL..3H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(KBS_3_2020)[names(KBS_3_2020)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(KBS_3_2020)[names(KBS_3_2020)=="Temp...C..LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(KBS_3_2020)[names(KBS_3_2020)=="RH.....LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(KBS_3_2020)[names(KBS_3_2020)=="Temp...C..LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(KBS_3_2020)[names(KBS_3_2020)=="RH.....LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(KBS_3_2020)[names(KBS_3_2020)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(KBS_3_2020)[names(KBS_3_2020)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(KBS_3_2020)[names(KBS_3_2020)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(KBS_3_2020)[names(KBS_3_2020)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(KBS_3_2020)[names(KBS_3_2020)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_3_2020)

#Convert to POSIXct date
KBS_3_1516$Date_Time <- as.POSIXct(KBS_3_1516$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_3_2017$Date_Time <- as.POSIXct(KBS_3_2017$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_3_2018$Date_Time <- as.POSIXct(KBS_3_2018$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_3_2019$Date_Time <- as.POSIXct(KBS_3_2019$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
KBS_3_2020$Date_Time <- as.POSIXct(KBS_3_2020$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

#Convert C to F for 2020
KBS_3_2020$XH_warmed_air_1m <- celsius.to.fahrenheit(KBS_3_2020$XH_warmed_air_1m)
KBS_3_2020$XH_ambient_air_1m <- celsius.to.fahrenheit(KBS_3_2020$XH_ambient_air_1m)

# #check any columns with NA values, if present
# #check to see if all days contain 24h. (only first and last days not expected to have 24h.)
#DateTime3 <- as.POSIXlt(KBS_3$Date_Time, format = "%m/%d/%y %H:%M")
#str(DateTime3)
#DateTime3_hours <- as.numeric(format(DateTime3, "%H"))
#str(DateTime3_hours)
#DateTime3_days <- format(DateTime3, "%m%d%y")
#str(DateTime3_days)
#testing3 <- tapply(DateTime3_hours, as.factor(DateTime3_days), length)
#subset(testing3, testing3<24)
#subset(testing3, testing3>24)

#Create csv files for each year
write.csv(KBS_3_1516, file="L0/KBS/sensor_data/2015_2016/KBS_3_clean.csv")
write.csv(KBS_3_2017, file="L0/KBS/sensor_data/2017/KBS_3.csv")
write.csv(KBS_3_2018, file="L0/KBS/sensor_data/2018/KBS_3.csv")
write.csv(KBS_3_2019, file="L0/KBS/sensor_data/2019/KBS_3.csv")
write.csv(KBS_3_2020, file="L0/KBS/sensor_data/2020/KBS_3.csv")

#Merge data from all years
diff1617_3k <- anti_join(KBS_3_2017, KBS_3_1516, by = "Date_Time")
diff1718_3k <- anti_join(KBS_3_2018, KBS_3_2017, by = "Date_Time")
diff1819_3k <- anti_join(KBS_3_2019, KBS_3_2018, by = "Date_Time")
diff1920_3k <- anti_join(KBS_3_2020, KBS_3_2019, by = "Date_Time")

KBS_3 <- rbind(KBS_3_1516, diff1617_3k, diff1718_3k, diff1819_3k, diff1920_3k)
write.csv(KBS_3, file="L0/KBS/sensor_data/KBS_3.csv")

#Combine data from all years and all paired stations for KBS
KBS_allyears <- rbind(KBS_1, KBS_2, KBS_3)
write.csv(KBS_all, file="L0/KBS/sensor_data/KBS_allyears")



#######################################################################
#    UMBS
#######################################################################
# KS: Relaunched HOBO loggers on 6-25-18 for U
# KD: Relaunched HOBO loggers on 5-13-20 for U

##UMBS Pair 1
#Read in H
UMBS_1_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_1.csv")

UMBS_1H_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_1H_08152017.csv")
summary(UMBS_1H_2017)

UMBS_1H_2018 <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_1H_09192018.csv", skip=1)
summary(UMBS_1H_2018)

UMBS_1H_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_1H_09102019.csv", skip=1)

UMBS_1H_2020 <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_1H_06122020.csv", skip=1)

#Read in U
UMBS_1U_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_1U_08152017.csv")
summary(UMBS_1U_2017)

UMBS_1U_2018a <- read.csv("L0/UMBS/sensor_data/2018/06_25_2018/UMBS_1U_06252018.csv", skip=1)
UMBS_1U_2018b <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_1U_09192018.csv", skip=1)
UMBS_1U_2018 <- rbind(UMBS_1U_2018a, UMBS_1U_2018b)
summary(UMBS_1U_2018)

UMBS_1U_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_1U_09102019.csv", skip=1)[ ,1:6]

UMBS_1U_2020a <- read.csv("L0/UMBS/sensor_data/2020/05_13_2020/UMBS_1U_05132020.csv", skip=1)[ ,1:6]
UMBS_1U_2020b <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_1U_06122020.csv", skip=1)[ ,1:6]
names(UMBS_1U_2020a)[names(UMBS_1U_2020a)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_1U_2020a)[names(UMBS_1U_2020a)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_1U_2020a)[names(UMBS_1U_2020a)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_1U_2020a)[names(UMBS_1U_2020a)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_1U_2020b)[names(UMBS_1U_2020b)=="Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_1U_2020b)[names(UMBS_1U_2020b)=="Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_1U_2020b)[names(UMBS_1U_2020b)=="Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_1U_2020b)[names(UMBS_1U_2020b)=="Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
UMBS_1U_2020 <- rbind(UMBS_1U_2020a, UMBS_1U_2020b)

#Merge data for each year
UMBS_1_2017 <- merge(UMBS_1H_2017, UMBS_1U_2017, by="Date_Time", all.x=T, all.y=T)
str(UMBS_1_2017)
summary(UMBS_1_2017)
UMBS_1_2018 <- merge(UMBS_1H_2018, UMBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(UMBS_1_2018)
summary(UMBS_1_2018)
UMBS_1_2019 <- merge(UMBS_1H_2019, UMBS_1U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_1_2020 <- merge(UMBS_1H_2020, UMBS_1U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

# rename columns to match manually curated data
UMBS_1_1516$X<-NULL
UMBS_1_2017$X..x <- NULL
UMBS_1_2017$X..y<- NULL
UMBS_1_2018$X..x<-NULL
UMBS_1_2018$X..y<-NULL
UMBS_1_2019$X..x<-NULL
UMBS_1_2019$X..y<-NULL
UMBS_1_2020$X..x<-NULL
UMBS_1_2020$X..y<-NULL

names(UMBS_1_1516)[names(UMBS_1_1516)=="X1U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(UMBS_1_1516)[names(UMBS_1_1516)=="X1U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(UMBS_1_1516)

names(UMBS_1_2017)[names(UMBS_1_2017)=="X1H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(UMBS_1_2017)[names(UMBS_1_2017)=="X1U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
head(UMBS_1_2017)

names(UMBS_1_2018)[names(UMBS_1_2018)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736057..LBL..1H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736059..LBL..1H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="RH.....LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="RH.....LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_1_2018)

names(UMBS_1_2019)[names(UMBS_1_2019)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736057..LBL..1H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736059..LBL..1H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Temp...F..LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(UMBS_1_2019)[names(UMBS_1_2019)=="RH.....LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Temp...F..LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_1_2019)[names(UMBS_1_2019)=="RH.....LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_1_2019)[names(UMBS_1_2019)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_1_2019)

names(UMBS_1_2020)[names(UMBS_1_2020)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736057..LBL..1H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_1_2020)[names(UMBS_1_2020)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736059..LBL..1H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_1_2020)[names(UMBS_1_2020)=="Temp...C..LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(UMBS_1_2020)[names(UMBS_1_2020)=="RH.....LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_1_2020)[names(UMBS_1_2020)=="Temp...C..LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_1_2020)[names(UMBS_1_2020)=="RH.....LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_1_2020)[names(UMBS_1_2020)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_1_2020)

# #check any columns with NA values, if present
# #check to see if all days contain 24h.(only first and last days not expected to have 24h.)
# DateTime1 <- as.POSIXlt(UMBS_1$Date_Time, format = "%m/%d/%y %H:%M")
# str(DateTime1)
# DateTime1_hours <- as.numeric(format(DateTime1, "%H"))
# str(DateTime1_hours)
# DateTime1_days <- format(DateTime1, "%m%d%y")
# str(DateTime1_days)
# testing1 <- tapply(DateTime1_hours, as.factor(DateTime1_days), length)
# subset(testing1, testing1<24)
# subset(testing1, testing1>24)

#Convert to POSIXct date
UMBS_1_1516$Date_Time <- as.POSIXct(UMBS_1_1516$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_1_2017$Date_Time <- as.POSIXct(UMBS_1_2017$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_1_2018$Date_Time <- as.POSIXct(UMBS_1_2018$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_1_2019$Date_Time <- as.POSIXct(UMBS_1_2019$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_1_2020$Date_Time <- as.POSIXct(UMBS_1_2020$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

#Create csv files for each year
write.csv(UMBS_1_1516, file="L0/UMBS/sensor_data/2015_2016/UMBS_1_clean.csv")
write.csv(UMBS_1_2017, file="L0/UMBS/sensor_data/2017/UMBS_1.csv")
write.csv(UMBS_1_2018, file="L0/UMBS/sensor_data/2018/UMBS_1.csv")
write.csv(UMBS_1_2019, file="L0/UMBS/sensor_data/2019/UMBS_1.csv")
write.csv(UMBS_1_2020, file="L0/UMBS/sensor_data/2020/UMBS_1.csv")

#Merge data from all years
diff1617_1u <- anti_join(UMBS_1_2017, UMBS_1_1516, by = "Date_Time")
diff1718_1u <- anti_join(UMBS_1_2018, UMBS_1_2017, by = "Date_Time")
diff1819_1u <- anti_join(UMBS_1_2019, UMBS_1_2018, by = "Date_Time")
diff1920_1u <- anti_join(UMBS_1_2020, UMBS_1_2019, by = "Date_Time")

UMBS_1 <- rbind(UMBS_1_1516, diff1617_1u, diff1718_1u, diff1819_1u, diff1920_1u)
write.csv(UMBS_1, file="L0/UMBS/sensor_data/UMBS_1.csv")

############## UMBS Pair 2
#Read in H
UMBS_2_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_2.csv")

UMBS_2H_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_2H_08152017.csv")
str(UMBS_2H_2017)
summary(UMBS_2H_2017)

UMBS_2H_2018 <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_2H_09192018.csv", skip=1)
str(UMBS_2H_2018)
summary(UMBS_2H_2018)

UMBS_2H_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_2H_09102019.csv", skip=1)

UMBS_2H_2020 <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_2H_06122020.csv", skip=1)

#Read in U
UMBS_2U_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_2U_08152017.csv")
tail(UMBS_2U_2017)
str(UMBS_2U_2017)
summary(UMBS_2U_2017)

UMBS_2U_2018a <- read.csv("L0/UMBS/sensor_data/2018/06_25_2018/UMBS_2U_06252018.csv", skip=1)
UMBS_2U_2018b <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_2U_09192018.csv", skip=1)
UMBS_2U_2018 <- rbind(UMBS_2U_2018a, UMBS_2U_2018b)
str(UMBS_2U_2018)
summary(UMBS_2U_2018)

UMBS_2U_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_2U_09102019.csv", skip = 1)[ ,1:6]

UMBS_2U_2020a <- read.csv("L0/UMBS/sensor_data/2020/05_13_2020/UMBS_2U_05132020.csv", skip=1)[ ,1:6]
UMBS_2U_2020b <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_2U_06122020.csv", skip=1)[ ,1:6]
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
UMBS_2U_2020 <- rbind(UMBS_2U_2020a, UMBS_2U_2020b)

#Merge the data
UMBS_2_2017 <- merge(UMBS_2H_2017, UMBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
str(UMBS_2_2017)
summary(UMBS_2_2017)
UMBS_2_2018 <- merge(UMBS_2H_2018, UMBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(UMBS_2_2018)
summary(UMBS_2_2018)
UMBS_2_2019 <- merge(UMBS_2H_2019, UMBS_2U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_2_2020 <- merge(UMBS_2H_2020, UMBS_2U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

# rename columns to match manually curated data
UMBS_2_1516$x<-NULL
UMBS_2_2017$X..x <- NULL
UMBS_2_2017$X..y<- NULL
UMBS_2_2018$X..x<-NULL
UMBS_2_2018$X..y<-NULL
UMBS_2_2019$X..x<-NULL
UMBS_2_2019$X..y<-NULL
UMBS_2_2020$X..x<-NULL
UMBS_2_2020$X..y<-NULL

names(UMBS_2_1516)[names(UMBS_2_1516)=="X2U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(UMBS_2_1516)[names(UMBS_2_1516)=="X2U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(UMBS_2_1516)

names(UMBS_2_2017)[names(UMBS_2_2017)=="X2H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2H_ambient_aim_1m"] <- "XH_ambient_air_1m"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(UMBS_2_2017)[names(UMBS_2_2017)=="X2U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
head(UMBS_2_2017)

names(UMBS_2_2018)[names(UMBS_2_2018)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736052..LBL..2H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736056..LBL..2H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="RH.....LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="RH.....LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_2_2018)

names(UMBS_2_2019)[names(UMBS_2_2019)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736052..LBL..2H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736056..LBL..2H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_2_2019)[names(UMBS_2_2019)=="RH.....LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"
names(UMBS_2_2019)[names(UMBS_2_2019)=="RH.....LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_2_2019)[names(UMBS_2_2019)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_2_2019)

names(UMBS_2_2020)[names(UMBS_2_2020)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736052..LBL..2H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_2_2020)[names(UMBS_2_2020)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736056..LBL..2H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_2_2020)[names(UMBS_2_2020)=="Temp...C..LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_2_2020)[names(UMBS_2_2020)=="RH.....LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_2_2020)[names(UMBS_2_2020)=="Temp...C..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"
names(UMBS_2_2020)[names(UMBS_2_2020)=="RH.....LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_2_2020)[names(UMBS_2_2020)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_2_2020)

#check any columns with NA values, if present
# UMBS_2[-which(is.na(UMBS_2$X2U_ambient_air_10cm)),1]
#2U was not logging from 7/28/2015 19:: thorugh 11/24/2015, when it was launched again.

# #check to see if all days contain 24h.(only first and last days not expected to have 24h.)
# DateTime2 <- as.POSIXlt(UMBS_2$Date_Time, format = "%m/%d/%y %H:%M")
# str(DateTime2)
# DateTime2_hours <- as.numeric(format(DateTime2, "%H"))
# str(DateTime2_hours)
# DateTime2_days <- format(DateTime2, "%m%d%y")
# str(DateTime2_days)
# testing2 <- tapply(DateTime2_hours, as.factor(DateTime2_days), length)
# subset(testing2, testing2<24) #09/26/2015 10:00 missing, likely because data was being downloaded/maintenance.
# subset(testing2, testing2>24)

#Convert to POSIX format
UMBS_2_1516$Date_Time <- as.POSIXct(UMBS_2_1516$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_2_2017$Date_Time <- as.POSIXct(UMBS_2_2017$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_2_2018$Date_Time <- as.POSIXct(UMBS_2_2018$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_2_2019$Date_Time <- as.POSIXct(UMBS_2_2019$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_2_2020$Date_Time <- as.POSIXct(UMBS_2_2020$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

#Merge data from all years
diff1617_2u <- anti_join(UMBS_2_2017, UMBS_2_1516, by = "Date_Time")
diff1718_2u <- anti_join(UMBS_2_2018, UMBS_2_2017, by = "Date_Time")
diff1819_2u <- anti_join(UMBS_2_2019, UMBS_2_2018, by = "Date_Time")
diff1920_2u <- anti_join(UMBS_2_2020, UMBS_2_2019, by = "Date_Time")

UMBS_2 <- rbind(UMBS_2_1516, diff1617_2u, diff1718_2u, diff1819_2u, diff1920_2u)
write.csv(UMBS_2, file="L0/UMBS/sensor_data/UMBS_2.csv")

############# UMBS Pair 3
#Read in H
UMBS_3_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_3.csv")
UMBS_3H_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_3H_08152017.csv")
str(UMBS_3H_2017)
summary(UMBS_3H_2017)

UMBS_3H_2018 <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_3H_09192018.csv", skip=1)
str(UMBS_3H_2018)
summary(UMBS_3H_2018)

UMBS_3H_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_3H_09102019.csv", skip=1)

UMBS_3H_2020 <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_3H_06122020.csv", skip=1)

#Read in U
UMBS_3U_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_3U_08152017.csv")
tail(UMBS_3U_2017)
str(UMBS_3U_2017)
summary(UMBS_3U_2017)

UMBS_3U_2018a <- read.csv("L0/UMBS/sensor_data/2018/06_25_2018/UMBS_3U_06252018.csv", skip=1)
UMBS_3U_2018b <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_3U_09192018.csv", skip=1)
UMBS_3U_2018 <- rbind(UMBS_3U_2018a, UMBS_3U_2018b)
str(UMBS_3U_2018)
summary(UMBS_3U_2018)

UMBS_3U_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_3U_09102019.csv", skip = 1)[ ,1:6]

UMBS_3U_2020a <- read.csv("L0/UMBS/sensor_data/2020/05_13_2020/UMBS_3U_05132020.csv", skip=1)[ ,1:6]
UMBS_3U_2020b <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_3U_06122020.csv", skip=1)[ ,1:6]
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
UMBS_3U_2020 <- rbind(UMBS_3U_2020a, UMBS_3U_2020b)

#Merge the data
UMBS_3_2017 <- merge(UMBS_3H_2017, UMBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
str(UMBS_3_2017)
summary(UMBS_3_2017)
UMBS_3_2018 <- merge(UMBS_3H_2018, UMBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(UMBS_3_2018)
summary(UMBS_3_2018)
UMBS_3_2019 <- merge(UMBS_3H_2019, UMBS_3U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_3_2020 <- merge(UMBS_3H_2020, UMBS_3U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

# rename columns to match manually curated data
UMBS_3_1516$X<-NULL
UMBS_3_2017$X..x <- NULL
UMBS_3_2017$X..y<- NULL
UMBS_3_2018$X..x<-NULL
UMBS_3_2018$X..y<-NULL
UMBS_3_2019$X..x<-NULL
UMBS_3_2019$X..y<-NULL
UMBS_3_2020$X..x<-NULL
UMBS_3_2020$X..y<-NULL

names(UMBS_3_1516)[names(UMBS_3_1516)=="X3U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(UMBS_3_1516)[names(UMBS_3_1516)=="X3U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
head(UMBS_3_1516)

names(UMBS_3_2017)[names(UMBS_3_2017)=="X3H_ambient_soil_moisture_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3H_warmed_soil_moisture_5cm"] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3H_ambient_air_1m"] <- "XH_ambient_air_1m"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3H_ambient_RH_1m"] <- "XH_ambient_RH_1m"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3H_warmed_air_1m"] <- "XH_warmed_air_1m"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3H_warmed_RH_1m"] <- "XH_warmed_RH_1m"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3U_ambient_soil_5cm"] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3U_ambient_air_10cm"] <- "XU_ambient_air_10cm"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3U_warmed_air_10cm"] <- "XU_warmed_air_10cm"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3U_warmed_soil_5cm"] <- "XU_warmed_soil_temp_5cm"
head(UMBS_3_2017)

names(UMBS_3_2018)[names(UMBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736055..LBL..3H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736058..LBL..3H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="RH.....LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="RH.....LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_3_2018)

names(UMBS_3_2019)[names(UMBS_3_2019)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736055..LBL..3H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736058..LBL..3H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Temp...F..LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_3_2019)[names(UMBS_3_2019)=="RH.....LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Temp...F..LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(UMBS_3_2019)[names(UMBS_3_2019)=="RH.....LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_3_2019)[names(UMBS_3_2019)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_3_2019)

names(UMBS_3_2020)[names(UMBS_3_2020)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736055..LBL..3H_warmed_soil_moisture_5cm."] <- "XH_warmed_soil_moisture_5cm"
names(UMBS_3_2020)[names(UMBS_3_2020)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736058..LBL..3H_ambient_soil_moisture_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(UMBS_3_2020)[names(UMBS_3_2020)=="Temp...C..LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_air_1m."] <- "XH_warmed_air_1m"
names(UMBS_3_2020)[names(UMBS_3_2020)=="RH.....LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_RH_1m."] <- "XH_warmed_RH_1m"
names(UMBS_3_2020)[names(UMBS_3_2020)=="Temp...C..LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_air_1m."] <- "XH_ambient_air_1m"
names(UMBS_3_2020)[names(UMBS_3_2020)=="RH.....LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_RH_1m."] <- "XH_ambient_RH_1m"
names(UMBS_3_2020)[names(UMBS_3_2020)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_3_2020)

# #####NOTE: Columns atr labelled X1H (e.g.) rather than X3H. 
# #check any columns with NA values, if present
# which(is.na(UMBS_3$X3U_ambient_air_10cm))
# UMBS_3[1132,] #11/24/15 11:00 No U12 readings.  Probably dpwloaded 1h earlier.
# UMBS_3[1309,] #6/25/15 17:00 No U12 readings.  Probably launched 1h later. 
# #check to see if all days contain 24h. (only first and last days not expected to have 24h.)
# DateTime3 <- as.POSIXlt(UMBS_3$Date_Time, format = "%m/%d/%y %H:%M")
# str(DateTime3)
# DateTime3_hours <- as.numeric(format(DateTime3, "%H"))
# str(DateTime3_hours)
# DateTime3_days <- format(DateTime3, "%m%d%y")
# str(DateTime3_days)
# testing3 <- tapply(DateTime3_hours, as.factor(DateTime3_days), length)
# subset(testing3, testing3<24)
# subset(testing3, testing3>24)

#Convert to POSIX format
UMBS_3_1516$Date_Time <- as.POSIXct(UMBS_3_1516$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_3_2017$Date_Time <- as.POSIXct(UMBS_3_2017$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_3_2018$Date_Time <- as.POSIXct(UMBS_3_2018$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_3_2019$Date_Time <- as.POSIXct(UMBS_3_2019$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")
UMBS_3_2020$Date_Time <- as.POSIXct(UMBS_3_2020$Date_Time,format="%m/%d/%y %I:%M:%S %p", tz="UTC")

#Merge data from all years
diff1617_3u <- anti_join(UMBS_3_2017, UMBS_3_1516, by = "Date_Time")
diff1718_3u <- anti_join(UMBS_3_2018, UMBS_3_2017, by = "Date_Time")
diff1819_3u <- anti_join(UMBS_3_2019, UMBS_3_2018, by = "Date_Time")
diff1920_3u <- anti_join(UMBS_3_2020, UMBS_3_2019, by = "Date_Time")

UMBS_3 <- rbind(UMBS_3_1516, diff1617_3u, diff1718_3u, diff1819_3u, diff1920_3u)
write.csv(UMBS_3, file="L0/UMBS/sensor_data/UMBS_3.csv")



### remove this section once the POSIX conversion is figured out for 1516 data

####### Create a new merged file that contains data from all years for the both KBS and UMBS pairs.
# write the merged file in the sensor_data folder #######

#UMBS pair 1
UMBS_1 <- read.csv("L0/UMBS/sensor_data/UMBS_1.csv")
UMBS_1_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_1.csv")
Merged_UMBS_1<-rbind(UMBS_1_s18,UMBS_1_s17,UMBS_1_s15_16)
dim(Merged_UMBS_1)
head(Merged_UMBS_1)
new.Merged_UMBS_1<-distinct(Merged_UMBS_1)
new.Merged_UMBS_1<- unique(Merged_UMBS_1) #function not working, clear duplicated in head(Merged_UMBS_1)
dim(new.Merged_UMBS_1)
write.csv(Merged_UMBS_1, file="final_data/UMBS/sensor_data/Merged_UMBS_1.csv")

#UMBS pair 2
UMBS_2_s18 <- read.csv("L0/UMBS/sensor_data/2018/UMBS_2.csv")
UMBS_2_s17 <- read.csv("final_data/UMBS/sensor_data/2017/UMBS_2.csv")
UMBS_2_s15_16 <- read.csv("final_data/UMBS/sensor_data/2015_2016/UMBS_2.csv")
Merged_UMBS_2<-rbind(UMBS_2_s18,UMBS_2_s17,UMBS_2_s15_16)
dim(Merged_UMBS_2)
write.csv(Merged_UMBS_2, file="final_data/UMBS/sensor_data/Merged_UMBS_2.csv")

#UMBS pair 3
UMBS_3_s18 <- read.csv("final_data/UMBS/sensor_data/2018/UMBS_3.csv")
UMBS_3_s17 <- read.csv("final_data/UMBS/sensor_data/2017/UMBS_3.csv")
UMBS_3_s15_16 <- read.csv("final_data/UMBS/sensor_data/2015_2016/UMBS_3.csv")
# edit column names "X3U_warmed_soil_5cm" and "X3U_ambient_soil_5cm"
names(UMBS_3_s15_16)[names(UMBS_3_s15_16)=="X3U_warmed_soil_5cm"] <- "X3U_warmed_soil_temp_5cm"
names(UMBS_3_s15_16)[names(UMBS_3_s15_16)=="X3U_ambient_soil_5cm"] <- "X3U_ambient_soil_temp_5cm"
Merged_UMBS_3<-rbind(UMBS_3_s18,UMBS_3_s17,UMBS_3_s15_16)
head(UMBS_3_s15_16)
dim(Merged_UMBS_3)
write.csv(Merged_UMBS_3, file="final_data/UMBS/sensor_data/Merged_UMBS_3.csv")

#KBS pair 1
KBS_1_s18 <- read.csv("final_data/KBS/sensor_data/2018/KBS_1.csv")
KBS_1_s17 <- read.csv("final_data/KBS/sensor_data/2017/KBS_1.csv")
KBS_1_s15_16 <- read.csv("final_data/KBS/sensor_data/2015_2016/KBS_1.csv")
# edit column names "X1U_warmed_soil_5cm" and "X1U_ambient_soil_5cm"
names(KBS_1_s15_16)[names(KBS_1_s15_16)=="X1U_warmed_soil_5cm"] <- "X1U_warmed_soil_temp_5cm"
names(KBS_1_s15_16)[names(KBS_1_s15_16)=="X1U_ambient_soil_5cm"] <- "X1U_ambient_soil_temp_5cm"
Merged_KBS_1<-rbind(KBS_1_s18,KBS_1_s17,KBS_1_s15_16)
dim(Merged_KBS_1)
write.csv(Merged_KBS_1, file="final_data/KBS/sensor_data/Merged_KBS_1.csv")

#KBS pair 2
KBS_2_s18 <- read.csv("final_data/KBS/sensor_data/2018/KBS_2.csv")
KBS_2_s17 <- read.csv("final_data/KBS/sensor_data/2017/KBS_2.csv")
KBS_2_s15_16 <- read.csv("final_data/KBS/sensor_data/2015_2016/KBS_2.csv")
# edit column names "X2U_warmed_soil_5cm", "X2U_ambient_soil_5cm", and "X2H_ambient_soil_moist_5cm"
names(KBS_2_s15_16)[names(KBS_2_s15_16)=="X2U_warmed_soil_5cm"] <- "X2U_warmed_soil_temp_5cm"
names(KBS_2_s15_16)[names(KBS_2_s15_16)=="X2U_ambient_soil_5cm"] <- "X2U_ambient_soil_temp_5cm"
Merged_KBS_2<-rbind(KBS_2_s18,KBS_2_s17,KBS_2_s15_16)
head(KBS_2_s17)
dim(Merged_KBS_2)
write.csv(Merged_KBS_2, file="final_data/KBS/sensor_data/Merged_KBS_2.csv")

#KBS pair 3
KBS_3_s18 <- read.csv("final_data/KBS/sensor_data/2018/KBS_3.csv")
KBS_3_s17 <- read.csv("final_data/KBS/sensor_data/2017/KBS_3.csv")
KBS_3_s15_16 <- read.csv("final_data/KBS/sensor_data/2015_2016/KBS_3.csv")
# edit column names "X3U_warmed_soil_5cm", and "X3U_ambient_soil_5cm"
names(KBS_3_s15_16)[names(KBS_3_s15_16)=="X3U_warmed_soil_5cm"] <- "X3U_warmed_soil_temp_5cm"
names(KBS_3_s15_16)[names(KBS_3_s15_16)=="X3U_ambient_soil_5cm"] <- "X3U_ambient_soil_temp_5cm"
Merged_KBS_3<-rbind(KBS_3_s18,KBS_3_s17,KBS_3_s15_16)
dim(Merged_KBS_3)
write.csv(Merged_KBS_3, file="final_data/KBS/sensor_data/Merged_KBS_3.csv")
###############################################################################