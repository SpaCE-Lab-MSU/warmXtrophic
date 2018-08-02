##################    prep HOBO DATA LOGGERS    ###############################
#This script reads in HOBO data from raw_data, combines the data from the U21 and the H12 units for each pair, and writes a CSV file to the final_data folder.  
#The KBS_sensors and UMBS_sensors scripts then take that data and summarize/analyze how the chambers affect the abiotic environment. 
# original script created by NKL & KBW 2016-2017
# KS edit May 23, 2018: created merged files for UMBS; August 1, 2018: remove manual preparation step and add 2018 data from KBS and UMBS

rm(list=ls())

#read in the data (change the path to navigate to the MIGrass directory on your computer).
#setwd("/Users/Nina/Dropbox/MI_GRASS/")
setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/")

############ KBS Pair 1
KBS_1H_2017 <- read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/KBS_1H.csv")
str(KBS_1H_2017)
summary(KBS_1H_2017)
KBS_1H_2018 <- read.csv("raw_data/KBS/sensor_data/2018/excel files from HOBOWARE/1H_0_071218.csv", skip=1)
str(KBS_1H_2018)
summary(KBS_1H_2018)

KBS_1U_2017 <- read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/KBS_1U.csv")
str(KBS_1U_2017)
summary(KBS_1U_2017)
KBS_1U_2018 <- read.csv("raw_data/KBS/sensor_data/2018/excel files from HOBOWARE/1U_0_071218.csv",skip=1)[ ,1:6]
head(KBS_1U_2018)
str(KBS_1U_2017)
summary(KBS_1U_2017)

KBS_1_2017 <- merge(KBS_1H_2017, KBS_1U_2017, by="Date_Time", all.x=T, all.y=T)
str(KBS_1_2017)
summary(KBS_1_2017)
KBS_1_2018 <- merge(KBS_1H_2018, KBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_1_2018)
summary(KBS_1_2018)

# rename columns to match manually curated data
KBS_1_2018$X..x<-NULL
KBS_1_2018$X..y<-NULL
KBS_1_2017[2] <- NULL # "X..x"
KBS_1_2017[8]<- NULL # "X..y"
names(KBS_1_2017)[names(KBS_1_2017)=="X1U_warmed_soil_5cm"] <- "X1U_warmed_soil_temp_5cm"
names(KBS_1_2017)[names(KBS_1_2017)=="X1U_ambient_soil_5cm"] <- "X1U_ambient_soil_temp_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736060..LBL..1H_warmed_soil_moisture_5cm."] <- "X1H_warmed_soil_moisture_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Water.Content..m..m...LGR.S.N..10736963..SEN.S.N..10736062..LBL..1H_ambient_soil_moisture_5cm."] <- "X1H_ambient_soil_moisture_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_air_1m."] <- "X1H_warmed_air_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="RH.....LGR.S.N..10736963..SEN.S.N..10737463..LBL..1H_warmed_RH_1m."] <- "X1H_warmed_RH_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_air_1m."] <- "X1H_ambient_air_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="RH.....LGR.S.N..10736963..SEN.S.N..10737464..LBL..1H_ambient_RH_1m."] <- "X1H_ambient_RH_1m"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_air_10cm."] <- "X1U_warmed_air_10cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_warmed_soil_5cm."] <- "X1U_warmed_soil_temp_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_soil_5cm."] <- "X1U_ambient_soil_temp_5cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Temp...F..LGR.S.N..10737622..SEN.S.N..10737622..LBL..1U_ambient_air_10cm."] <- "X1U_ambient_air_10cm"
names(KBS_1_2018)[names(KBS_1_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_1_2018)

#check any columns with NA values, if present
#check to see if all days contain 24h.(only first and last days not expected to have 24h.)
DateTime1 <- as.POSIXlt(KBS_1$Date_Time, format = "%m/%d/%y %H:%M")
str(DateTime1)
DateTime1_hours <- as.numeric(format(DateTime1, "%H"))
str(DateTime1_hours)
DateTime1_days <- format(DateTime1, "%m%d%y")
str(DateTime1_days)
testing1 <- tapply(DateTime1_hours, as.factor(DateTime1_days), length)
subset(testing1, testing1<24)
subset(testing1, testing1>24)

write.csv(KBS_1_2017, file="final_data/KBS/sensor_data/2017/KBS_1.csv")
write.csv(KBS_1_2018, file="final_data/KBS/sensor_data/2018/KBS_1.csv")

############## KBS Pair 2
KBS_2H_2017 <- read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/KBS_2H.csv")
str(KBS_2H_2017)
summary(KBS_2H_2017)
KBS_2H_2018 <- read.csv("raw_data/KBS/sensor_data/2018/excel files from HOBOWARE/2H_0_071218.csv", skip=1)
str(KBS_2H_2018)
summary(KBS_2H_2018)

KBS_2U_2017 <- read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/KBS_2U.csv")
str(KBS_2U_2017)
summary(KBS_2U_2017)
KBS_2U_2018 <- read.csv("raw_data/KBS/sensor_data/2018/excel files from HOBOWARE/2U_0_071218.csv",skip=1)[ ,1:6]
str(KBS_2U_2018)
summary(KBS_2U_2018)

KBS_2_2017 <- merge(KBS_2H_2017, KBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
str(KBS_2_2017)
summary(KBS_2_2017)
KBS_2_2018 <- merge(KBS_2H_2018, KBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_2_2018)
summary(KBS_2_2018)
head(KBS_2_2018)

# rename columns to match manually curated data
KBS_2_2018$X..x<-NULL
KBS_2_2018$X..y<-NULL
KBS_2_2017[2] <- NULL # "X..x"
KBS_2_2017[8]<- NULL # "X..y"
names(KBS_2_2017)[names(KBS_2_2017)=="X2U_warmed_soil_5cm"] <- "X2U_warmed_soil_temp_5cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2U_ambient_soil_5cm"] <- "X2U_ambient_soil_temp_5cm"
names(KBS_2_2017)[names(KBS_2_2017)=="X2H_ambient_soil_moist_5cm"] <- "X2H_ambient_soil_moisture_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "X2H_ambient_soil_moisture_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736063..LBL..2H_warmed_soil_moisture_5cm."] <- "X2H_warmed_soil_moisture_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_air_1m."] <- "X2H_ambient_air_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="RH.....LGR.S.N..10736967..SEN.S.N..10737465..LBL..2H_ambient_RH_1m."] <- "X2H_ambient_RH_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_air_1m."] <- "X2H_warmed_air_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="RH.....LGR.S.N..10736967..SEN.S.N..10737467..LBL..2H_warmed_RH_1m."] <- "X2H_warmed_RH_1m"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_soil_5cm."] <- "X2U_ambient_soil_temp_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_ambient_air_10cm."] <- "X2U_ambient_air_10cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_air_10cm."] <- "X2U_warmed_air_10cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Temp...F..LGR.S.N..10737623..SEN.S.N..10737623..LBL..2U_warmed_soil_5cm."] <- "X2U_warmed_soil_temp_5cm"
names(KBS_2_2018)[names(KBS_2_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_2_2018)

#check any columns with NA values, if present
which(is.na(KBS_2$X2H_ambient_air_1m))
KBS_2[2538,]
KBS_2[2634,]#2H not logged data 2015-06-15 17:00 through 2015-06-19 09:00.  Will not be a problem becasue it gets subsetted out later.
which(is.na(KBS_2$X2U_ambient_air_10cm))
KBS_2[51,]
#check to see if all days contain 24h.(only first and last days not expected to have 24h.)
DateTime2 <- as.POSIXlt(KBS_2$Date_Time, format = "%m/%d/%y %H:%M")
str(DateTime2)
DateTime2_hours <- as.numeric(format(DateTime2, "%H"))
str(DateTime2_hours)
DateTime2_days <- format(DateTime2, "%m%d%y")
str(DateTime2_days)
testing2 <- tapply(DateTime2_hours, as.factor(DateTime2_days), length)
subset(testing2, testing2<24)
subset(testing2, testing2>24)

write.csv(KBS_2_2017, file="final_data/KBS/sensor_data/2017/KBS_2.csv")
write.csv(KBS_2_2018, file="final_data/KBS/sensor_data/2018/KBS_2.csv")

############# KBS Pair 3
KBS_3H_2017 <- read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/KBS_3H.csv")
str(KBS_3H_2017)
summary(KBS_3H_2017)
KBS_3H_2018 <- read.csv("raw_data/KBS/sensor_data/2018/excel files from HOBOWARE/3H_0_071218.csv", skip=1)
str(KBS_3H_2018)
summary(KBS_3H_2018)

KBS_3U_2017 <- read.csv("raw_data/KBS/sensor_data/2017/9_1_2017/KBS_3U.csv")
str(KBS_3U_2017)
summary(KBS_3U_2017)
KBS_3U_2018 <- read.csv("raw_data/KBS/sensor_data/2018/excel files from HOBOWARE/3U_0_071218.csv",skip=1)[ ,1:6]
str(KBS_3U_2018)
summary(KBS_3U_2018)

KBS_3_2017 <- merge(KBS_3H_2017, KBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
str(KBS_3_2017)
summary(KBS_3_2017)
KBS_3_2018 <- merge(KBS_3H_2018, KBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(KBS_3_2018)
summary(KBS_3_2018)
head(KBS_3_2018)

# rename columns to match manually curated data
KBS_3_2018$X..x<-NULL
KBS_3_2018$X..y<-NULL
KBS_3_2017[2] <- NULL # "X..x"
KBS_3_2017[8]<- NULL # "X..y"
names(KBS_3_2017)[names(KBS_3_2017)=="X3H_ambient_soil_moistire_5cm"] <- "X3H_ambient_soil_moisture_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736053..LBL..3H_warmed_soil_moisture_5cm."] <- "X3H_warmed_soil_moisture_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "X3H_ambient_soil_moisture_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_air_1m."] <- "X3H_warmed_air_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="RH.....LGR.S.N..10736968..SEN.S.N..10737466..LBL..3H_warmed_RH_1m."] <- "X3H_warmed_RH_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_air_1m."] <- "X3H_ambient_air_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="RH.....LGR.S.N..10736968..SEN.S.N..10737468..LBL..3H_ambient_RH_1m."] <- "X3H_ambient_RH_1m"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "X3U_warmed_soil_temp_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_air_10cm."] <- "X3U_warmed_air_10cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_air_10cm."] <- "X3U_ambient_air_10cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "X3U_ambient_soil_temp_5cm"
names(KBS_3_2018)[names(KBS_3_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(KBS_3_2018)

#check any columns with NA values, if present
#check to see if all days contain 24h. (only first and last days not expected to have 24h.)
DateTime3 <- as.POSIXlt(KBS_3$Date_Time, format = "%m/%d/%y %H:%M")
str(DateTime3)
DateTime3_hours <- as.numeric(format(DateTime3, "%H"))
str(DateTime3_hours)
DateTime3_days <- format(DateTime3, "%m%d%y")
str(DateTime3_days)
testing3 <- tapply(DateTime3_hours, as.factor(DateTime3_days), length)
subset(testing3, testing3<24)
subset(testing3, testing3>24)

write.csv(KBS_3_2017, file="final_data/KBS/sensor_data/2017/KBS_3.csv")
write.csv(KBS_3_2018, file="final_data/KBS/sensor_data/2018/KBS_3.csv")

#######################################################################
#    UMBS
#######################################################################
# KS: Relaunched HOBO loggers on 6-25-18

##UMBS Pair 1
UMBS_1H_2017 <- read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_1H.csv")
str(UMBS_1H_2017)
summary(UMBS_1H_2017)
UMBS_1H_2018a <- read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_1H_1.csv", skip=1)
UMBS_1H_2018b <- read.csv("raw_data/UMBS/sensor_data/2018/7_31_18/UMBS_1H_731.csv", skip=1)
UMBS_1H_2018 <- rbind(UMBS_1H_2018a, UMBS_1H_2018b)
str(UMBS_1H_2018)
summary(UMBS_1H_2018)

UMBS_1U_2017 <- read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_1U.csv")
tail(UMBS_1U_2017)
str(UMBS_1U_2017)
summary(UMBS_1U_2017)
UMBS_1U_2018a <- read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_1U_1.csv", skip=1)
UMBS_1U_2018b <- read.csv("raw_data/UMBS/sensor_data/2018/7_31_18/UMBS_1U_731.csv", skip=1)
UMBS_1U_2018 <- rbind(UMBS_1U_2018a, UMBS_1U_2018b)
str(UMBS_1U_2018)
summary(UMBS_1U_2018)


UMBS_1_2017 <- merge(UMBS_1H_2017, UMBS_1U_2017, by="Date_Time", all.x=T, all.y=T)
str(UMBS_1_2017)
summary(UMBS_1_2017)
UMBS_1_2018 <- merge(UMBS_1H_2018, UMBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(UMBS_1_2018)
summary(UMBS_1_2018)

# rename columns to match manually curated data
UMBS_1_2018$X.<-NULL
UMBS_1_2018$X..x<-NULL
UMBS_1_2018$X..y<-NULL
UMBS_1_2017[2] <- NULL # "X..x"
UMBS_1_2017[8]<- NULL # "X..y"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736057..LBL..1H_ambient_soil_moisture_5cm."] <- "X1H_ambient_soil_moisture_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Water.Content..m..m...LGR.S.N..10956853..SEN.S.N..10736059..LBL..1H_warmed_soil_moisture_5cm."] <- "X1H_warmed_soil_moisture_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_air_1m."] <- "X1H_ambient_air_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="RH.....LGR.S.N..10956853..SEN.S.N..10737457..LBL..1H_ambient_RH_1m."] <- "X1H_ambient_RH_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_air_1m."] <- "X1H_warmed_air_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="RH.....LGR.S.N..10956853..SEN.S.N..10737462..LBL..1H_warmed_RH_1m."] <- "X1H_warmed_RH_1m"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "X1U_ambient_soil_temp_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_air_10cm."] <- "X1U_ambient_air_10cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_air_10cm."] <- "X1U_warmed_air_10cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "X1U_warmed_soil_temp_5cm"
names(UMBS_1_2018)[names(UMBS_1_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_1_2018)

#check any columns with NA values, if present
#check to see if all days contain 24h.(only first and last days not expected to have 24h.)
DateTime1 <- as.POSIXlt(UMBS_1$Date_Time, format = "%m/%d/%y %H:%M")
str(DateTime1)
DateTime1_hours <- as.numeric(format(DateTime1, "%H"))
str(DateTime1_hours)
DateTime1_days <- format(DateTime1, "%m%d%y")
str(DateTime1_days)
testing1 <- tapply(DateTime1_hours, as.factor(DateTime1_days), length)
subset(testing1, testing1<24)
subset(testing1, testing1>24)

write.csv(UMBS_1_2017, file="final_data/UMBS/sensor_data/2017/UMBS_1.csv")
write.csv(UMBS_1_2018, file="final_data/UMBS/sensor_data/2018/UMBS_1.csv")

############## UMBS Pair 2
UMBS_2H_2017 <- read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_2H.csv")
str(UMBS_2H_2017)
summary(UMBS_2H_2017)
UMBS_2H_2018a <- read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_2H_1.csv", skip=1)
UMBS_2H_2018b <- read.csv("raw_data/UMBS/sensor_data/2018/7_31_18/UMBS_2H_731.csv", skip=1)
UMBS_2H_2018 <- rbind(UMBS_2H_2018a, UMBS_2H_2018b)
str(UMBS_2H_2018)
summary(UMBS_2H_2018)

UMBS_2U_2017 <- read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_2U.csv")
tail(UMBS_2U_2017)
str(UMBS_2U_2017)
summary(UMBS_2U_2017)
UMBS_2U_2018a <- read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_2U_1.csv", skip=1)
UMBS_2U_2018b <- read.csv("raw_data/UMBS/sensor_data/2018/7_31_18/UMBS_2U_731.csv", skip=1)
UMBS_2U_2018 <- rbind(UMBS_2U_2018a, UMBS_2U_2018b)
str(UMBS_2U_2018)
summary(UMBS_2U_2018)

UMBS_2_2017 <- merge(UMBS_2H_2017, UMBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
str(UMBS_2_2017)
summary(UMBS_2_2017)
UMBS_2_2018 <- merge(UMBS_2H_2018, UMBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(UMBS_2_2018)
summary(UMBS_2_2018)

# rename columns to match manually curated data
UMBS_2_2018$X.<-NULL
UMBS_2_2018$X..x<-NULL
UMBS_2_2018$X..y<-NULL
UMBS_2_2017[2] <- NULL # "X..x"
UMBS_2_2017[8]<- NULL # "X..y"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736052..LBL..2H_warmed_soil_moisture_5cm."] <- "X2H_warmed_soil_moisture_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Water.Content..m..m...LGR.S.N..10910775..SEN.S.N..10736056..LBL..2H_ambient_soil_moisture_5cm."] <- "X2H_ambient_soil_moisture_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_air_1m."] <- "X2H_warmed_air_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="RH.....LGR.S.N..10910775..SEN.S.N..10737460..LBL..2H_warmed_RH_1m."] <- "X2H_warmed_RH_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "X2H_ambient_aim_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="RH.....LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_RH_1m."] <- "X2H_ambient_RH_1m"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "X2U_ambient_air_10cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "X2U_ambient_soil_temp_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "X2U_warmed_air_10cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "X2U_warmed_soil_temp_5cm"
names(UMBS_2_2018)[names(UMBS_2_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_2_2018)
head(UMBS_2_2017)

#check any columns with NA values, if present
UMBS_2[-which(is.na(UMBS_2$X2U_ambient_air_10cm)),1]
#2U was not logging from 7/28/2015 19:: thorugh 11/24/2015, when it was launched again.

#check to see if all days contain 24h.(only first and last days not expected to have 24h.)
DateTime2 <- as.POSIXlt(UMBS_2$Date_Time, format = "%m/%d/%y %H:%M")
str(DateTime2)
DateTime2_hours <- as.numeric(format(DateTime2, "%H"))
str(DateTime2_hours)
DateTime2_days <- format(DateTime2, "%m%d%y")
str(DateTime2_days)
testing2 <- tapply(DateTime2_hours, as.factor(DateTime2_days), length)
subset(testing2, testing2<24) #09/26/2015 10:00 missing, likely because data was being downloaded/maintenance.
subset(testing2, testing2>24)

write.csv(UMBS_2_2017, file="final_data/UMBS/sensor_data/2017/UMBS_2.csv")
write.csv(UMBS_2_2018, file="final_data/UMBS/sensor_data/2018/UMBS_2.csv")

############# UMBS Pair 3
UMBS_3H_2017 <- read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_3H.csv")
str(UMBS_3H_2017)
summary(UMBS_3H_2017)
UMBS_3H_2018a <- read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_3H_1.csv", skip=1)
UMBS_3H_2018b <- read.csv("raw_data/UMBS/sensor_data/2018/7_31_18/UMBS_3H_731.csv", skip=1)
UMBS_3H_2018 <- rbind(UMBS_3H_2018a, UMBS_3H_2018b)
str(UMBS_3H_2018)
summary(UMBS_3H_2018)

UMBS_3U_2017 <- read.csv("raw_data/UMBS/sensor_data/2017/8_15_2017/UMBS_3U.csv")
tail(UMBS_3U_2017)
str(UMBS_3U_2017)
summary(UMBS_3U_2017)
UMBS_3U_2018a <- read.csv("raw_data/UMBS/sensor_data/2018/6_25_18/6_25_18_UMBS_3U_1.csv", skip=1)
UMBS_3U_2018b <- read.csv("raw_data/UMBS/sensor_data/2018/7_31_18/UMBS_3U_731.csv", skip=1)
UMBS_3U_2018 <- rbind(UMBS_3U_2018a, UMBS_3U_2018b)
str(UMBS_3U_2018)
summary(UMBS_3U_2018)

UMBS_3_2017 <- merge(UMBS_3H_2017, UMBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
str(UMBS_3_2017)
summary(UMBS_3_2017)
UMBS_3_2018 <- merge(UMBS_3H_2018, UMBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
str(UMBS_3_2018)
summary(UMBS_3_2018)

# rename columns to match manually curated data
UMBS_3_2018$X.<-NULL
UMBS_3_2018$X..x<-NULL
UMBS_3_2018$X..y<-NULL
UMBS_3_2017[2] <- NULL # "X..x"
UMBS_3_2017[8]<- NULL # "X..y"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3U_warmed_soil_5cm"] <- "X3U_warmed_soil_temp_5cm"
names(UMBS_3_2017)[names(UMBS_3_2017)=="X3U_ambient_soil_5cm"] <- "X3U_ambient_soil_temp_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736055..LBL..3H_warmed_soil_moisture_5cm."] <- "X3H_warmed_soil_moisture_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736966..SEN.S.N..10736058..LBL..3H_ambient_soil_moisture_5cm."] <- "X3H_ambient_soil_moisture_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_air_1m."] <- "X3H_warmed_air_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="RH.....LGR.S.N..10736966..SEN.S.N..10737458..LBL..3H_warmed_RH_1m."] <- "X3H_warmed_RH_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_air_1m."] <- "X3H_ambient_air_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="RH.....LGR.S.N..10736966..SEN.S.N..10737459..LBL..1H_ambient_RH_1m."] <- "X3H_ambient_RH_1m"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "X3U_warmed_air_10cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "X3U_warmed_soil_temp_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "X3U_ambient_soil_temp_5cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "X3U_ambient_air_10cm"
names(UMBS_3_2018)[names(UMBS_3_2018)=="Date.Time..GMT.04.00"] <- "Date_Time"
head(UMBS_3_2018)
head(UMBS_3_2017)

#####NOTE: Columns atr labelled X1H (e.g.) rather than X3H. 
#check any columns with NA values, if present
which(is.na(UMBS_3$X3U_ambient_air_10cm))
UMBS_3[1132,] #11/24/15 11:00 No U12 readings.  Probably dpwloaded 1h earlier.
UMBS_3[1309,] #6/25/15 17:00 No U12 readings.  Probably launched 1h later. 
#check to see if all days contain 24h. (only first and last days not expected to have 24h.)
DateTime3 <- as.POSIXlt(UMBS_3$Date_Time, format = "%m/%d/%y %H:%M")
str(DateTime3)
DateTime3_hours <- as.numeric(format(DateTime3, "%H"))
str(DateTime3_hours)
DateTime3_days <- format(DateTime3, "%m%d%y")
str(DateTime3_days)
testing3 <- tapply(DateTime3_hours, as.factor(DateTime3_days), length)
subset(testing3, testing3<24)
subset(testing3, testing3>24)

write.csv(UMBS_3_2017, file="final_data/UMBS/sensor_data/2017/UMBS_3.csv")
write.csv(UMBS_3_2018, file="final_data/UMBS/sensor_data/2018/UMBS_3.csv")

####### Create a new merged file that contains data from all years for the both KBS and UMBS pairs.
# write the merged file in the sensor_data folder #######

#UMBS pair 1
UMBS_1_s18 <- read.csv("final_data/UMBS/sensor_data/2018/UMBS_1.csv")
UMBS_1_s17 <- read.csv("final_data/UMBS/sensor_data/2017/UMBS_1.csv")
UMBS_1_s15_16 <- read.csv("final_data/UMBS/sensor_data/2015_2016/UMBS_1.csv")
Merged_UMBS_1<-rbind(UMBS_1_s18,UMBS_1_s17,UMBS_1_s15_16)
dim(Merged_UMBS_1)
write.csv(Merged_UMBS_1, file="final_data/UMBS/sensor_data/Merged_UMBS_1.csv")

#UMBS pair 2
UMBS_2_s18 <- read.csv("final_data/UMBS/sensor_data/2018/UMBS_2.csv")
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