##################    prep HOBO data    ###############################
#This script reads in HOBO data from raw_data, combines the data from the U21 and the H?? units for each pair, and writes a CSV file to the final_data folder.  The KBS_sensors and UMBS_sensors scripts then take that data and summarize/analyze how the chambers affect the abiotic environment. 
# original script created by NKL & KBW 2016-2017

rm(list=ls())

#read in the data (change the path to navigate to the MIGrass directory on your computer).
#setwd("/Users/Nina/Dropbox/MI_GRASS/")
setwd("S:/DATA/MIGrass/")

### Begin with HOBO data that have been manually prepared by removing header row and renaming columns. The column with the time stamp must be named "Date_Time". Also try to remove any "Logged" columns - the HOBO U12 units print a line marking each time data were offloaded.

############ KBS Pair 1
KBS_1H <- read.csv("raw_data/KBS/sensor_data/KBS_1H.csv")
str(KBS_1H)
summary(KBS_1H)

KBS_1U <- read.csv("raw_data/KBS/sensor_data/KBS_1U.csv")
str(KBS_1U)
summary(KBS_1U)


KBS_1 <- merge(KBS_1H, KBS_1U, by="Date_Time", all.x=T, all.y=T)
str(KBS_1)
summary(KBS_1)

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

write.csv(KBS_1, file="final_data/KBS/sensor_data/KBS_1.csv")
############## KBS Pair 2
KBS_2H <- read.csv("raw_data/KBS/sensor_data/KBS_2H.csv")
str(KBS_2H)
summary(KBS_2H)

KBS_2U <- read.csv("raw_data/KBS/sensor_data/KBS_2U.csv")
str(KBS_2U)
summary(KBS_2U)
KBS_2 <- merge(KBS_2H, KBS_2U, by="Date_Time", all.x=T, all.y=T)
str(KBS_2)
summary(KBS_2)

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

write.csv(KBS_2, file="final_data/KBS/sensor_data/KBS_2.csv")

############# KBS Pair 3
KBS_3H <- read.csv("raw_data/KBS/sensor_data/KBS_3H.csv")
str(KBS_3H)
summary(KBS_3H)

KBS_3U <- read.csv("raw_data/KBS/sensor_data/KBS_3U.csv")
str(KBS_3U)
summary(KBS_3U)

KBS_3 <- merge(KBS_3H, KBS_3U, by="Date_Time", all.x=T, all.y=T)
str(KBS_3)
summary(KBS_3)

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

write.csv(KBS_3, file="final_data/KBS/sensor_data/KBS_3.csv")


#######################################################################
#    UMBS
#######################################################################

##UMBS Pair 1
UMBS_1H <- read.csv("raw_data/UMBS/sensor_data/UMBS_1H.csv")
str(UMBS_1H)
summary(UMBS_1H)

UMBS_1U <- read.csv("raw_data/UMBS/sensor_data/UMBS_1U.csv")
str(UMBS_1U)
summary(UMBS_1U)


UMBS_1 <- merge(UMBS_1H, UMBS_1U, by="Date_Time", all.x=T, all.y=T)
str(UMBS_1)
summary(UMBS_1)

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

write.csv(UMBS_1, file="final_data/UMBS/sensor_data/UMBS_1.csv")

############## UMBS Pair 2
UMBS_2H <- read.csv("raw_data/UMBS/sensor_data/UMBS_2H.csv")
str(UMBS_2H)
summary(UMBS_2H)

UMBS_2U <- read.csv("raw_data/UMBS/sensor_data/UMBS_2U.csv")
str(UMBS_2U)
summary(UMBS_2U)
UMBS_2 <- merge(UMBS_2H, UMBS_2U, by="Date_Time", all.x=T, all.y=T)
str(UMBS_2)
summary(UMBS_2)

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

write.csv(UMBS_2, file="final_data/UMBS/sensor_data/UMBS_2.csv")

############# UMBS Pair 3
UMBS_3H <- read.csv("raw_data/UMBS/sensor_data/UMBS_3H.csv")
str(UMBS_3H)
summary(UMBS_3H)

UMBS_3U <- read.csv("raw_data/UMBS/sensor_data/UMBS_3U.csv")
str(UMBS_3U)
summary(UMBS_3U)

UMBS_3 <- merge(UMBS_3H, UMBS_3U, by="Date_Time", all.x=T, all.y=T)
str(UMBS_3)
summary(UMBS_3)
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

write.csv(UMBS_3, file="final_data/UMBS/sensor_data/UMBS_3.csv")

###############################################################################