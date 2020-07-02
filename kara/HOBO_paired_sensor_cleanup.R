# TITLE: HOBO paired sensor cleanup
# AUTHORS: Nina Lany (original), Kathryn Schmidt (original), Kara Dobson (edited June 2020)
# COLLABORATORS: Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT: This script combines the data from the U and H units for each pair and writes a csv file to the L1 HOBO_U_H_data folder.
    ## A merged csv file is also created for each pair over every year within the same folder (i.e. KBS_pair1.csv)
    ## This script also created KBS_allyears and UMBS_allyears, combining the data for all U and H paired sensors
# PROJECT: warmXtrophic
# DATE: 2016-2017
    ## KS edit May 23, 2018: created merged files for UMBS; August 1, 2018: remove manual preparation step and add 2018 data from KBS and UMBS
    ## KD edit June 2020: Updated script to insert 2019 and 2020 data & functions

# Clear all existing data
rm(list=ls())

#Load packages
for (package in c("tidyverse", "weathermetrics")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

# Source functions
source("~/warmXtrophic/kara/HOBO_functions.R")

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")


#######################################################################
#    KBS
#######################################################################

############ KBS Pair 1
#Read in H
KBS_1_1516 <- read.csv("L0/KBS/sensor_data/2015_2016/KBS_1.csv")
KBS_1H_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_1H_09012017.csv")
KBS_1H_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_1H_07122018.csv", skip=1)
KBS_1H_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_1H_10072019.csv", skip=1)
KBS_1H_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_1H_04052020.csv", skip=1)

#Read in U
KBS_1U_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_1U_09012017.csv")
KBS_1U_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_1U_07122018.csv",skip=1)[ ,1:6]
KBS_1U_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_1U_10072019.csv",skip=1)[ ,1:6]
KBS_1U_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_1U_04052020.csv",skip=1)[ ,1:6]

# Merge H and U data
KBS_1_2017 <- merge(KBS_1H_2017, KBS_1U_2017, by="Date_Time", all.x=T, all.y=T)
KBS_1_2018 <- merge(KBS_1H_2018, KBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_1_2019 <- merge(KBS_1H_2019, KBS_1U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_1_2020 <- merge(KBS_1H_2020, KBS_1U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

# Rename columns to match manually curated data
KBS_1_1516$X<-NULL
KBS_1_2017$X..x<-NULL
KBS_1_2017$X..y<-NULL
KBS_1_2018$X..x<-NULL
KBS_1_2018$X..y<-NULL
KBS_1_2019$X..x<-NULL
KBS_1_2019$X..y<-NULL
KBS_1_2020$X..x<-NULL
KBS_1_2020$X..y<-NULL

#Apply functions
list_pairk1 <- list(KBS_1_1516=KBS_1_1516, KBS_1_2017=KBS_1_2017, KBS_1_2018=KBS_1_2018, KBS_1_2019=KBS_1_2019, KBS_1_2020=KBS_1_2020)
list_pairk1 <- lapply(list_pairk1, change_pair_names)
list_pairk1 <- lapply(list_pairk1, change_POSIX)

#Write csv files
write.csv(list_pairk1$KBS_1_1516, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1_1516.csv")
write.csv(list_pairk1$KBS_1_2017, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1_2017.csv")
write.csv(list_pairk1$KBS_1_2018, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1_2018.csv")
write.csv(list_pairk1$KBS_1_2019, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1_2019.csv")
write.csv(list_pairk1$KBS_1_2020, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1_2020.csv")

#Merge data from all years & write merged csv
diff1617_1k <- anti_join(list_pairk1$KBS_1_2017, list_pairk1$KBS_1_1516, by = "Date_Time")
diff1718_1k <- anti_join(list_pairk1$KBS_1_2018, list_pairk1$KBS_1_2017, by = "Date_Time")
diff1819_1k <- anti_join(list_pairk1$KBS_1_2019, list_pairk1$KBS_1_2018, by = "Date_Time")
diff1920_1k <- anti_join(list_pairk1$KBS_1_2020, list_pairk1$KBS_1_2019, by = "Date_Time")

KBS_1 <- rbind(list_pairk1$KBS_1_1516, diff1617_1k, diff1718_1k, diff1819_1k, diff1920_1k)
write.csv(KBS_1, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1.csv")

############ KBS Pair 2
#Read in H
KBS_2_1516 <- read.csv("L0/KBS/sensor_data/2015_2016/KBS_2.csv")
KBS_2H_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_2H_09012017.csv")
KBS_2H_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_2H_07122018.csv", skip=1)
KBS_2H_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_2H_10072019.csv", skip=1)
KBS_2H_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_2H_04052020.csv", skip=1)

#Read in U
KBS_2U_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_2U_09012017.csv")
KBS_2U_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_2U_07122018.csv",skip=1)[ ,1:6]
KBS_2U_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_2U_10072019.csv",skip=1)[ ,1:6]
KBS_2U_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_2U_04052020.csv",skip=1)[ ,1:6]

#Merge H and U
KBS_2_2017 <- merge(KBS_2H_2017, KBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
KBS_2_2018 <- merge(KBS_2H_2018, KBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_2_2019 <- merge(KBS_2H_2019, KBS_2U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_2_2020 <- merge(KBS_2H_2020, KBS_2U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Rename columns to match manually curated data
KBS_2_1516$X<-NULL
KBS_2_2017$X..x<-NULL
KBS_2_2017$X..y<-NULL
KBS_2_2018$X..x<-NULL
KBS_2_2018$X..y<-NULL
KBS_2_2019$X..x<-NULL
KBS_2_2019$X..y<-NULL
KBS_2_2020$X..x<-NULL
KBS_2_2020$X..y<-NULL

#Apply functions
list_pairk2 <- list(KBS_2_1516=KBS_2_1516, KBS_2_2017=KBS_2_2017, KBS_2_2018=KBS_2_2018, KBS_2_2019=KBS_2_2019, KBS_2_2020=KBS_2_2020)
list_pairk2 <- lapply(list_pairk2, change_pair_names)
list_pairk2 <- lapply(list_pairk2, change_POSIX)

#Manually rename columns with different names
names(list_pairk2$KBS_2_2017)[names(list_pairk2$KBS_2_2017)=="X2H_ambient_soil_moist_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2018)[names(list_pairk2$KBS_2_2018)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2019)[names(list_pairk2$KBS_2_2019)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2020)[names(list_pairk2$KBS_2_2020)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"

#Write csv files
write.csv(list_pairk2$KBS_2_1516, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2_1516.csv")
write.csv(list_pairk2$KBS_2_2017, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2_2017.csv")
write.csv(list_pairk2$KBS_2_2018, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2_2018.csv")
write.csv(list_pairk2$KBS_2_2019, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2_2019.csv")
write.csv(list_pairk2$KBS_2_2020, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2_2020.csv")

#Merge data from all years & write merged csv
diff1617_2k <- anti_join(list_pairk2$KBS_2_2017, list_pairk2$KBS_2_1516, by = "Date_Time")
diff1718_2k <- anti_join(list_pairk2$KBS_2_2018, list_pairk2$KBS_2_2017, by = "Date_Time")
diff1819_2k <- anti_join(list_pairk2$KBS_2_2019, list_pairk2$KBS_2_2018, by = "Date_Time")
diff1920_2k <- anti_join(list_pairk2$KBS_2_2020, list_pairk2$KBS_2_2019, by = "Date_Time")

KBS_2 <- rbind(list_pairk2$KBS_2_1516, diff1617_2k, diff1718_2k, diff1819_2k, diff1920_2k)
write.csv(KBS_2, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2.csv")

############ KBS Pair 3
#Read in H
KBS_3_1516 <- read.csv("L0/KBS/sensor_data/2015_2016/KBS_3.csv")
KBS_3H_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_3H_09012017.csv")
KBS_3H_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_3H_07122018.csv", skip=1)
KBS_3H_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_3H_10072019.csv", skip=1)
KBS_3H_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_3H_04052020.csv", skip=1)

#Read in U
KBS_3U_2017 <- read.csv("L0/KBS/sensor_data/2017/09_01_2017/KBS_3U_09012017.csv")
KBS_3U_2018 <- read.csv("L0/KBS/sensor_data/2018/07_12_2018 (stations)/KBS_3U_07122018.csv",skip=1)[ ,1:6]
KBS_3U_2019 <- read.csv("L0/KBS/sensor_data/2019/10_07_2019/KBS_3U_10072019.csv",skip=1)[ ,1:6]
KBS_3U_2020 <- read.csv("L0/KBS/sensor_data/2020/04_05_2020/KBS_3U_04052020.csv",skip=1)[ ,1:6]

#Merge H and U
KBS_3_2017 <- merge(KBS_3H_2017, KBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
KBS_3_2018 <- merge(KBS_3H_2018, KBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_3_2019 <- merge(KBS_3H_2019, KBS_3U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_3_2020 <- merge(KBS_3H_2020, KBS_3U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Rename columns to match manually curated data
KBS_3_1516$X<-NULL
KBS_3_2017$X..x<-NULL
KBS_3_2017$X..y<-NULL
KBS_3_2018$X..x<-NULL
KBS_3_2018$X..y<-NULL
KBS_3_2019$X..x<-NULL
KBS_3_2019$X..y<-NULL
KBS_3_2020$X..x<-NULL
KBS_3_2020$X..y<-NULL

#Apply functions
list_pairk3 <- list(KBS_3_1516=KBS_3_1516, KBS_3_2017=KBS_3_2017, KBS_3_2018=KBS_3_2018, KBS_3_2019=KBS_3_2019, KBS_3_2020=KBS_3_2020)
list_pairk3 <- lapply(list_pairk3, change_pair_names)
list_pairk3 <- lapply(list_pairk3, change_POSIX)

#Manually rename columns with different names
names(list_pairk3$KBS_3_2017)[names(list_pairk3$KBS_3_2017)=="X3U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairk3$KBS_3_2017)[names(list_pairk3$KBS_3_2017)=="X3U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairk3$KBS_3_2017)[names(list_pairk3$KBS_3_2017)=="X3H_ambient_soil_moistire_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk3$KBS_3_2018)[names(list_pairk3$KBS_3_2018)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk3$KBS_3_2018)[names(list_pairk3$KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairk3$KBS_3_2018)[names(list_pairk3$KBS_3_2018)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairk3$KBS_3_2019)[names(list_pairk3$KBS_3_2019)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk3$KBS_3_2019)[names(list_pairk3$KBS_3_2019)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairk3$KBS_3_2019)[names(list_pairk3$KBS_3_2019)=="Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairk3$KBS_3_2020)[names(list_pairk3$KBS_3_2020)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk3$KBS_3_2020)[names(list_pairk3$KBS_3_2020)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairk3$KBS_3_2020)[names(list_pairk3$KBS_3_2020)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"

#Create csv files for each year
write.csv(list_pairk3$KBS_3_1516, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3_1516.csv")
write.csv(list_pairk3$KBS_3_2017, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3_2017.csv")
write.csv(list_pairk3$KBS_3_2018, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3_2018.csv")
write.csv(list_pairk3$KBS_3_2019, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3_2019.csv")
write.csv(list_pairk3$KBS_3_2020, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3_2020.csv")

#Merge data from all years & write merged csv
diff1617_3k <- anti_join(list_pairk3$KBS_3_2017, list_pairk3$KBS_3_1516, by = "Date_Time")
diff1718_3k <- anti_join(list_pairk3$KBS_3_2018, list_pairk3$KBS_3_2017, by = "Date_Time")
diff1819_3k <- anti_join(list_pairk3$KBS_3_2019, list_pairk3$KBS_3_2018, by = "Date_Time")
diff1920_3k <- anti_join(list_pairk3$KBS_3_2020, list_pairk3$KBS_3_2019, by = "Date_Time")

KBS_3 <- rbind(list_pairk3$KBS_3_1516, diff1617_3k, diff1718_3k, diff1819_3k, diff1920_3k)
write.csv(KBS_3, file="L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3.csv")

#Combine data from all years and all paired stations for KBS
KBS_allyears <- rbind(KBS_1, KBS_2, KBS_3)
write.csv(KBS_all, file="L0/KBS/sensor_data/KBS_allyears")



#######################################################################
#    UMBS
#######################################################################
# KS: Relaunched HOBO loggers on 6-25-18 for U
# KD: Relaunched HOBO loggers on 5-13-20 for U

############ UMBS Pair 1
#Read in H
UMBS_1_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_1.csv")
UMBS_1H_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_1H_08152017.csv")
UMBS_1H_2018 <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_1H_09192018.csv", skip=1)
UMBS_1H_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_1H_09102019.csv", skip=1)
UMBS_1H_2020 <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_1H_06122020.csv", skip=1)

#Read in U
UMBS_1U_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_1U_08152017.csv")
UMBS_1U_2018a <- read.csv("L0/UMBS/sensor_data/2018/06_25_2018/UMBS_1U_06252018.csv", skip=1)
UMBS_1U_2018b <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_1U_09192018.csv", skip=1)
UMBS_1U_2018 <- rbind(UMBS_1U_2018a, UMBS_1U_2018b)
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
UMBS_1_2018 <- merge(UMBS_1H_2018, UMBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
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

#Apply functions
list_pairu1 <- list(UMBS_1_1516=UMBS_1_1516, UMBS_1_2017=UMBS_1_2017, UMBS_1_2018=UMBS_1_2018, UMBS_1_2019=UMBS_1_2019, UMBS_1_2020=UMBS_1_2020)
list_pairu1 <- lapply(list_pairu1, change_pair_names)
list_pairu1 <- lapply(list_pairu1, change_POSIX)

#Manually rename columns with different names
names(list_pairu1$UMBS_1_1516)[names(list_pairu1$UMBS_1_1516)=="X1U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_1516)[names(list_pairu1$UMBS_1_1516)=="X1U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2017)[names(list_pairu1$UMBS_1_2017)=="X1U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2017)[names(list_pairu1$UMBS_1_2017)=="X1U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2018)[names(list_pairu1$UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2018)[names(list_pairu1$UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2019)[names(list_pairu1$UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2019)[names(list_pairu1$UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"

#Create csv files for each year
write.csv(list_pairu1$UMBS_1_1516, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1_1516.csv")
write.csv(list_pairu1$UMBS_1_2017, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1_2017.csv")
write.csv(list_pairu1$UMBS_1_2018, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1_2018.csv")
write.csv(list_pairu1$UMBS_1_2019, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1_2019.csv")
write.csv(list_pairu1$UMBS_1_2020, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1_2020.csv")

#Merge data from all years & write merged csv
diff1617_1u <- anti_join(list_pairu1$UMBS_1_2017, list_pairu1$UMBS_1_1516, by = "Date_Time")
diff1718_1u <- anti_join(list_pairu1$UMBS_1_2018, list_pairu1$UMBS_1_2017, by = "Date_Time")
diff1819_1u <- anti_join(list_pairu1$UMBS_1_2019, list_pairu1$UMBS_1_2018, by = "Date_Time")
diff1920_1u <- anti_join(list_pairu1$UMBS_1_2020, list_pairu1$UMBS_1_2019, by = "Date_Time")

UMBS_1 <- rbind(list_pairu1$UMBS_1_1516, diff1617_1u, diff1718_1u, diff1819_1u, diff1920_1u)
write.csv(UMBS_1, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1.csv")

############ UMBS Pair 2
#2U was not logging from 7/28/2015 through 11/24/2015, when it was launched again
#Read in H
UMBS_2_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_2.csv")
UMBS_2H_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_2H_08152017.csv")
UMBS_2H_2018 <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_2H_09192018.csv", skip=1)
UMBS_2H_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_2H_09102019.csv", skip=1)
UMBS_2H_2020 <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_2H_06122020.csv", skip=1)

#Read in U
UMBS_2U_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_2U_08152017.csv")
UMBS_2U_2018a <- read.csv("L0/UMBS/sensor_data/2018/06_25_2018/UMBS_2U_06252018.csv", skip=1)
UMBS_2U_2018b <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_2U_09192018.csv", skip=1)
UMBS_2U_2018 <- rbind(UMBS_2U_2018a, UMBS_2U_2018b)
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
UMBS_2_2018 <- merge(UMBS_2H_2018, UMBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_2_2019 <- merge(UMBS_2H_2019, UMBS_2U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_2_2020 <- merge(UMBS_2H_2020, UMBS_2U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

# rename columns to match manually curated data
UMBS_2_1516$X<-NULL
UMBS_2_2017$X..x <- NULL
UMBS_2_2017$X..y<- NULL
UMBS_2_2018$X..x<-NULL
UMBS_2_2018$X..y<-NULL
UMBS_2_2019$X..x<-NULL
UMBS_2_2019$X..y<-NULL
UMBS_2_2020$X..x<-NULL
UMBS_2_2020$X..y<-NULL

#Apply functions
list_pairu2 <- list(UMBS_2_1516=UMBS_2_1516, UMBS_2_2017=UMBS_2_2017, UMBS_2_2018=UMBS_2_2018, UMBS_2_2019=UMBS_2_2019, UMBS_2_2020=UMBS_2_2020)
list_pairu2 <- lapply(list_pairu2, change_pair_names)
list_pairu2 <- lapply(list_pairu2, change_POSIX)

#Manually rename columns with different names
names(list_pairu2$UMBS_2_1516)[names(list_pairu2$UMBS_2_1516)=="X2U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairu2$UMBS_2_1516)[names(list_pairu2$UMBS_2_1516)=="X2U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairu2$UMBS_2_2017)[names(list_pairu2$UMBS_2_2017)=="X2H_ambient_aim_1m"] <- "XH_ambient_air_1m"
names(list_pairu2$UMBS_2_2017)[names(list_pairu2$UMBS_2_2017)=="X2U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairu2$UMBS_2_2017)[names(list_pairu2$UMBS_2_2017)=="X2U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairu2$UMBS_2_2018)[names(list_pairu2$UMBS_2_2018)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"
names(list_pairu2$UMBS_2_2018)[names(list_pairu2$UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairu2$UMBS_2_2018)[names(list_pairu2$UMBS_2_2018)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu2$UMBS_2_2019)[names(list_pairu2$UMBS_2_2019)=="Temp...F..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"
names(list_pairu2$UMBS_2_2019)[names(list_pairu2$UMBS_2_2019)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu2$UMBS_2_2019)[names(list_pairu2$UMBS_2_2019)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairu2$UMBS_2_2020)[names(list_pairu2$UMBS_2_2020)=="Temp...C..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"

#Create csv files for each year
write.csv(list_pairu2$UMBS_2_1516, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2_1516.csv")
write.csv(list_pairu2$UMBS_2_2017, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2_2017.csv")
write.csv(list_pairu2$UMBS_2_2018, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2_2018.csv")
write.csv(list_pairu2$UMBS_2_2019, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2_2019.csv")
write.csv(list_pairu2$UMBS_2_2020, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2_2020.csv")

#Merge data from all years & write merged csv
diff1617_2u <- anti_join(list_pairu2$UMBS_2_2017, list_pairu2$UMBS_2_1516, by = "Date_Time")
diff1718_2u <- anti_join(list_pairu2$UMBS_2_2018, list_pairu2$UMBS_2_2017, by = "Date_Time")
diff1819_2u <- anti_join(list_pairu2$UMBS_2_2019, list_pairu2$UMBS_2_2018, by = "Date_Time")
diff1920_2u <- anti_join(list_pairu2$UMBS_2_2020, list_pairu2$UMBS_2_2019, by = "Date_Time")

UMBS_2 <- rbind(list_pairu2$UMBS_2_1516, diff1617_2u, diff1718_2u, diff1819_2u, diff1920_2u)
write.csv(UMBS_2, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2.csv")

############ UMBS Pair 3
#Read in H
UMBS_3_1516 <- read.csv("L0/UMBS/sensor_data/2015_2016/UMBS_3.csv")[-1,]
UMBS_3H_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_3H_08152017.csv")
UMBS_3H_2018 <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_3H_09192018.csv", skip=1)
UMBS_3H_2019 <- read.csv("L0/UMBS/sensor_data/2019/09_10_2019/UMBS_3H_09102019.csv", skip=1)
UMBS_3H_2020 <- read.csv("L0/UMBS/sensor_data/2020/06_12_2020/UMBS_3H_06122020.csv", skip=1)

#Read in U
UMBS_3U_2017 <- read.csv("L0/UMBS/sensor_data/2017/08_15_2017/UMBS_3U_08152017.csv")
UMBS_3U_2018a <- read.csv("L0/UMBS/sensor_data/2018/06_25_2018/UMBS_3U_06252018.csv", skip=1)
UMBS_3U_2018b <- read.csv("L0/UMBS/sensor_data/2018/09_19_2018/UMBS_3U_09192018.csv", skip=1)
UMBS_3U_2018 <- rbind(UMBS_3U_2018a, UMBS_3U_2018b)
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
UMBS_3_2018 <- merge(UMBS_3H_2018, UMBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_3_2019 <- merge(UMBS_3H_2019, UMBS_3U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_3_2020 <- merge(UMBS_3H_2020, UMBS_3U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Rename columns to match manually curated data
UMBS_3_1516$X<-NULL
UMBS_3_2017$X..x <- NULL
UMBS_3_2017$X..y<- NULL
UMBS_3_2018$X..x<-NULL
UMBS_3_2018$X..y<-NULL
UMBS_3_2019$X..x<-NULL
UMBS_3_2019$X..y<-NULL
UMBS_3_2020$X..x<-NULL
UMBS_3_2020$X..y<-NULL

#Apply functions
list_pairu3 <- list(UMBS_3_1516=UMBS_3_1516, UMBS_3_2017=UMBS_3_2017, UMBS_3_2018=UMBS_3_2018, UMBS_3_2019=UMBS_3_2019, UMBS_3_2020=UMBS_3_2020)
list_pairu3 <- lapply(list_pairu3, change_pair_names)
list_pairu3 <- lapply(list_pairu3, change_POSIX)

#Create csv files for each year
write.csv(list_pairu3$UMBS_3_1516, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3_1516.csv")
write.csv(list_pairu3$UMBS_3_2017, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3_2017.csv")
write.csv(list_pairu3$UMBS_3_2018, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3_2018.csv")
write.csv(list_pairu3$UMBS_3_2019, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3_2019.csv")
write.csv(list_pairu3$UMBS_3_2020, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3_2020.csv")

#Merge data from all years & write merged csv
diff1617_3u <- anti_join(list_pairu3$UMBS_3_2017, list_pairu3$UMBS_3_1516, by = "Date_Time")
diff1718_3u <- anti_join(list_pairu3$UMBS_3_2018, list_pairu3$UMBS_3_2017, by = "Date_Time")
diff1819_3u <- anti_join(list_pairu3$UMBS_3_2019, list_pairu3$UMBS_3_2018, by = "Date_Time")
diff1920_3u <- anti_join(list_pairu3$UMBS_3_2020, list_pairu3$UMBS_3_2019, by = "Date_Time")

UMBS_3 <- rbind(list_pairu3$UMBS_3_1516, diff1617_3u, diff1718_3u, diff1819_3u, diff1920_3u)
write.csv(UMBS_3, file="L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3.csv")

