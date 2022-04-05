# TITLE:          HOBO paired sensor cleanup
# AUTHORS:        Nina Lany (original), Kathryn Schmidt (original), Kara Dobson (edited June 2020)
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    This script combines the data from the U and H units for each pair and writes a csv file to the L1 HOBO_U_H_data folder.
    ## A merged csv file is also created for each pair over every year within the same folder (i.e. KBS_pair1.csv)
    ## This script also created KBS_allyears and UMBS_allyears, combining the data for all U and H paired sensors
# PROJECT:        warmXtrophic
# DATE:           2016-2017
    ## KS edit May 23, 2018: created merged files for UMBS; August 1, 2018: remove manual preparation step and add 2018 data from KBS and UMBS
    ## KD edit June 2020 & May 2021: Updated script to insert 2019 and 2020 data & functions
    ## KD edit Nov 2021 to add 2021 data

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(weathermetrics)

# Source functions
source("~/warmXtrophic/R/L1/HOBO_functions_L1.R")

# Set working directory
Sys.getenv("L0DIR")
L0_dir<-Sys.getenv("L0DIR")
list.files(L0_dir)

#######################################################################
#    KBS
#######################################################################

# note: data read in is often cumulative, so for example data from 2018 may contain data from 2016 and 2017 also
# the data are merged with this duplicate data across years removed in the "merge_L1" scripts

############ KBS Pair 1
#Read in data from H pendants
KBS_1_1516 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2015_2016/KBS_1.csv"))
KBS_1H_2017 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_1H_09012017.csv"))
KBS_1H_2018 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2018/07_12_2018 (stations)/KBS_1H_07122018.csv"), skip=1)
KBS_1H_2019 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2019/10_07_2019/KBS_1H_10072019.csv"), skip=1)
KBS_1H_2020 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2020/10_14_2020/KBS_1H_10142020.csv"), skip=1)
KBS_1H_2021 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2021/11_10_2021/1H_kbs_11202021.csv"), skip=1)

#Read in data from U pendants
KBS_1U_2017 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_1U_09012017.csv"))
KBS_1U_2018 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_29_2018 (stations minus 3)/KBS_1U_09292018.csv"), skip=1)[ ,1:6]
KBS_1U_2019 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2019/10_07_2019/KBS_1U_10072019.csv"), skip=1)[ ,1:6]
KBS_1U_2020 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2020/10_14_2020/KBS_1U_10142020.csv"), skip=1)[ ,1:6]
KBS_1U_2021 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2021/11_10_2021/1U_kbs_11202021.csv"), skip=1)[ ,1:6]

# Merge H and U data - 2015/2016 did not have separate U and H files
KBS_1_2017 <- merge(KBS_1H_2017, KBS_1U_2017, by="Date_Time", all.x=T, all.y=T)
KBS_1_2018 <- merge(KBS_1H_2018, KBS_1U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_1_2019 <- merge(KBS_1H_2019, KBS_1U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_1_2020 <- merge(KBS_1H_2020, KBS_1U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_1_2021 <- merge(KBS_1H_2021, KBS_1U_2021, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
# note: sensor 1 for KBS failed for 2021 - it is removed in the analyses and plotting scripts

# Apply functions
list_pairk1 <- list(KBS_1_1516=KBS_1_1516, KBS_1_2017=KBS_1_2017, KBS_1_2018=KBS_1_2018, KBS_1_2019=KBS_1_2019, KBS_1_2020=KBS_1_2020, KBS_1_2021=KBS_1_2021)
list_pairk1 <- lapply(list_pairk1, change_pair_names)
list_pairk1 <- lapply(list_pairk1, change_POSIX)
list_pairk1 <- lapply(list_pairk1, remove_col, name=c('X', 'X..x', 'X..y'))
list_pairk1[2:4] <- lapply(list_pairk1[2:4], f_to_c)
list_pairk1 <- lapply(list_pairk1, remove_outliers)

############ KBS Pair 2
#Read in H
KBS_2_1516 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2015_2016/KBS_2.csv"))
KBS_2H_2017 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_2H_09012017.csv"))
KBS_2H_2018 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2018/07_12_2018 (stations)/KBS_2H_07122018.csv"), skip=1)
KBS_2H_2019 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2019/10_07_2019/KBS_2H_10072019.csv"), skip=1)
KBS_2H_2020 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2020/10_14_2020/KBS_2H_10142020.csv"), skip=1)
KBS_2H_2021 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2021/11_10_2021/2H_kbs_11202021.csv"), skip=1)

#Read in U
KBS_2U_2017 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_2U_09012017.csv"))
KBS_2U_2018 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2018/07_12_2018 (stations)/KBS_2U_07122018.csv"), skip=1)[ ,1:6]
KBS_2U_2019 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2019/10_07_2019/KBS_2U_10072019.csv"), skip=1)[ ,1:6]
KBS_2U_2020 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2020/10_14_2020/KBS_2U_10142020.csv"), skip=1)[ ,1:6]
KBS_2U_2021 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2021/11_10_2021/2U_kbs_11202021.csv"), skip=1)[ ,1:6]

#Merge H and U
KBS_2_2017 <- merge(KBS_2H_2017, KBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
KBS_2_2018 <- merge(KBS_2H_2018, KBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_2_2019 <- merge(KBS_2H_2019, KBS_2U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_2_2020 <- merge(KBS_2H_2020, KBS_2U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_2_2021 <- merge(KBS_2H_2021, KBS_2U_2021, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Apply functions
list_pairk2 <- list(KBS_2_1516=KBS_2_1516, KBS_2_2017=KBS_2_2017, KBS_2_2018=KBS_2_2018, KBS_2_2019=KBS_2_2019, KBS_2_2020=KBS_2_2020, KBS_2_2021=KBS_2_2021)
list_pairk2 <- lapply(list_pairk2, change_pair_names)
list_pairk2 <- lapply(list_pairk2, change_POSIX)
list_pairk2 <- lapply(list_pairk2, remove_col, name=c('X', 'X..x', 'X..y'))
list_pairk2[2:4] <- lapply(list_pairk2[2:4], f_to_c)

#Manually rename columns with different names
names(list_pairk2$KBS_2_2017)[names(list_pairk2$KBS_2_2017)=="X2H_ambient_soil_moist_5cm"] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2018)[names(list_pairk2$KBS_2_2018)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2019)[names(list_pairk2$KBS_2_2019)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2020)[names(list_pairk2$KBS_2_2020)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk2$KBS_2_2021)[names(list_pairk2$KBS_2_2021)=="Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm."] <- "XH_ambient_soil_moisture_5cm"

list_pairk2 <- lapply(list_pairk2, remove_outliers)

############ KBS Pair 3
#Read in H
KBS_3_1516 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2015_2016/KBS_3.csv"))
KBS_3H_2017 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_3H_09012017.csv"))
KBS_3H_2018 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2018/07_12_2018 (stations)/KBS_3H_07122018.csv"), skip=1)
KBS_3H_2019 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2019/10_07_2019/KBS_3H_10072019.csv"), skip=1)
KBS_3H_2020 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2020/10_14_2020/KBS_3H_10142020.csv"), skip=1)
KBS_3H_2021 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2021/11_10_2021/3H_kbs_11202021.csv"), skip=1)

#Read in U
KBS_3U_2017 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_3U_09012017.csv"))
KBS_3U_2018 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2018/07_12_2018 (stations)/KBS_3U_07122018.csv"), skip=1)[ ,1:6]
KBS_3U_2019 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2019/10_07_2019/KBS_3U_10072019.csv"), skip=1)[ ,1:6]
KBS_3U_2020 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2020/10_14_2020/KBS_3U_10142020.csv"), skip=1)[ ,1:6]
KBS_3U_2021 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2021/11_10_2021/3U_kbs_11202021.csv"), skip=1)[ ,1:6]

#Merge H and U
KBS_3_2017 <- merge(KBS_3H_2017, KBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
KBS_3_2018 <- merge(KBS_3H_2018, KBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_3_2019 <- merge(KBS_3H_2019, KBS_3U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_3_2020 <- merge(KBS_3H_2020, KBS_3U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_3_2021 <- merge(KBS_3H_2021, KBS_3U_2021, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Apply functions
list_pairk3 <- list(KBS_3_1516=KBS_3_1516, KBS_3_2017=KBS_3_2017, KBS_3_2018=KBS_3_2018, KBS_3_2019=KBS_3_2019, KBS_3_2020=KBS_3_2020, KBS_3_2021=KBS_3_2021)
list_pairk3 <- lapply(list_pairk3, change_pair_names)
list_pairk3 <- lapply(list_pairk3, change_POSIX)
list_pairk3 <- lapply(list_pairk3, remove_col, name=c('X', 'X..x', 'X..y'))

#Manually rename columns with different names - functions won't work on these
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
names(list_pairk3$KBS_3_2021)[names(list_pairk3$KBS_3_2021)=="Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm."] <- "XH_ambient_soil_moisture_5cm"
names(list_pairk3$KBS_3_2021)[names(list_pairk3$KBS_3_2021)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairk3$KBS_3_2021)[names(list_pairk3$KBS_3_2021)=="Temp...C..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"

list_pairk3[2:4] <- lapply(list_pairk3[2:4], f_to_c)
list_pairk3 <- lapply(list_pairk3, remove_outliers)

#Create .RData file - this is used in the script that merges all of the clean data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
save(list_pairk1, list_pairk2, list_pairk3, file=file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.RData"))



#######################################################################
#    UMBS
#######################################################################
# KS: Relaunched HOBO loggers on 6-25-18 for U
# KD: Relaunched HOBO loggers on 5-13-20 for U

############ UMBS Pair 1
#Read in H
UMBS_1_1516 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2015_2016/UMBS_1.csv"))
UMBS_1H_2017 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_1H_08152017.csv"))
UMBS_1H_2018 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_1H_09192018.csv"), skip=1)
UMBS_1H_2019 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_10_2019/UMBS_1H_09102019.csv"), skip=1)
UMBS_1H_2020 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_1H_20200901.csv"), skip=1)
UMBS_1H_2021 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/11_15_2021/UMBS_1H_20211115.csv"), skip=1)

#Read in U - two separate files for 2018, 2020 and 2021 because of the sensor reset
UMBS_1U_2017 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_1U_08152017.csv"))
UMBS_1U_2018a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/06_25_2018/UMBS_1U_06252018.csv"), skip=1)
UMBS_1U_2018b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_1U_09192018.csv"), skip=1)
UMBS_1U_2018 <- rbind(UMBS_1U_2018a, UMBS_1U_2018b)
UMBS_1U_2019 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_10_2019/UMBS_1U_09102019.csv"), skip=1)[ ,1:6]
UMBS_1U_2020a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/05_13_2020/UMBS_1U_05132020.csv"), skip=1)[ ,1:6]
UMBS_1U_2020b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_1U_20200901.csv"), skip=1)[ ,1:6]
UMBS_1U_2021a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/06_16_2021/UMBS_1U_20210617.csv"), skip=1)[ ,1:6]
UMBS_1U_2021b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/11_15_2021/UMBS_1U_20211115.csv"), skip=1)[ ,1:6]
UMBS_1U_2021 <- rbind(UMBS_1U_2021a, UMBS_1U_2021b)

# Need to change column names in order to merge both 2020 files
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
UMBS_1_2021 <- merge(UMBS_1H_2021, UMBS_1U_2021, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
# sensor 1 for 2021 UMBS had a wasp nest from July-Nov, these data are removed in analyses & plotting scripts

#Apply functions
list_pairu1 <- list(UMBS_1_1516=UMBS_1_1516, UMBS_1_2017=UMBS_1_2017, UMBS_1_2018=UMBS_1_2018, UMBS_1_2019=UMBS_1_2019, UMBS_1_2020=UMBS_1_2020, UMBS_1_2021=UMBS_1_2021)
list_pairu1 <- lapply(list_pairu1, change_pair_names)
list_pairu1 <- lapply(list_pairu1, change_POSIX)
list_pairu1 <- lapply(list_pairu1, remove_col, name=c('X', 'X..x', 'X..y'))

#Manually rename columns with different names
names(list_pairu1$UMBS_1_1516)[names(list_pairu1$UMBS_1_1516)=="X1U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_1516)[names(list_pairu1$UMBS_1_1516)=="X1U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2017)[names(list_pairu1$UMBS_1_2017)=="X1U_ambient_soil_temp_5cm"] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2017)[names(list_pairu1$UMBS_1_2017)=="X1U_warmed_soil_temp_5cm"] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2018)[names(list_pairu1$UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2018)[names(list_pairu1$UMBS_1_2018)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2019)[names(list_pairu1$UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2019)[names(list_pairu1$UMBS_1_2019)=="Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairu1$UMBS_1_2021)[names(list_pairu1$UMBS_1_2021)=="Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu1$UMBS_1_2021)[names(list_pairu1$UMBS_1_2021)=="Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"

list_pairu1[2:4] <- lapply(list_pairu1[2:4], f_to_c)
list_pairu1 <- lapply(list_pairu1, remove_outliers)

############ UMBS Pair 2
#2U was not logging from 7/28/2015 through 11/24/2015, when it was launched again
#Read in H
UMBS_2_1516 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2015_2016/UMBS_2.csv"))
UMBS_2H_2017 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_2H_08152017.csv"))
UMBS_2H_2018 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_2H_09192018.csv"), skip=1)
UMBS_2H_2019 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_10_2019/UMBS_2H_09102019.csv"), skip=1)
UMBS_2H_2020 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_2H_20200901.csv"), skip=1)
UMBS_2H_2021 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/11_15_2021/UMBS_2H_20211115.csv"), skip=1)

#Read in U
UMBS_2U_2017 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_2U_08152017.csv"))
UMBS_2U_2018a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/06_25_2018/UMBS_2U_06252018.csv"), skip=1)
UMBS_2U_2018b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_2U_09192018.csv"), skip=1)
UMBS_2U_2018 <- rbind(UMBS_2U_2018a, UMBS_2U_2018b)
UMBS_2U_2019 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_10_2019/UMBS_2U_09102019.csv"), skip=1)[ ,1:6]
UMBS_2U_2020a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/05_13_2020/UMBS_2U_05132020.csv"), skip=1)[ ,1:6]
UMBS_2U_2020b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_2U_20200901.csv"), skip=1)[ ,1:6]
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_2U_2020a)[names(UMBS_2U_2020a)=="Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_2U_2020b)[names(UMBS_2U_2020b)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
UMBS_2U_2020 <- rbind(UMBS_2U_2020a, UMBS_2U_2020b)
UMBS_2U_2021a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/06_16_2021/UMBS_2U_20210617.csv"), skip=1)[ ,1:6]
UMBS_2U_2021b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/11_15_2021/UMBS_2U_20211115.csv"), skip=1)[ ,1:6]
UMBS_2U_2021 <- rbind(UMBS_2U_2021a, UMBS_2U_2021b)

#Merge the data
UMBS_2_2017 <- merge(UMBS_2H_2017, UMBS_2U_2017, by="Date_Time", all.x=T, all.y=T)
UMBS_2_2018 <- merge(UMBS_2H_2018, UMBS_2U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_2_2019 <- merge(UMBS_2H_2019, UMBS_2U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_2_2020 <- merge(UMBS_2H_2020, UMBS_2U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_2_2021 <- merge(UMBS_2H_2021, UMBS_2U_2021, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Apply functions
list_pairu2 <- list(UMBS_2_1516=UMBS_2_1516, UMBS_2_2017=UMBS_2_2017, UMBS_2_2018=UMBS_2_2018, UMBS_2_2019=UMBS_2_2019, UMBS_2_2020=UMBS_2_2020, UMBS_2_2021=UMBS_2_2021)
list_pairu2 <- lapply(list_pairu2, change_pair_names)
list_pairu2 <- lapply(list_pairu2, change_POSIX)
list_pairu2 <- lapply(list_pairu2, remove_col, name=c('X', 'X..x', 'X..y'))

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
names(list_pairu2$UMBS_2_2021)[names(list_pairu2$UMBS_2_2021)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm."] <- "XU_ambient_soil_temp_5cm"
names(list_pairu2$UMBS_2_2021)[names(list_pairu2$UMBS_2_2021)=="Temp...C..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm."] <- "XU_warmed_soil_temp_5cm"
names(list_pairu2$UMBS_2_2021)[names(list_pairu2$UMBS_2_2021)=="Temp...C..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m."] <- "XH_ambient_air_1m"

list_pairu2[2:4] <- lapply(list_pairu2[2:4], f_to_c)
list_pairu2 <- lapply(list_pairu2, remove_outliers)

############ UMBS Pair 3
#Read in H
UMBS_3_1516 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2015_2016/UMBS_3.csv"))[-1,]
UMBS_3H_2017 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_3H_08152017.csv"))
UMBS_3H_2018 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_3H_09192018.csv"), skip=1)
UMBS_3H_2019 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_10_2019/UMBS_3H_09102019.csv"), skip=1)
UMBS_3H_2020 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_3H_20200901.csv"), skip=1)
UMBS_3H_2021 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/11_15_2021/UMBS_3H_20211115.csv"), skip=1)

#Read in U
UMBS_3U_2017 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_3U_08152017.csv"))
UMBS_3U_2018a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/06_25_2018/UMBS_3U_06252018.csv"), skip=1)
UMBS_3U_2018b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_3U_09192018.csv"), skip=1)
UMBS_3U_2018 <- rbind(UMBS_3U_2018a, UMBS_3U_2018b)
UMBS_3U_2019 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_10_2019/UMBS_3U_09102019.csv"), skip=1)[ ,1:6]
UMBS_3U_2020a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/05_13_2020/UMBS_3U_05132020.csv"), skip=1)[ ,1:6]
UMBS_3U_2020b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_3U_20200901.csv"), skip=1)[ ,1:6]
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_3U_2020a)[names(UMBS_3U_2020a)=="Temp...F..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_soil_5cm."] <- "XU_ambient_soil_temp_5cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_ambient_air_10cm."] <- "XU_ambient_air_10cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_air_10cm."] <- "XU_warmed_air_10cm"
names(UMBS_3U_2020b)[names(UMBS_3U_2020b)=="Temp...C..LGR.S.N..10737619..SEN.S.N..10737619..LBL..3U_warmed_soil_5cm."] <- "XU_warmed_soil_temp_5cm"
UMBS_3U_2020 <- rbind(UMBS_3U_2020a, UMBS_3U_2020b)
UMBS_3U_2021a <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/06_16_2021/UMBS_3U_20210617.csv"), skip=1)[ ,1:6]
UMBS_3U_2021b <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2021/11_15_2021/UMBS_3U_20211115.csv"), skip=1)[ ,1:6]
UMBS_3U_2021 <- rbind(UMBS_3U_2021a, UMBS_3U_2021b)


#Merge the data
UMBS_3_2017 <- merge(UMBS_3H_2017, UMBS_3U_2017, by="Date_Time", all.x=T, all.y=T)
UMBS_3_2018 <- merge(UMBS_3H_2018, UMBS_3U_2018, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_3_2019 <- merge(UMBS_3H_2019, UMBS_3U_2019, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_3_2020 <- merge(UMBS_3H_2020, UMBS_3U_2020, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
UMBS_3_2021 <- merge(UMBS_3H_2021, UMBS_3U_2021, by="Date.Time..GMT.04.00", all.x=T, all.y=T)

#Apply functions
list_pairu3 <- list(UMBS_3_1516=UMBS_3_1516, UMBS_3_2017=UMBS_3_2017, UMBS_3_2018=UMBS_3_2018, UMBS_3_2019=UMBS_3_2019, UMBS_3_2020=UMBS_3_2020, UMBS_3_2021=UMBS_3_2021)
list_pairu3 <- lapply(list_pairu3, change_pair_names)
list_pairu3 <- lapply(list_pairu3, change_POSIX)
list_pairu3 <- lapply(list_pairu3, remove_col, name=c('X', 'X..x', 'X..y'))
list_pairu3[2:4] <- lapply(list_pairu3[2:4], f_to_c)
list_pairu3 <- lapply(list_pairu3, remove_outliers)

#Create .RData file
save(list_pairu1, list_pairu2, list_pairu3, file=file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.RData"))
