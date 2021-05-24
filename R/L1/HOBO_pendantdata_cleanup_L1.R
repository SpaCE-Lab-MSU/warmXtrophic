# TITLE:            HOBO L0 pendant data cleanup
# AUTHORS:          Nina Lany (original), Kara Dobson (edited June 2020)
# COLLABORATORS:    Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young, Kathryn Schmidt
# DATA INPUT:       Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:      Makes the following .RData files the L1 HOBO_pendant_data folder:
    ## KBS_HOBOpendant_L1.Rdata
    ## UMBS_HOBOpendant_L1.Rdata
    # These .RData files contain clean, yearly data for each site, which is merged in the HOBO_pendant_data_merge_L1 script
# PROJECT:          warmXtrophic
# DATE:             2018 (initiated)
    ## KD edit June 2020: Updated script to insert 2019 and 2020 data & functions


# Clear all existing data
rm(list=ls())

# Source functions
source("~/warmXtrophic/R/L1/HOBO_functions_L1.R")

# Set working directory
Sys.getenv("L0DIR")
L0_dir<-Sys.getenv("L0DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L0_dir)

# Load packages
library(tidyverse)
library(weathermetrics)

#************************
###*** DATA IMPORT ***###
#************************

### ***KBS*** ###
# Read in KBS HOBO data from all years
                    
pend4P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_4P_09012017.csv"), skip=1, header =T)
pend5P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_5P_09012017.csv"), skip=1, header =T)
pend6P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_6P_09012017.csv"), skip=1, header =T)
pend7P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_7P_09012017.csv"), skip=1, header =T)
pend8P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_8P_09012017.csv"), skip=1, header =T)
pend9P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_9P_09012017.csv"), skip=1, header =T)
pend10P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_10P_09012017.csv"), skip=1, header =T)
pend11P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_11P_09012017.csv"), skip=1, header =T)
pend12P_17k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2017/09_01_2017/KBS_12P_09012017.csv"), skip=1, header =T)

pend4P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_4P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend5P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_5P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend6P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_6P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend7P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_7P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend8P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_8P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend10P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_10P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend11P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_11P_09202018.csv"), skip=1, header =T)[ ,1:4]
pend12P_18k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2018/09_20_2018 (pendants)/KBS_12P_09202018.csv"), skip=1, header =T)[ ,1:4]

pend4P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_4P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend5P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_5P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend6P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_6P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend7P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBs_7P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend8P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_8P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend10P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_10P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend11P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_11P_09232019.csv"), skip=1, header =T)[ ,1:4]
pend12P_19k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2019/09_23_2019/KBS_12P_09232019.csv"), skip=1, header =T)[ ,1:4]

pend4P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_4P_09172020.csv"), skip=1, header =T)[ ,1:4]
pend5P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_5P_09162020.csv"), skip=1, header =T)[ ,1:4]
pend6P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_6P_09162020.csv"), skip=1, header =T)[ ,1:4]
pend7P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_7P_09162020.csv"), skip=1, header =T)[ ,1:4]
pend8P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_8P_09172020.csv"), skip=1, header =T)[ ,1:4]
pend10P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_10P_09162020.csv"), skip=1, header =T)[ ,1:4]
pend11P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_11P_09172020.csv"), skip=1, header =T)[ ,1:4]
pend12P_20k<-read.csv(file.path(L0_dir,"KBS/sensor_data/2020/09_17_2020/KBS_12P_09172020.csv"), skip=1, header =T)[ ,1:4]

# Apply functions
list_k <- list(pend4P_17k=pend4P_17k,pend5P_17k=pend5P_17k,pend6P_17k=pend6P_17k,pend7P_17k=pend7P_17k,pend8P_17k=pend8P_17k,pend9P_17k=pend9P_17k,pend10P_17k=pend10P_17k,pend11P_17k=pend11P_17k,pend12P_17k=pend12P_17k,
               pend4P_18k=pend4P_18k,pend5P_18k=pend5P_18k,pend6P_18k=pend6P_18k,pend7P_18k=pend7P_18k,pend8P_18k=pend8P_18k,pend10P_18k=pend10P_18k,pend11P_18k=pend11P_18k,pend12P_18k=pend12P_18k,
               pend4P_19k=pend4P_19k,pend5P_19k=pend5P_19k,pend6P_19k=pend6P_19k,pend7P_19k=pend7P_19k,pend8P_19k=pend8P_19k,pend10P_19k=pend10P_19k,pend11P_19k=pend11P_19k,pend12P_19k=pend12P_19k,
               pend4P_20k=pend4P_20k,pend5P_20k=pend5P_20k,pend6P_20k=pend6P_20k,pend7P_20k=pend7P_20k,pend8P_20k=pend8P_20k,pend10P_20k=pend10P_20k,pend11P_20k=pend11P_20k,pend12P_20k=pend12P_20k)
list_k <- lapply(list_k, change_pend_names)
list_k[1:25] <- lapply(list_k[1:25], change_POSIX)
list_k[26:33] <- lapply(list_k[26:33], change_POSIX2)
list_k[1:25] <- lapply(list_k[1:25], f_to_c2)
list_k <- add_name_cols(list_k)

# Combine KBS pendant files
pend17k<-rbind(list_k$pend4P_17k,list_k$pend5P_17k,list_k$pend6P_17k,list_k$pend7P_17k,list_k$pend8P_17k,list_k$pend9P_17k,list_k$pend10P_17k,list_k$pend11P_17k,list_k$pend12P_17k)
pend18k<-rbind(list_k$pend4P_18k,list_k$pend5P_18k,list_k$pend6P_18k,list_k$pend7P_18k,list_k$pend8P_18k,list_k$pend10P_18k,list_k$pend11P_18k,list_k$pend12P_18k)
pend19k<-rbind(list_k$pend4P_19k,list_k$pend5P_19k,list_k$pend6P_19k,list_k$pend7P_19k,list_k$pend8P_19k,list_k$pend10P_19k,list_k$pend11P_19k,list_k$pend12P_19k)
pend20k<-rbind(list_k$pend4P_20k,list_k$pend5P_20k,list_k$pend6P_20k,list_k$pend7P_20k,list_k$pend8P_20k,list_k$pend10P_20k,list_k$pend11P_20k,list_k$pend12P_20k)
pend17k$Site<-"KBS"
pend18k$Site<-"KBS"
pend19k$Site<-"KBS"
pend20k$Site<-"KBS"

#Create RData save file - this is used in the script that merges all of the clean data together
save(pend17k, pend18k, pend19k, pend20k, file=file.path(L1_dir,"HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.RData"))


### ***UMBS*** ###
# Read in UMBS HOBO data from all years
pend4P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_4P_08152017.csv"), skip=1, header =T)
pend5P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_5P_08152017.csv"), skip=1, header =T)
pend6P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_6P_08152017.csv"), skip=1, header =T)
pend7P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_7P_08152017.csv"), skip=1, header =T)
pend8P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_8P_08152017.csv"), skip=1, header =T)
pend9P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_9P_08152017.csv"), skip=1, header =T)
pend10P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_10P_08152017.csv"), skip=1, header =T)
pend11P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_11P_08152017.csv"), skip=1, header =T)
pend12P_17u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2017/08_15_2017/UMBS_12P_08152017.csv"), skip=1, header =T)

pend4P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_4P_09192018.csv"), skip=1, header =T)
pend5P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_5P_09192018.csv"), skip=1, header =T)
pend6P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_6P_09192018.csv"), skip=1, header =T)
pend7P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_7P_09192018.csv"), skip=1, header =T)
pend8P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_8P_09192018.csv"), skip=1, header =T)
pend9P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_9P_09192018.csv"), skip=1, header =T)
pend10P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_10P_09192018.csv"), skip=1, header =T)
pend11P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_11P_09192018.csv"), skip=1, header =T)
pend12P_18u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2018/09_19_2018/UMBS_12P_09192018.csv"), skip=1, header =T)

pend4P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_4P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend5P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_5P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend6P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_6P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend7P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_7P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend8P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_8P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend9P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_9P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend10P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_10P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend11P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_11P_09052019.csv"), skip=1, header =T)[ ,1:4]
pend12P_19u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2019/09_05_2019/UMBS_12P_09052019.csv"), skip=1, header =T)[ ,1:4]

pend4P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_4P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend5P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_5P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend6P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_6P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend7P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_7P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend8P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_8P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend9P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_9P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend10P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_10P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend11P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_11P_20200831.csv"), skip=1, header =T)[ ,1:4]
pend12P_20u<-read.csv(file.path(L0_dir,"UMBS/sensor_data/2020/08_31_2020/UMBS_12P_20200831.csv"), skip=1, header =T)[ ,1:4]

#Manually change 10p column names (they don't match the names of the others)
names(pend10P_17u)[names(pend10P_17u)=="Temp...F..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_17u)[names(pend10P_17u)=="Intensity..lum.ft...LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_18u)[names(pend10P_18u)=="Temp...F..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_18u)[names(pend10P_18u)=="Intensity..lum.ft...LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_19u)[names(pend10P_19u)=="Temp...F..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_19u)[names(pend10P_19u)=="Intensity..lum.ft...LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"
names(pend10P_20u)[names(pend10P_20u)=="Temp...C..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_air_warmed_1m."] <- "Temp_F_XP_air_1m"
names(pend10P_20u)[names(pend10P_20u)=="Intensity..Lux..LGR.S.N..10747441..SEN.S.N..10747441..LBL..B6_light_warmed_1m."] <- "Intensity_lum_ft_XP_light_1m"

# Apply functions
list_u <- list(pend4P_17u=pend4P_17u,pend5P_17u=pend5P_17u,pend6P_17u=pend6P_17u,pend7P_17u=pend7P_17u,pend8P_17u=pend8P_17u,pend9P_17u=pend9P_17u,pend10P_17u=pend10P_17u,pend11P_17u=pend11P_17u,pend12P_17u=pend12P_17u,
                  pend4P_18u=pend4P_18u,pend5P_18u=pend5P_18u,pend6P_18u=pend6P_18u,pend7P_18u=pend7P_18u,pend8P_18u=pend8P_18u,pend10P_18u=pend10P_18u,pend11P_18u=pend11P_18u,pend12P_18u=pend12P_18u,
                  pend4P_19u=pend4P_19u,pend5P_19u=pend5P_19u,pend6P_19u=pend6P_19u,pend7P_19u=pend7P_19u,pend8P_19u=pend8P_19u,pend10P_19u=pend10P_19u,pend11P_19u=pend11P_19u,pend12P_19u=pend12P_19u,
                  pend4P_20u=pend4P_20u,pend5P_20u=pend5P_20u,pend6P_20u=pend6P_20u,pend7P_20u=pend7P_20u,pend8P_20u=pend8P_20u,pend10P_20u=pend10P_20u,pend11P_20u=pend11P_20u,pend12P_20u=pend12P_20u)
list_u <- lapply(list_u, change_pend_names)
list_u <- lapply(list_u, change_POSIX)
list_u[1:25] <- lapply(list_u[1:25], f_to_c2)
list_u <- add_name_cols(list_u)

# Combine UMBS pendant files for 2017 and 2018
pend17u<-rbind(list_u$pend4P_17u,list_u$pend5P_17u,list_u$pend6P_17u,list_u$pend7P_17u,list_u$pend8P_17u,list_u$pend9P_17u,list_u$pend10P_17u,list_u$pend11P_17u,list_u$pend12P_17u) 
pend18u<-rbind(list_u$pend4P_18u,list_u$pend5P_18u,list_u$pend6P_18u,list_u$pend7P_18u,list_u$pend8P_18u,list_u$pend9P_18u,list_u$pend10P_18u,list_u$pend11P_18u,list_u$pend12P_18u)
pend19u<-rbind(list_u$pend4P_19u,list_u$pend5P_19u,list_u$pend6P_19u,list_u$pend7P_19u,list_u$pend8P_19u,list_u$pend9P_19u,list_u$pend10P_19u,list_u$pend11P_19u,list_u$pend12P_19u)
pend20u<-rbind(list_u$pend4P_20u,list_u$pend5P_20u,list_u$pend6P_20u,list_u$pend7P_20u,list_u$pend8P_20u,list_u$pend9P_20u,list_u$pend10P_20u,list_u$pend11P_20u,list_u$pend12P_20u)
pend17u$Site<-"UMBS"
pend18u$Site<-"UMBS"
pend19u$Site<-"UMBS"
pend20u$Site<-"UMBS"
 
#Create RData save file:
# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
save(pend17u, pend18u, pend19u, pend20u, file=file.path(L1_dir,"HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.RData"))
