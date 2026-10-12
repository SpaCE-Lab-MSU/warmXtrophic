# TITLE:          2025 HOBO paired sensor cleanup
# AUTHORS:        Moriah Young

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(weathermetrics)
library(lubridate)

# Set working directory
Sys.getenv("L0DIR")
L0_dir<-Sys.getenv("L0DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L0_dir)
list.files(L1_dir)

# Source functions
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/HOBO_functions_L1.R")


# paired sensors have: 10cm warmed air, 5cm warmed soil, 5cm ambient soil, 10cm ambient air

# paired sensors have: 5cm ambient soil moisture, 5cm warmed soil moisture, 1m ambient air temp, 1m ambient RH, 1m warmed air temp, 1m warmed RH

#Change column names for paired sensor cleanup
change_pair_names <- function(df){
        colnames(df) <- sub("^X\\dU_warmed_soil_5cm", "XU_warmed_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^X\\dU_ambient_soil_5cm", "XU_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^X\\dH_warmed_soil_moisture_5cm", "XH_warmed_soil_moisture_5cm", colnames(df))
        colnames(df) <- sub("^X\\dH_ambient_soil_moisture_5cm", "XH_ambient_soil_moisture_5cm", colnames(df))
        colnames(df) <- sub("^X\\dH_warmed_air_1m", "XH_warmed_air_1m", colnames(df))
        colnames(df) <- sub("^X\\dH_warmed_RH_1m", "XH_warmed_RH_1m", colnames(df))
        colnames(df) <- sub("^X\\dH_ambient_air_1m", "XH_ambient_air_1m", colnames(df))
        colnames(df) <- sub("^X\\dH_ambient_RH_1m", "XH_ambient_RH_1m", colnames(df))
        colnames(df) <- sub("^X\\dU_warmed_air_10cm", "XU_warmed_air_10cm", colnames(df))
        colnames(df) <- sub("^X\\dU_ambient_air_10cm", "XU_ambient_air_10cm", colnames(df))
        colnames(df) <- sub("^Date.Time..GMT.0\\d.00", "Date_Time", colnames(df))
        colnames(df) <- sub("^Water.Content.*warmed_soil_moisture_5cm.", "XH_warmed_soil_moisture_5cm", colnames(df))
        colnames(df) <- sub("^Water.Content.*ambient_soil_moisture_5cm.", "XH_ambient_soil_moisture_5cm", colnames(df))
        colnames(df) <- sub("^Temp.*warmed_air_1m.", "XH_warmed_air_1m", colnames(df))
        colnames(df) <- sub("^Temp.*ambient_air_1m.", "XH_ambient_air_1m", colnames(df))
        colnames(df) <- sub("^RH.*warmed_RH_1m.", "XH_warmed_RH_1m", colnames(df))
        colnames(df) <- sub("^RH.*ambient_RH_1m.", "XH_ambient_RH_1m", colnames(df))
        colnames(df) <- sub("^Temp.*warmed_air_10cm.", "XU_warmed_air_10cm", colnames(df))
        colnames(df) <- sub("^Temp.*warmed_soil_5cm.", "XU_warmed_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp.*ambient_soil_5cm.", "XU_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp.*ambient_air_10cm.", "XU_ambient_air_10cm", colnames(df))
        colnames(df) <- sub("^Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm.", "XU_warmed_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm.", "XU_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_warmed_soil_temp_5cm.", "XU_warmed_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^X1U_ambient_soil_temp_5cm", "XU_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^X1U_warmed_soil_temp_5cm", "XU_warmed_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp...C..LGR.S.N..10737620..SEN.S.N..10737620..LBL..1U_ambient_soil_temp_5cm.", "XU_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Water.Content..m..m...LGR.S.N..10736967..SEN.S.N..10736061..LBL..2H_ambient_soil_moist_5cm.", "XU_ambient_soil_moisture_5cm", colnames(df))
        colnames(df) <- sub("^Water.Content..m..m...LGR.S.N..10736968..SEN.S.N..10736054..LBL..3H_ambient_soil_moistire_5cm.", "XU_ambient_soil_moisture_5cm", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_warmed_soil_temp_5cm.", "XU_warmed_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10737624..SEN.S.N..10737624..LBL..3U_ambient_soil_temp_5cm.", "XU_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10910775..SEN.S.N..10737461..LBL..2H_ambient_aim_1m.", "XH_ambient_air_1m", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_ambient_soil_temp_5cm.", "XH_ambient_soil_temp_5cm", colnames(df))
        colnames(df) <- sub("^Temp...F..LGR.S.N..10737621..SEN.S.N..10737621..LBL..2U_warmed_soil_temp_5cm.", "XH_warmed_soil_temp_5cm", colnames(df))
        return(df)
}

remove_battery_cols <- function(df) {
        df[, !grepl("^(Bad\\.Battery|Good\\.Battery|End\\.Of\\.File|Host\\.Connected)",
                    names(df))]
}

#######################################################################
#    KBS
#######################################################################

# note: data read in is often cumulative, so for example data from 2018 may contain data from 2016 and 2017 also
# the data are merged with this duplicate data across years removed in the "merge_L1" scripts

############ KBS Pair 1
#Read in data from H pendants
# no 1H for 2025
KBS_2H_2025 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2025 and 2026/KBS 2025 WarmX/non pendant data/csv files/2H_kbs_102026.csv"), skip=1)
KBS_3H_2025 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2025 and 2026/KBS 2025 WarmX/non pendant data/csv files/3H_kbs_102026.csv"), skip=1)

#Read in data from U pendants
KBS_1U_2025 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2025 and 2026/KBS 2025 WarmX/non pendant data/csv files/1U_kbs_102026.csv"), skip=1)
KBS_2U_2025 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2025 and 2026/KBS 2025 WarmX/non pendant data/csv files/2U_kbs_102026.csv"), skip=1)
KBS_3U_2025 <- read.csv(file.path(L0_dir,"KBS/sensor_data/2025 and 2026/KBS 2025 WarmX/non pendant data/csv files/3U_kbs_102026.csv"), skip=1)

# Merge H and U data for those that have both
KBS_2_2025 <- merge(KBS_2H_2025, KBS_2U_2025, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_3_2025 <- merge(KBS_3H_2025, KBS_3U_2025, by="Date.Time..GMT.04.00", all.x=T, all.y=T)
KBS_1_2025 <- KBS_1U_2025

# Apply functions
list_pairk1 <- list(KBS_1_2025=KBS_1_2025, KBS_2_2025=KBS_2_2025, KBS_3_2025=KBS_3_2025)
list_pairk1 <- lapply(list_pairk1, change_pair_names)
list_pairk1 <- lapply(list_pairk1, change_POSIX)
list_pairk1 <- lapply(list_pairk1, remove_col, name=c('X', 'X..x', 'X..y', 'X.'))
list_pairk1 <- lapply(list_pairk1, remove_battery_cols)

# check how the change_pair_names function did
unique(unlist(lapply(list_pairk1, names)))

names(list_pairk1)

# convert data that was in F to C
list_pairk1[c(1:3)] <- lapply(list_pairk1[c(1:3)], f_to_c)
#list_pairu1[2:4] <- lapply(list_pairu1[2:4], f_to_c)

#list_pairk1 <- lapply(list_pairk1, remove_outliers)

# create a dataframe and inspect
pairk1_df <- dplyr::bind_rows(list_pairk1)

#######################################################################
#    UMBS
#######################################################################
#Read in data from H pendants
UMBS_2H_2025 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2025/UMBS November 2025 csv files/UMBS_2H.csv"), skip=1)

#Read in data from U pendants
UMBS_1U_2025 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2025/UMBS November 2025 csv files/UMBS_1U.csv"), skip=1)
UMBS_2U_2025 <- read.csv(file.path(L0_dir,"UMBS/sensor_data/2025/UMBS November 2025 csv files/UMBS_2U.csv"), skip=1)

# Merge H and U data for those that have both
UMBS_2_2025 <- merge(UMBS_2H_2025, UMBS_2U_2025, by="Date.Time..GMT.05.00", all.x=T, all.y=T)
UMBS_1_2025 <- UMBS_1U_2025

# Apply functions
list_pairu1 <- list(UMBS_1_2025=UMBS_1_2025, UMBS_2_2025=UMBS_2_2025)
list_pairu1 <- lapply(list_pairu1, change_pair_names)
list_pairu1 <- lapply(list_pairu1, change_POSIX)
list_pairu1 <- lapply(list_pairu1, remove_col, name=c('X', 'X..x', 'X..y', 'X.'))
list_pairu1 <- lapply(list_pairu1, remove_battery_cols)

# check how the change_pair_names function did
unique(unlist(lapply(list_pairu1, names)))

names(list_pairu1)

# convert data that was in F to C
list_pairu1[c(1:2)] <- lapply(list_pairu1[c(1:2)], f_to_c)
#list_pairu1[2:4] <- lapply(list_pairu1[2:4], f_to_c)

#list_pairk1 <- lapply(list_pairk1, remove_outliers)

# create a dataframe and inspect
pairu1_df <- dplyr::bind_rows(list_pairu1)


################################################################################

write.csv(pairu1_df, file.path(L1_dir, "/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensor_2025_L1.csv"), row.names=F)
write.csv(pairk1_df, file.path(L1_dir, "/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensor_2025_L1.csv"), row.names=F)


################################################################################
kbs_air <- pairk1_df %>% 
        select(c("Date_Time", "XH_ambient_air_1m"))
kbs_air <- na.omit(kbs_air)

umbs_air <- pairu1_df %>% 
        select(c("Date_Time", "XH_ambient_air_1m"))
umbs_air <- na.omit(umbs_air)



kbs_air$month <- format(kbs_air$Date_Time,format="%m")
kbs_air$year <- format(kbs_air$Date_Time,format="%Y")
kbs_air$hour <- format(kbs_air$Date_Time, format="%H")

kbs_season_air <- kbs_air %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XH_ambient_air_1m)

kbs_avg_month <- kbs_season_air %>%  # by month and year, 1m temp only
        group_by(month, year) %>%
        summarize(average_temp = mean(XH_ambient_air_1m, na.rm = TRUE),
                  se = std.error(XH_ambient_air_1m, na.rm = TRUE))

umbs_air$month <- format(umbs_air$Date_Time,format="%m")
umbs_air$year <- format(umbs_air$Date_Time,format="%Y")
umbs_air$hour <- format(umbs_air$Date_Time, format="%H")

umbs_season_air <- umbs_air %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XH_ambient_air_1m)

umb_avg_month <- umbs_season_air %>%  # by month and year, 1m temp only
        group_by(month, year) %>%
        summarize(average_temp = mean(XH_ambient_air_1m, na.rm = TRUE),
                  se = std.error(XH_ambient_air_1m, na.rm = TRUE))

