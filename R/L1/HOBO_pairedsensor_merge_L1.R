# TITLE:          HOBO paired sensor data merge
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT:     Data imported as .RData files from shared L1 HOBO_data folder
# DATA OUTPUT:    This script combines the data from the clean HOBO paired sensor .RData file and writes a csv file to the L1 HOBO_paired_sensor_data folder.
    ## csv files are created per site, per sensor (i.e. KBS_pair1_L1.csv)
# PROJECT:        warmXtrophic
# DATE:           July, 2020

# NOTE: Must run HOBO_pairedsensor_cleanup_L1 first

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)

#Load in .RData files
load(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.RData"))
load(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.RData"))

### KBS ###
# Merge data from pair 1 for all years
diff1617_1k <- anti_join(list_pairk1$KBS_1_2017, list_pairk1$KBS_1_1516, by = "Date_Time")
diff1718_1k <- anti_join(list_pairk1$KBS_1_2018, list_pairk1$KBS_1_2017, by = "Date_Time")
diff1819_1k <- anti_join(list_pairk1$KBS_1_2019, list_pairk1$KBS_1_2018, by = "Date_Time")
diff1920_1k <- anti_join(list_pairk1$KBS_1_2020, list_pairk1$KBS_1_2019, by = "Date_Time")
diff2021_1k <- anti_join(list_pairk1$KBS_1_2021, list_pairk1$KBS_1_2020, by = "Date_Time")

KBS_1 <- rbind(list_pairk1$KBS_1_1516, diff1617_1k, diff1718_1k, diff1819_1k, diff1920_1k, diff2021_1k)
write.csv(KBS_1, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair1_L1.csv"))

# Merge data from pair 2 for all years
diff1617_2k <- anti_join(list_pairk2$KBS_2_2017, list_pairk2$KBS_2_1516, by = "Date_Time")
diff1718_2k <- anti_join(list_pairk2$KBS_2_2018, list_pairk2$KBS_2_2017, by = "Date_Time")
diff1819_2k <- anti_join(list_pairk2$KBS_2_2019, list_pairk2$KBS_2_2018, by = "Date_Time")
diff1920_2k <- anti_join(list_pairk2$KBS_2_2020, list_pairk2$KBS_2_2019, by = "Date_Time")
diff2021_2k <- anti_join(list_pairk2$KBS_2_2021, list_pairk2$KBS_2_2020, by = "Date_Time")

KBS_2 <- rbind(list_pairk2$KBS_2_1516, diff1617_2k, diff1718_2k, diff1819_2k, diff1920_2k, diff2021_2k)
write.csv(KBS_2, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair2_L1.csv"))

# Merge data from pair 3 for all years
diff1617_3k <- anti_join(list_pairk3$KBS_3_2017, list_pairk3$KBS_3_1516, by = "Date_Time")
diff1718_3k <- anti_join(list_pairk3$KBS_3_2018, list_pairk3$KBS_3_2017, by = "Date_Time")
diff1819_3k <- anti_join(list_pairk3$KBS_3_2019, list_pairk3$KBS_3_2018, by = "Date_Time")
diff1920_3k <- anti_join(list_pairk3$KBS_3_2020, list_pairk3$KBS_3_2019, by = "Date_Time")
diff2021_3k <- anti_join(list_pairk3$KBS_3_2021, list_pairk3$KBS_3_2020, by = "Date_Time")

KBS_3 <- rbind(list_pairk3$KBS_3_1516, diff1617_3k, diff1718_3k, diff1819_3k, diff1920_3k, diff2021_3k)
write.csv(KBS_3, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair3_L1.csv"))

# create one file for all KBS
KBS_1$site <- "KBS"
KBS_2$site <- "KBS"
KBS_3$site <- "KBS"
KBS_1$sensor <- "1"
KBS_2$sensor <- "2"
KBS_3$sensor <- "3"
KBS <- rbind(KBS_1, KBS_2, KBS_3)
write.csv(KBS, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.csv"))


### UMBS ###
# Merge data from pair 1 for all years
diff1617_1u <- anti_join(list_pairu1$UMBS_1_2017, list_pairu1$UMBS_1_1516, by = "Date_Time")
diff1718_1u <- anti_join(list_pairu1$UMBS_1_2018, list_pairu1$UMBS_1_2017, by = "Date_Time")
diff1819_1u <- anti_join(list_pairu1$UMBS_1_2019, list_pairu1$UMBS_1_2018, by = "Date_Time")
diff1920_1u <- anti_join(list_pairu1$UMBS_1_2020, list_pairu1$UMBS_1_2019, by = "Date_Time")
diff2021_1u <- anti_join(list_pairu1$UMBS_1_2021, list_pairu1$UMBS_1_2020, by = "Date_Time")

UMBS_1 <- rbind(list_pairu1$UMBS_1_1516, diff1617_1u, diff1718_1u, diff1819_1u, diff1920_1u, diff2021_1u)
write.csv(UMBS_1, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair1_L1.csv"))

# Merge data from pair 2 for all years
diff1617_2u <- anti_join(list_pairu2$UMBS_2_2017, list_pairu2$UMBS_2_1516, by = "Date_Time")
diff1718_2u <- anti_join(list_pairu2$UMBS_2_2018, list_pairu2$UMBS_2_2017, by = "Date_Time")
diff1819_2u <- anti_join(list_pairu2$UMBS_2_2019, list_pairu2$UMBS_2_2018, by = "Date_Time")
diff1920_2u <- anti_join(list_pairu2$UMBS_2_2020, list_pairu2$UMBS_2_2019, by = "Date_Time")
diff2021_2u <- anti_join(list_pairu2$UMBS_2_2021, list_pairu2$UMBS_2_2020, by = "Date_Time")

UMBS_2 <- rbind(list_pairu2$UMBS_2_1516, diff1617_2u, diff1718_2u, diff1819_2u, diff1920_2u, diff2021_2u)
write.csv(UMBS_2, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair2_L1.csv"))

# Merge data from pair 3 for all years
diff1617_3u <- anti_join(list_pairu3$UMBS_3_2017, list_pairu3$UMBS_3_1516, by = "Date_Time")
diff1718_3u <- anti_join(list_pairu3$UMBS_3_2018, list_pairu3$UMBS_3_2017, by = "Date_Time")
diff1819_3u <- anti_join(list_pairu3$UMBS_3_2019, list_pairu3$UMBS_3_2018, by = "Date_Time")
diff1920_3u <- anti_join(list_pairu3$UMBS_3_2020, list_pairu3$UMBS_3_2019, by = "Date_Time")
diff2021_3u <- anti_join(list_pairu3$UMBS_3_2021, list_pairu3$UMBS_3_2020, by = "Date_Time")

UMBS_3 <- rbind(list_pairu3$UMBS_3_1516, diff1617_3u, diff1718_3u, diff1819_3u, diff1920_3u, diff2021_3u)
write.csv(UMBS_3, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair3_L1.csv"))

# create one file for all UMBS
UMBS_1$site <- "UMBS"
UMBS_2$site <- "UMBS"
UMBS_3$site <- "UMBS"
UMBS_1$sensor <- "1"
UMBS_2$sensor <- "2"
UMBS_3$sensor <- "3"
UMBS <- rbind(UMBS_1, UMBS_2, UMBS_3)
write.csv(UMBS, file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.csv"))

