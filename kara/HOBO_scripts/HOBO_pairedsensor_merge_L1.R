# TITLE:          HOBO paired sensor data merge
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT:     Data imported as .RData files from shared L1 HOBO_data folder
# DATA OUTPUT:    This script combines the data from the clean HOBO paired sensor .RData file and writes a csv file to the L1 HOBO_paired_sensor_data folder.
    ## csv files are created per site, per sensor (i.e. KBS_pair1_L1.csv)
# PROJECT:        warmXtrophic
# DATE:           July, 2020

# Clear all existing data
rm(list=ls())

#Load packages
for (package in c("tidyverse")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

#Load in .RData files
load("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.RData")
load("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.RData")

### KBS ###
# Merge data from pair 1 for all years
diff1617_1k <- anti_join(list_pairk1$KBS_1_2017, list_pairk1$KBS_1_1516, by = "Date_Time")
diff1718_1k <- anti_join(list_pairk1$KBS_1_2018, list_pairk1$KBS_1_2017, by = "Date_Time")
diff1819_1k <- anti_join(list_pairk1$KBS_1_2019, list_pairk1$KBS_1_2018, by = "Date_Time")
diff1920_1k <- anti_join(list_pairk1$KBS_1_2020, list_pairk1$KBS_1_2019, by = "Date_Time")

KBS_1 <- rbind(list_pairk1$KBS_1_1516, diff1617_1k, diff1718_1k, diff1819_1k, diff1920_1k)
write.csv(KBS_1, file="L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair1_L1.csv")

# Merge data from pair 2 for all years
diff1617_2k <- anti_join(list_pairk2$KBS_2_2017, list_pairk2$KBS_2_1516, by = "Date_Time")
diff1718_2k <- anti_join(list_pairk2$KBS_2_2018, list_pairk2$KBS_2_2017, by = "Date_Time")
diff1819_2k <- anti_join(list_pairk2$KBS_2_2019, list_pairk2$KBS_2_2018, by = "Date_Time")
diff1920_2k <- anti_join(list_pairk2$KBS_2_2020, list_pairk2$KBS_2_2019, by = "Date_Time")

KBS_2 <- rbind(list_pairk2$KBS_2_1516, diff1617_2k, diff1718_2k, diff1819_2k, diff1920_2k)
write.csv(KBS_2, file="L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair2_L1.csv")

# Merge data from pair 3 for all years
diff1617_3k <- anti_join(list_pairk3$KBS_3_2017, list_pairk3$KBS_3_1516, by = "Date_Time")
diff1718_3k <- anti_join(list_pairk3$KBS_3_2018, list_pairk3$KBS_3_2017, by = "Date_Time")
diff1819_3k <- anti_join(list_pairk3$KBS_3_2019, list_pairk3$KBS_3_2018, by = "Date_Time")
diff1920_3k <- anti_join(list_pairk3$KBS_3_2020, list_pairk3$KBS_3_2019, by = "Date_Time")

KBS_3 <- rbind(list_pairk3$KBS_3_1516, diff1617_3k, diff1718_3k, diff1819_3k, diff1920_3k)
write.csv(KBS_3, file="L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair3_L1.csv")


### UMBS ###
# Merge data from pair 1 for all years
diff1617_1u <- anti_join(list_pairu1$UMBS_1_2017, list_pairu1$UMBS_1_1516, by = "Date_Time")
diff1718_1u <- anti_join(list_pairu1$UMBS_1_2018, list_pairu1$UMBS_1_2017, by = "Date_Time")
diff1819_1u <- anti_join(list_pairu1$UMBS_1_2019, list_pairu1$UMBS_1_2018, by = "Date_Time")
diff1920_1u <- anti_join(list_pairu1$UMBS_1_2020, list_pairu1$UMBS_1_2019, by = "Date_Time")

UMBS_1 <- rbind(list_pairu1$UMBS_1_1516, diff1617_1u, diff1718_1u, diff1819_1u, diff1920_1u)
write.csv(UMBS_1, file="L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair1_L1.csv")

# Merge data from pair 2 for all years
diff1617_2u <- anti_join(list_pairu2$UMBS_2_2017, list_pairu2$UMBS_2_1516, by = "Date_Time")
diff1718_2u <- anti_join(list_pairu2$UMBS_2_2018, list_pairu2$UMBS_2_2017, by = "Date_Time")
diff1819_2u <- anti_join(list_pairu2$UMBS_2_2019, list_pairu2$UMBS_2_2018, by = "Date_Time")
diff1920_2u <- anti_join(list_pairu2$UMBS_2_2020, list_pairu2$UMBS_2_2019, by = "Date_Time")

UMBS_2 <- rbind(list_pairu2$UMBS_2_1516, diff1617_2u, diff1718_2u, diff1819_2u, diff1920_2u)
write.csv(UMBS_2, file="L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair2_L1.csv")

# Merge data from pair 3 for all years
diff1617_3u <- anti_join(list_pairu3$UMBS_3_2017, list_pairu3$UMBS_3_1516, by = "Date_Time")
diff1718_3u <- anti_join(list_pairu3$UMBS_3_2018, list_pairu3$UMBS_3_2017, by = "Date_Time")
diff1819_3u <- anti_join(list_pairu3$UMBS_3_2019, list_pairu3$UMBS_3_2018, by = "Date_Time")
diff1920_3u <- anti_join(list_pairu3$UMBS_3_2020, list_pairu3$UMBS_3_2019, by = "Date_Time")

UMBS_3 <- rbind(list_pairu3$UMBS_3_1516, diff1617_3u, diff1718_3u, diff1819_3u, diff1920_3u)
write.csv(UMBS_3, file="L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair3_L1.csv")
