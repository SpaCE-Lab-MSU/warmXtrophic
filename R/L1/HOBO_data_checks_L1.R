# TITLE:          HOBO paired sensor data checks
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT:     Data imported as clean L1 files from the Google drive
# DATA OUTPUT:    This script plots each HOBO sensor data type over each year to find where data is missing after outlier removal
# PROJECT:        warmXtrophic
# DATE:           May 2022

# clear all existing data
rm(list=ls())

# load in packages
library(tidyverse)

# load in the data
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.csv"))

# adding in year and julian date columns
KBS$year <- format(KBS$Date_Time,format="%Y")
KBS$julian <- format(KBS$Date_Time,format="%j")
UMBS$year <- format(UMBS$Date_Time,format="%Y")
UMBS$julian <- format(UMBS$Date_Time,format="%j")


### checking for missing data
## 10cm air temps
# KBS 2016
KBS_2016_1 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2016_2 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2016_3 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2016_3, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2017
KBS_2017_1 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2017_2 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2017_3 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2017_3, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2018
KBS_2018_1 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2018_2 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2018_3 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2018_3, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## 1m air temps
# KBS 2016
KBS_2016_1 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2016_2 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2016_3 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2016_3, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2017
KBS_2017_1 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2017_2 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2017_3 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2017_2, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## 10cm air temps
# UMBS 2016
UMBS_2016_1 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2016_2 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2016_3 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2016_2, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

