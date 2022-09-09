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
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.csv"))

# convert date columns
KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d %H:%M")

# adding in year and julian date columns
KBS$year <- format(KBS$Date_Time,format="%Y")
KBS$julian <- format(KBS$Date_Time,format="%j")
UMBS$year <- format(UMBS$Date_Time,format="%Y")
UMBS$julian <- format(UMBS$Date_Time,format="%j")


### checking for missing data
## 10cm air temps
# KBS 2016
KBS_2016_1_10 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2016_2_10 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2016_3_10 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
# change sensor number in dataframe name here to see data for each sensor
ggplot(KBS_2016_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2017
KBS_2017_1_10 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2017_2_10 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2017_3_10 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2017_2_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2018
KBS_2018_1_10 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2018_2_10 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2018_3_10 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2018_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2019
KBS_2019_1_10 <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2019_2_10 <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2019_3_10 <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2019_1_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2020
KBS_2020_1_10 <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2020_2_10 <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2020_3_10 <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2020_2_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2021
KBS_2021_1_10 <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2021_2_10 <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2021_3_10 <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2021_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## 1m air temps
# KBS 2016
KBS_2016_1_1 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2016_2_1 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2016_3_1 <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2016_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2017
KBS_2017_1_1 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2017_2_1 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2017_3_1 <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2017_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2018
KBS_2018_1_1 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2018_2_1 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2018_3_1 <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2018_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2019
KBS_2019_1_1 <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2019_2_1 <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2019_3_1 <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2019_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2020
KBS_2020_1_1 <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2020_2_1 <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2020_3_1 <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2020_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2021
KBS_2021_1_1 <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2021_2_1 <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2021_3_1 <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2021_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## soil temps
# KBS 2016
KBS_2016_1_stemp <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2016_2_stemp <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2016_3_stemp <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2016_2_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2017
KBS_2017_1_stemp <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2017_2_stemp <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2017_3_stemp <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2017_2_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2018
KBS_2018_1_stemp <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2018_2_stemp <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2018_3_stemp <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2018_1_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2019
KBS_2019_1_stemp <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2019_2_stemp <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2019_3_stemp <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2019_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2020
KBS_2020_1_stemp <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2020_2_stemp <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2020_3_stemp <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2020_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2021
KBS_2021_1_stemp <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2021_2_stemp <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2021_3_stemp <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2021_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## soil moisture
# KBS 2016
KBS_2016_1_smoist <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2016_2_smoist <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2016_3_smoist <- KBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2016_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2017
KBS_2017_1_smoist <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2017_2_smoist <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2017_3_smoist <- KBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2017_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2018
KBS_2018_1_smoist <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2018_2_smoist <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2018_3_smoist <- KBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2018_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2019
KBS_2019_1_smoist <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2019_2_smoist <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2019_3_smoist <- KBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2019_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2020
KBS_2020_1_smoist <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2020_2_smoist <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2020_3_smoist <- KBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2020_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# KBS 2021
KBS_2021_1_smoist <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
KBS_2021_2_smoist <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
KBS_2021_3_smoist <- KBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(KBS_2021_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## 10cm air temps
# UMBS 2016
UMBS_2016_1_10 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2016_2_10 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2016_3_10 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2016_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2017
UMBS_2017_1_10 <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2017_2_10 <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2017_3_10 <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2017_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2018
UMBS_2018_1_10 <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2018_2_10 <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2018_3_10 <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2018_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2019
UMBS_2019_1_10 <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2019_2_10 <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2019_3_10 <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2019_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2020
UMBS_2020_1_10 <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2020_2_10 <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2020_3_10 <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2020_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2021
UMBS_2021_1_10 <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2021_2_10 <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2021_3_10 <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_ambient_air_10cm, XU_warmed_air_10cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2021_3_10, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## 1m air temps
# UMBS 2016
UMBS_2016_1_1 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2016_2_1 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2016_3_1 <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2016_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2017
UMBS_2017_1_1 <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2017_2_1 <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2017_3_1 <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2017_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2018
UMBS_2018_1_1 <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2018_2_1 <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2018_3_1 <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2018_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2019
UMBS_2019_1_1 <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2019_2_1 <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2019_3_1 <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2019_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2020
UMBS_2020_1_1 <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2020_2_1 <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2020_3_1 <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2020_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2021
UMBS_2021_1_1 <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2021_2_1 <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2021_3_1 <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_warmed_air_1m, XH_ambient_air_1m) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2021_3_1, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## soil temps
# UMBS 2016
UMBS_2016_1_stemp <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2016_2_stemp <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2016_3_stemp <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2016_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2017
UMBS_2017_1_stemp <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2017_2_stemp <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2017_3_stemp <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2017_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2018
UMBS_2018_1_stemp <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2018_2_stemp <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2018_3_stemp <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2018_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2019
UMBS_2019_1_stemp <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2019_2_stemp <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2019_3_stemp <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2019_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2020
UMBS_2020_1_stemp <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2020_2_stemp <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2020_3_stemp <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2020_3_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2021
UMBS_2021_1_stemp <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2021_2_stemp <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2021_3_stemp <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2021_1_stemp, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))


## soil moisture
# UMBS 2016
UMBS_2016_1_smoist <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2016_2_smoist <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2016_3_smoist <- UMBS %>%
        filter(year == 2016) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2016_1_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2017
UMBS_2017_1_smoist <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2017_2_smoist <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2017_3_smoist <- UMBS %>%
        filter(year == 2017) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2017_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2018
UMBS_2018_1_smoist <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2018_2_smoist <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2018_3_smoist <- UMBS %>%
        filter(year == 2018) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2018_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2019
UMBS_2019_1_smoist <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2019_2_smoist <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2019_3_smoist <- UMBS %>%
        filter(year == 2019) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2019_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2020
UMBS_2020_1_smoist <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2020_2_smoist <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2020_3_smoist <- UMBS %>%
        filter(year == 2020) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2020_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))

# UMBS 2021
UMBS_2021_1_smoist <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 1)
UMBS_2021_2_smoist <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 2)
UMBS_2021_3_smoist <- UMBS %>%
        filter(year == 2021) %>%
        select(Date_Time, year, julian, sensor, XH_ambient_soil_moisture_5cm, XH_warmed_soil_moisture_5cm) %>%
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(sensor == 3)
ggplot(UMBS_2021_3_smoist, aes(x=julian, y=temp, group=treatment, color=treatment)) +
        geom_point() +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient 10cm","Warmed 10cm"))
