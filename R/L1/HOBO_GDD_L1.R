# TITLE: warmXtrophic: OTC GDD calculations
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the L1 hobo folder in the shared Google drive
# DATA OUTPUT: Dataframe with a column for cumulative GDD and average temps for each site + year
# PROJECT: warmXtrophic
# DATE: Nov 2022

# Clear all existing data
rm(list=ls())

# read in packages
library(tidyverse)

# read in HOBO data
L1_dir<-Sys.getenv("L1DIR")
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_dataremoved_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_dataremoved_L1.csv"))

# date is a character column - convert to date format
KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d %H:%M")

# add in columns with date information
KBS_dates <- KBS
KBS_dates$julian <- format(KBS_dates$Date_Time, "%j")
UMBS_dates <- UMBS
UMBS_dates$julian <- format(UMBS_dates$Date_Time, "%j")

# remove all columns except 1m air temps + date info
KBS_dates <-dplyr::select(KBS_dates, -c('X',
                                        'XH_warmed_soil_moisture_5cm','XH_ambient_soil_moisture_5cm',
                                        'XH_warmed_RH_1m','XH_ambient_RH_1m',
                                        'XU_warmed_air_10cm','XU_ambient_air_10cm',
                                        'XU_warmed_soil_temp_5cm','XU_ambient_soil_temp_5cm',
                                        'site'))
UMBS_dates <-dplyr::select(UMBS_dates, -c('X',
                                       'XH_warmed_soil_moisture_5cm','XH_ambient_soil_moisture_5cm',
                                       'XH_warmed_RH_1m','XH_ambient_RH_1m',
                                       'XU_warmed_air_10cm','XU_ambient_air_10cm',
                                       'XU_warmed_soil_temp_5cm','XU_ambient_soil_temp_5cm',
                                       'site'))

# take calculations for each year+treatment
KBS_GDD_calc <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        group_by(year, julian, treatment) %>%
        summarize(max_temp = max(temp, na.rm = TRUE),
                  min_temp = min(temp, na.rm=TRUE),
                  mean_GDD_temp = (max_temp+min_temp)/2, # using these guidelines: https://www.canr.msu.edu/uploads/files/Research_Center/NW_Mich_Hort/General/CalculatingGrowingDegreeDays.pdf
                  GDD_base_10 = mean_GDD_temp-10) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_GDD_calc <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        group_by(year, julian, treatment) %>%
        summarize(max_temp = max(temp, na.rm = TRUE),
                  min_temp = min(temp, na.rm=TRUE),
                  mean_GDD_temp = (max_temp+min_temp)/2, # using these guidelines: https://www.canr.msu.edu/uploads/files/Research_Center/NW_Mich_Hort/General/CalculatingGrowingDegreeDays.pdf
                  GDD_base_10 = mean_GDD_temp-10) %>% # using a base temp of 10C, commonly used for plants
        na.omit()

# setting all negative GDD to 0
KBS_GDD_calc$GDD_base_10 <- ifelse(KBS_GDD_calc$GDD_base_10 < 0, 0, KBS_GDD_calc$GDD_base_10)
UMBS_GDD_calc$GDD_base_10 <- ifelse(UMBS_GDD_calc$GDD_base_10 < 0, 0, UMBS_GDD_calc$GDD_base_10)

# column for cumulative GDD per year
# this can be used for all response variables:
# determine the julian "end date" of that response variable, and look in this dataframe to see
# the cumulative number of GDD by that end date for each year + treatment
KBS_GDD <- KBS_GDD_calc %>%
        group_by(year, treatment) %>%
        mutate(GDD_cumulative = cumsum(GDD_base_10))
UMBS_GDD <- UMBS_GDD_calc %>%
        group_by(year, treatment) %>%
        mutate(GDD_cumulative = cumsum(GDD_base_10))


## new dataframes for temp mean, median, and max for each year + treatment
# specific for each response variable

## notes from meeting:
# for phenology, determine range for each as spring equinox -> date of latest date for each phenological event
# may have to be a bit earlier than the spring equinox to account for green-up
# note: using julian day 59 and 96 instead of spring equinox because the earliest recorded green-up occurs on 59 at KBS, and 96 at UMBS
# for herbivory & plant comps: determine date range of beginning of season (earliest green-up) -> latest record for each variable


### phenology ###
## latest occurring date of first green-up: KBS 178, UMBS 167 (determined in phenology_dates_L2.R)
# (not using latest date of green-up because it is recorded as % cover all season)

## latest occurring date of first flowering: KBS 262, UMBS 252 (determined in phenology_dates_L2.R)

## latest occurring date of first seed set: KBS 283, UMBS 245 (determined in phenology_dates_L2.R)
# (UMBS seed set date is earlier than flower data bc a species was recorded as flowering for the first time later
# than a species was recorded setting seed for the first time)


### herbivory ###
## latest record for herbivory: KBS 291, UMBS 255 (determined in herbivory_analyses_L2.R)


### C & N ###
## using the same latest record as herbivory, since C & N date data is not well documented
## C & N leaf harvest was also an end-season measurement, similar to herbivory


### plant comp ###
## latest record for max plant comp: 237 KBS, UMBS 209 (determined in plant_comp_data_wrangling_L2.R)



## calculations
# green-up:
KBS_avg_greenup <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(julian > "059", julian < "178") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_avg_greenup <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        filter(julian > "096", julian < "167") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()

# flowering:
KBS_avg_flower <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(julian > "059", julian < "262") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_avg_flower <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        filter(julian > "096", julian < "252") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()

# seed set:
KBS_avg_seed <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(julian > "059", julian < "283") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_avg_seed <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        filter(julian > "096", julian < "245") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()

# herbivory:
KBS_avg_herb <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(julian > "059", julian < "291") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_avg_herb <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        filter(julian > "096", julian < "255") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()


# C & N:
KBS_avg_CN <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(julian > "059", julian < "291") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_avg_CN <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        filter(julian > "096", julian < "255") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()


# plant comp:
KBS_avg_comp <- KBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -julian, -Date_Time, -sensor) %>%
        filter(julian > "059", julian < "237") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()
UMBS_avg_comp <- UMBS_dates %>%  # by year, 1m temp only
        gather(key = "treatment", value = "temp", -year, -month, -julian, -Date_Time, -sensor) %>%
        filter(julian > "096", julian < "209") %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm=T),
                  median_temp = median(temp, na.rm=T),
                  max_temp = max(temp, na.rm=T)) %>% # using a base temp of 10C, commonly used for plants
        na.omit()


# fixing column names
names(KBS_GDD)[names(KBS_GDD) == 'treatment'] <- 'state'
names(KBS_avg_greenup)[names(KBS_avg_greenup) == 'treatment'] <- 'state'
names(KBS_avg_flower)[names(KBS_avg_flower) == 'treatment'] <- 'state'
names(KBS_avg_seed)[names(KBS_avg_seed) == 'treatment'] <- 'state'
names(KBS_avg_herb)[names(KBS_avg_herb) == 'treatment'] <- 'state'
names(KBS_avg_CN)[names(KBS_avg_CN) == 'treatment'] <- 'state'
names(KBS_avg_comp)[names(KBS_avg_comp) == 'treatment'] <- 'state'

names(UMBS_GDD)[names(UMBS_GDD) == 'treatment'] <- 'state'
names(UMBS_avg_greenup)[names(UMBS_avg_greenup) == 'treatment'] <- 'state'
names(UMBS_avg_flower)[names(UMBS_avg_flower) == 'treatment'] <- 'state'
names(UMBS_avg_seed)[names(UMBS_avg_seed) == 'treatment'] <- 'state'
names(UMBS_avg_herb)[names(UMBS_avg_herb) == 'treatment'] <- 'state'
names(UMBS_avg_CN)[names(UMBS_avg_CN) == 'treatment'] <- 'state'
names(UMBS_avg_comp)[names(UMBS_avg_comp) == 'treatment'] <- 'state'

# fixing state treatment names
KBS_GDD$state[KBS_GDD$state == "XH_ambient_air_1m"] <- "ambient"
KBS_GDD$state[KBS_GDD$state == "XH_warmed_air_1m"] <- "warmed"
KBS_avg_greenup$state[KBS_avg_greenup$state == "XH_ambient_air_1m"] <- "ambient"
KBS_avg_greenup$state[KBS_avg_greenup$state == "XH_warmed_air_1m"] <- "warmed"
KBS_avg_flower$state[KBS_avg_flower$state == "XH_ambient_air_1m"] <- "ambient"
KBS_avg_flower$state[KBS_avg_flower$state == "XH_warmed_air_1m"] <- "warmed"
KBS_avg_seed$state[KBS_avg_seed$state == "XH_ambient_air_1m"] <- "ambient"
KBS_avg_seed$state[KBS_avg_seed$state == "XH_warmed_air_1m"] <- "warmed"
KBS_avg_herb$state[KBS_avg_herb$state == "XH_ambient_air_1m"] <- "ambient"
KBS_avg_herb$state[KBS_avg_herb$state == "XH_warmed_air_1m"] <- "warmed"
KBS_avg_CN$state[KBS_avg_CN$state == "XH_ambient_air_1m"] <- "ambient"
KBS_avg_CN$state[KBS_avg_CN$state == "XH_warmed_air_1m"] <- "warmed"
KBS_avg_comp$state[KBS_avg_comp$state == "XH_ambient_air_1m"] <- "ambient"
KBS_avg_comp$state[KBS_avg_comp$state == "XH_warmed_air_1m"] <- "warmed"

UMBS_GDD$state[UMBS_GDD$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_GDD$state[UMBS_GDD$state == "XH_warmed_air_1m"] <- "warmed"
UMBS_avg_greenup$state[UMBS_avg_greenup$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_avg_greenup$state[UMBS_avg_greenup$state == "XH_warmed_air_1m"] <- "warmed"
UMBS_avg_flower$state[UMBS_avg_flower$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_avg_flower$state[UMBS_avg_flower$state == "XH_warmed_air_1m"] <- "warmed"
UMBS_avg_seed$state[UMBS_avg_seed$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_avg_seed$state[UMBS_avg_seed$state == "XH_warmed_air_1m"] <- "warmed"
UMBS_avg_herb$state[UMBS_avg_herb$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_avg_herb$state[UMBS_avg_herb$state == "XH_warmed_air_1m"] <- "warmed"
UMBS_avg_CN$state[UMBS_avg_CN$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_avg_CN$state[UMBS_avg_CN$state == "XH_warmed_air_1m"] <- "warmed"
UMBS_avg_comp$state[UMBS_avg_comp$state == "XH_ambient_air_1m"] <- "ambient"
UMBS_avg_comp$state[UMBS_avg_comp$state == "XH_warmed_air_1m"] <- "warmed"

# add site column and merge
KBS_GDD$site <- "kbs"
UMBS_GDD$site <- "umbs"
GDD <- rbind(KBS_GDD,UMBS_GDD)
KBS_avg_greenup$site <- "kbs"
UMBS_avg_greenup$site <- "umbs"
greenup_temps <- rbind(KBS_avg_greenup, UMBS_avg_greenup)
KBS_avg_flower$site <- "kbs"
UMBS_avg_flower$site <- 'umbs'
flower_temps <- rbind(KBS_avg_flower, UMBS_avg_flower)
KBS_avg_seed$site <- "kbs"
UMBS_avg_seed$site <- 'umbs'
seed_temps <- rbind(KBS_avg_seed, UMBS_avg_seed)
KBS_avg_herb$site <- "kbs"
UMBS_avg_herb$site <- 'umbs'
herb_temps <- rbind(KBS_avg_herb, UMBS_avg_herb)
KBS_avg_CN$site <- "kbs"
UMBS_avg_CN$site <- 'umbs'
CN_temps <- rbind(KBS_avg_CN, UMBS_avg_CN)
KBS_avg_comp$site <- "kbs"
UMBS_avg_comp$site <- 'umbs'
comp_temps <- rbind(KBS_avg_comp, UMBS_avg_comp)

# save dataframes to L1 folder
# KBS specific
write.csv(KBS_GDD, file.path(L1_dir,"HOBO_data/KBS_GDD_L1.csv"), row.names = F)
write.csv(KBS_avg_greenup, file.path(L1_dir,"HOBO_data/KBS_greenup_temps_L1.csv"), row.names = F)
write.csv(KBS_avg_flower, file.path(L1_dir,"HOBO_data/KBS_flower_temps_L1.csv"), row.names = F)
write.csv(KBS_avg_seed, file.path(L1_dir,"HOBO_data/KBS_seed_temps_L1.csv"), row.names = F)
write.csv(KBS_avg_herb, file.path(L1_dir,"HOBO_data/KBS_herb_temps_L1.csv"), row.names = F)
write.csv(KBS_avg_CN, file.path(L1_dir,"HOBO_data/KBS_CN_temps_L1.csv"), row.names = F)
write.csv(KBS_avg_comp, file.path(L1_dir,"HOBO_data/KBS_plant_comp_temps_L1.csv"), row.names = F)

# UMBS specific
write.csv(UMBS_GDD, file.path(L1_dir,"HOBO_data/UMBS_GDD_L1.csv"), row.names = F)
write.csv(UMBS_avg_greenup, file.path(L1_dir,"HOBO_data/UMBS_greenup_temps_L1.csv"), row.names = F)
write.csv(UMBS_avg_flower, file.path(L1_dir,"HOBO_data/UMBS_flower_temps_L1.csv"), row.names = F)
write.csv(UMBS_avg_seed, file.path(L1_dir,"HOBO_data/UMBS_seed_temps_L1.csv"), row.names = F)
write.csv(UMBS_avg_herb, file.path(L1_dir,"HOBO_data/UMBS_herb_temps_L1.csv"), row.names = F)
write.csv(UMBS_avg_CN, file.path(L1_dir,"HOBO_data/UMBS_CN_temps_L1.csv"), row.names = F)
write.csv(UMBS_avg_comp, file.path(L1_dir,"HOBO_data/UMBS_plant_comp_temps_L1.csv"), row.names = F)

# both sites
write.csv(GDD, file.path(L1_dir,"HOBO_data/GDD_L1.csv"), row.names = F)
write.csv(greenup_temps, file.path(L1_dir,"HOBO_data/greenup_temps_L1.csv"), row.names = F)
write.csv(flower_temps, file.path(L1_dir,"HOBO_data/flower_temps_L1.csv"), row.names = F)
write.csv(seed_temps, file.path(L1_dir,"HOBO_data/seed_temps_L1.csv"), row.names = F)
write.csv(herb_temps, file.path(L1_dir,"HOBO_data/herb_temps_L1.csv"), row.names = F)
write.csv(CN_temps, file.path(L1_dir,"HOBO_data/CN_temps_L1.csv"), row.names = F)
write.csv(comp_temps, file.path(L1_dir,"HOBO_data/plant_comp_temps_L1.csv"), row.names = F)


          