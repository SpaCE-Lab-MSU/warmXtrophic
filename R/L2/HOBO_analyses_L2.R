# TITLE: OTC data analysis
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the HOBO_data folder in the shared Google drive
# DATA OUTPUT: Stats results
# PROJECT: warmXtrophic
# DATE: July 2020

# clear all existing data
rm(list=ls())

# load in packages and set working directory
library(tidyverse)
library(plotrix)
library(ggpubr)
library(rstatix)

# Set working directory
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)

# load in the data
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_L1.csv"))

KBS_par <- read.csv(file.path(L1_dir,"PAR_data/KBS_PAR_L1.csv"))
UMBS_par <- read.csv(file.path(L1_dir,"PAR_data/UMBS_PAR_L1.csv"))

KBS_pend <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.csv"))
UMBS_pend <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.csv"))

str(KBS)
str(UMBS)

# date is a character column - convert to date format

KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d %H:%M")
str(KBS)
str(UMBS)

#########################################
# KBS #
#########################################



###### testing for sig diff between microstation air temps ######

# merge the data + filter data for only the daytime during the growing season
KBS_season <- KBS
KBS_season$month <- format(KBS_season$Date_Time,format="%m")
KBS_season$year <- format(KBS_season$Date_Time,format="%Y")
KBS_season$hour <- format(KBS_season$Date_Time, format="%H")

KBS_season <- KBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
KBS_avg_year <- KBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time)

# test for outliers - none extreme
outliers <- KBS_avg_year %>%
  group_by(treatment, year) %>%
  identify_outliers(temp)
view(outliers)

## check for normality - can't get this to run
#KBS_avg_year %>%
#  group_by(treatment, year) %>%
#  shapiro_test(temp)
## visual check for normality - seems normal?
#ggqqplot(KBS_avg_year, "temp", ggtheme = theme_bw()) +
#  facet_grid(year ~ treatment, labeller = "label_both")

# run anova - significant interaction btw year and treatment
anova.res.kbs <- aov(temp ~ treatment * year, data = KBS_avg_year)
summary(anova.res.kbs)

# post-hoc test
pairwise.comp <- KBS_avg_year %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp

# avg temps in the chambers during the daytime
KBS_avg_temp <- KBS_avg_year %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# avg temps in the chambers during the daytime for each year
KBS_avg_temp_year <- KBS_avg_year %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# avg temps in the chambers on hot days
KBS_avg_hot_day <- KBS_season %>%
        filter(XH_ambient_air_1m > 27) %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# avg temps from march-april and july-august (early season vs late season)
KBS_early <- KBS_season %>%
        filter(month == "03" | month == "04") %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))
KBS_late <- KBS_season %>%
        filter(month == "07" | month == "08") %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# winter warming?
KBS_winter <- KBS
KBS_winter$month <- format(KBS_winter$Date_Time,format="%m")
KBS_winter$year <- format(KBS_winter$Date_Time,format="%Y")
KBS_winter$hour <- format(KBS_winter$Date_Time, format="%H")
KBS_winter <- KBS_winter %>%
        filter(month > "10" | month < "03") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)
KBS_avg_winter <- KBS_winter %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

#nighttime warming?
KBS_night <- KBS
KBS_night$month <- format(KBS_night$Date_Time,format="%m")
KBS_night$year <- format(KBS_night$Date_Time,format="%Y")
KBS_night$hour <- format(KBS_night$Date_Time, format="%H")
KBS_night <- KBS_night %>%
        filter(month > "03" | month < "09") %>%
        filter(hour > "20" | hour < "06") %>%
        select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)
KBS_avg_night <- KBS_night %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))
        



###### testing for sig diff between microstation air temps for July ######

KBS_season_july <- KBS_season %>%
  filter(month == "07") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
KBS_season_july <- KBS_season_july %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time)

# test for outliers - none extreme
outliers <- KBS_season_july %>%
  group_by(treatment, year) %>%
  identify_outliers(temp)
view(outliers)

# check for normality - can't get this to run
KBS_season_july %>%
  group_by(treatment, year) %>%
  shapiro_test(temp)
# visual check for normality - seems normal?
ggqqplot(KBS_season_july, "temp", ggtheme = theme_bw()) +
  facet_grid(year ~ treatment, labeller = "label_both")

# run anova - significant interaction btw year and treatment
anova.res.july <- aov(temp ~ treatment * year, data = KBS_season_july)
summary(anova.res.july)

# post-hoc test
pairwise.comp <- KBS_season_july %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp


###### testing for sig diff between microstation soil temps ######

# merge the data + filter data for only the daytime during the growing season
KBS_season_soil <- KBS
KBS_season_soil$month <- format(KBS_season_soil$Date_Time,format="%m")
KBS_season_soil$year <- format(KBS_season_soil$Date_Time,format="%y")
KBS_season_soil$hour <- format(KBS_season_soil$Date_Time, format="%H")

KBS_season_soil <- KBS_season_soil %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm)

# create new dataframes for temperatures averaged by year & averaged by month and year
KBS_avg_soil <- KBS_season_soil %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time)

# test for outliers - some extreme, but they seem like reasonable values
outliers <- KBS_avg_soil %>%
  group_by(treatment, year) %>%
  identify_outliers(temp)
view(outliers)

# check for normality - can't get this to run
KBS_avg_soil %>%
  group_by(treatment, year) %>%
  shapiro_test(temp)
# visual check for normality - seems normal?
ggqqplot(KBS_avg_soil, "temp", ggtheme = theme_bw()) +
  facet_grid(year ~ treatment, labeller = "label_both")

# run anova - significant interaction btw year and treatment
anova.res.soil <- aov(temp ~ treatment * year, data = KBS_avg_soil)
summary(anova.res.soil)

# post-hoc test
pairwise.comp <- KBS_avg_soil %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp


###### testing for sig diff between microstation soil moisture ######

# merge the data + filter data for only the daytime during the growing season
KBS_season_moist <- KBS
KBS_season_moist$month <- format(KBS_season_moist$Date_Time,format="%m")
KBS_season_moist$year <- format(KBS_season_moist$Date_Time,format="%y")
KBS_season_moist$hour <- format(KBS_season_moist$Date_Time, format="%H")

KBS_season_moist <- KBS_season_moist %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_soil_moisture_5cm, XH_ambient_soil_moisture_5cm)

# create new dataframes for temperatures averaged by year & averaged by month and year
KBS_avg_moist <- KBS_season_moist %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time)

# test for outliers - some extreme, but they seem like reasonable values
outliers <- KBS_avg_moist %>%
  group_by(treatment, year) %>%
  identify_outliers(temp)
view(outliers)

# check for normality - can't get this to run
KBS_avg_moist %>%
  group_by(treatment, year) %>%
  shapiro_test(temp)
# visual check for normality - seems normal?
ggqqplot(KBS_avg_moist, "temp", ggtheme = theme_bw()) +
  facet_grid(year ~ treatment, labeller = "label_both")

# run anova - significant interaction btw year and treatment
anova.res.moist <- aov(temp ~ treatment * year, data = KBS_avg_moist)
summary(anova.res.moist)

# post-hoc test
pairwise.comp <- KBS_avg_moist %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp


###### testing for stnd dev over time in chambers - pendant data ######

KBS_pend_avg <- KBS_pend %>%
  group_by(Date_Time) %>%
  summarize(average_temp = mean(Temp_F_XP_air_1m, na.rm = TRUE),
            stnd_dev = sd(Temp_F_XP_air_1m, na.rm = TRUE))

KBS_pend_avg$year <- format(KBS_pend_avg$Date_Time,format="%Y")

KBS_pend_med <- KBS_pend_avg %>%
  group_by(year) %>%
  summarize(median = median(stnd_dev, na.rm = TRUE))

ggplot(KBS_avg_month, aes(x = month, y = average_temp, fill = treatment)) + 
  facet_grid(.~year) +
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(x = NULL, y = NULL, fill = "Treatment")




#########################################
# UMBS #
#########################################



###### testing for sig diff between microstation air temps ######

# merge the data + filter data for only the daytime during the growing season
UMBS_season <- UMBS
UMBS_season$month <- format(UMBS_season$Date_Time,format="%m")
UMBS_season$year <- format(UMBS_season$Date_Time,format="%Y")
UMBS_season$hour <- format(UMBS_season$Date_Time, format="%H")

UMBS_season <- UMBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
UMBS_avg_year <- UMBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time)

# test for outliers - none extreme
outliers <- UMBS_avg_year %>%
  group_by(treatment, year) %>%
  identify_outliers(temp)
view(outliers)

# check for normality - can't get this to run
UMBS_avg_year %>%
  group_by(treatment, year) %>%
  shapiro_test(temp)
# visual check for normality - seems normal?
ggqqplot(UMBS_avg_year, "temp", ggtheme = theme_bw()) +
  facet_grid(year ~ treatment, labeller = "label_both")

# run anova - no significant interaction btw year and treatment
anova.res.umbs <- aov(temp ~ treatment * year, data = UMBS_avg_year)
summary(anova.res.umbs)

# main effect of treatment on temp
pairwise.comp <- UMBS_avg_year %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp


# avg temps in the chambers during the daytime
UMBS_avg_temp <- UMBS_avg_year %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# avg temps in the chambers during the daytime for each year
UMBS_avg_temp_year <- UMBS_avg_year %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# avg temps in the chambers on hot days
UMBS_avg_hot_day <- UMBS_season %>%
        filter(XH_ambient_air_1m > 27) %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# avg temps from march-april and july-august (early season vs late season)
UMBS_early <- UMBS_season %>%
        filter(month == "03" | month == "04") %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))
UMBS_late <- UMBS_season %>%
        filter(month == "07" | month == "08") %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

# winter warming?
UMBS_winter <- UMBS
UMBS_winter$month <- format(UMBS_winter$Date_Time,format="%m")
UMBS_winter$year <- format(UMBS_winter$Date_Time,format="%Y")
UMBS_winter$hour <- format(UMBS_winter$Date_Time, format="%H")
UMBS_winter <- UMBS_winter %>%
        filter(month > "10" | month < "03") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)
UMBS_avg_winter <- UMBS_winter %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))

#nighttime warming?
UMBS_night <- UMBS
UMBS_night$month <- format(UMBS_night$Date_Time,format="%m")
UMBS_night$year <- format(UMBS_night$Date_Time,format="%Y")
UMBS_night$hour <- format(UMBS_night$Date_Time, format="%H")
UMBS_night <- UMBS_night %>%
        filter(month > "03" | month < "09") %>%
        filter(hour > "20" | hour < "06") %>%
        select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)
UMBS_avg_night <- UMBS_night %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
        group_by(year, treatment) %>%
        summarize(mean_temp = mean(temp, na.rm = T),
                  sd_temp = sd(temp, na.rm = T))


