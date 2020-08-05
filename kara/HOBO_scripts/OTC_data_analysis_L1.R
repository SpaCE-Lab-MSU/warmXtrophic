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
for (package in c("tidyverse", "plotrix", "ggpubr",'rstatix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# load in the data
KBS_1 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair1_L1.csv")
KBS_2 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair2_L1.csv")
KBS_3 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair3_L1.csv")

UMBS_1 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair1_L1.csv")
UMBS_2 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair2_L1.csv")
UMBS_3 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair3_L1.csv")



#########################################
# KBS #
#########################################



###### testing for sig diff between microstation air temps ######

# merge the data + filter data for only the daytime during the growing season
KBS <- rbind(KBS_1, KBS_2, KBS_3)
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

# check for normality - can't get this to run
KBS_avg_year %>%
  group_by(treatment, year) %>%
  shapiro_test(temp)
# visual check for normality - seems normal?
ggqqplot(KBS_avg_year, "temp", ggtheme = theme_bw()) +
  facet_grid(year ~ treatment, labeller = "label_both")

# run anova - significant interaction btw year and treatment
anova.res <- aov(temp ~ treatment * year, data = KBS_avg_year)
summary(anova.res)

# post-hoc test
pairwise.comp <- KBS_avg_year %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp

# simple linear regression
KBS_comb_lm <- KBS_comb_2019 %>%
  filter(treatment == "XH_warmed_air_1m")
lm_kbs <- lm(average_temp ~ average_par, data = KBS_comb_lm)
summary(lm_kbs)
lm_kbs



#########################################
# UMBS #
#########################################



###### testing for sig diff between microstation air temps ######

# merge the data + filter data for only the daytime during the growing season
UMBS <- rbind(UMBS_1, UMBS_2, UMBS_3)
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
anova.res <- aov(temp ~ treatment * year, data = UMBS_avg_year)
summary(anova.res)

# main effect of treatment on temp
pairwise.comp <- UMBS_avg_year %>%
  group_by(year) %>%
  pairwise_t_test(
    temp ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pairwise.comp


