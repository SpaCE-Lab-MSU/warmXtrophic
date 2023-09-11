# TITLE: PAR analyses
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the L1 folder in the shared Google drive
# DATA OUTPUT: Results from PAR analyses above and below the canopy
# PROJECT: warmXtrophic
# DATE: Nov 2021


######## not used in manuscript #######


# clear all existing data
rm(list=ls())

# load in packages and source in functions
library(tidyverse)

# set working directory
L1_dir<-Sys.getenv("L1DIR")

# read in the data
KBS <- read.csv(file.path(L1_dir,"PAR/KBS_PAR_L1.csv"))
UMBS <- read.csv(file.path(L1_dir,"PAR/UMBS_PAR_L1.csv"))

# convert date column back to date
KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d")

## note:
# not doing growing season-specific PAR averages because all of the data collected on PAR was already
# during the growing season (including some in Sept.)

########## KBS ###########
# add column for month and year
KBS$month <- format(KBS$Date_Time,format="%m")
KBS$year <- format(KBS$Date_Time,format="%Y")

# t-tests
over_t.test <- t.test(Overstory~state,data=KBS)
over_t.test
under_t.test <- t.test(Average_Ground~state,data=KBS)
under_t.test

# avg PAR overall
KBS_avg_over <- KBS %>%
        group_by(state) %>%
        summarize(mean = mean(Overstory, na.rm = T),
                  sd = sd(Overstory, na.rm = T))
KBS_avg_under <- KBS %>%
        group_by(state) %>%
        summarize(mean = mean(Average_Ground, na.rm = T),
                  sd = sd(Average_Ground, na.rm = T))
KBS_avg_sun <- KBS %>%
        group_by(state) %>%
        summarize(mean = mean(Percent_Sunlight, na.rm = T),
                  sd = sd(Percent_Sunlight, na.rm = T))


# avg PAR early vs. late season
KBS_avg_over_early <- KBS %>%
        filter(month == "05" | month == "06") %>%
        group_by(state) %>%
        summarize(mean = mean(Overstory, na.rm = T),
                  sd = sd(Overstory, na.rm = T))
KBS_avg_over_late <- KBS %>%
        filter(month == "07" | month == "08" | month == "09") %>%
        group_by(state) %>%
        summarize(mean = mean(Overstory, na.rm = T),
                  sd = sd(Overstory, na.rm = T))

KBS_avg_under_early <- KBS %>%
        filter(month == "05" | month == "06") %>%
        group_by(state) %>%
        summarize(mean = mean(Average_Ground, na.rm = T),
                  sd = sd(Average_Ground, na.rm = T))
KBS_avg_under_late <- KBS %>%
        filter(month == "07" | month == "08" | month == "09") %>%
        group_by(state) %>%
        summarize(mean = mean(Average_Ground, na.rm = T),
                  sd = sd(Average_Ground, na.rm = T))

KBS_avg_sun_early <- KBS %>%
        filter(month == "05" | month == "06") %>%
        group_by(state) %>%
        summarize(mean = mean(Percent_Sunlight, na.rm = T),
                  sd = sd(Percent_Sunlight, na.rm = T))
KBS_avg_sun_late <- KBS %>%
        filter(month == "07" | month == "08" | month == "09") %>%
        group_by(state) %>%
        summarize(mean = mean(Percent_Sunlight, na.rm = T),
                  sd = sd(Percent_Sunlight, na.rm = T))


########## UMBS ###########
# add column for month and year
UMBS$month <- format(UMBS$Date_Time,format="%m")
UMBS$year <- format(UMBS$Date_Time,format="%Y")

# t-tests
over_umbs_t.test <- t.test(Overstory~state,data=UMBS)
over_umbs_t.test
under_umbs_t.test <- t.test(Average_Ground~state,data=UMBS)
under_umbs_t.test

# avg PAR overall
UMBS_avg_over <- UMBS %>%
        group_by(state) %>%
        summarize(mean = mean(Overstory, na.rm = T),
                  sd = sd(Overstory, na.rm = T))
UMBS_avg_under <- UMBS %>%
        group_by(state) %>%
        summarize(mean = mean(Average_Ground, na.rm = T),
                  sd = sd(Average_Ground, na.rm = T))
UMBS_avg_sun <- UMBS %>%
        group_by(state) %>%
        summarize(mean = mean(Percent_Sunlight, na.rm = T),
                  sd = sd(Percent_Sunlight, na.rm = T))

# avg PAR early vs. late season
UMBS_avg_over_early <- UMBS %>%
        filter(month == "05" | month == "06") %>%
        group_by(state) %>%
        summarize(mean = mean(Overstory, na.rm = T),
                  sd = sd(Overstory, na.rm = T))
UMBS_avg_over_late <- UMBS %>%
        filter(month == "07" | month == "08" | month == "09") %>%
        group_by(state) %>%
        summarize(mean = mean(Overstory, na.rm = T),
                  sd = sd(Overstory, na.rm = T))

UMBS_avg_under_early <- UMBS %>%
        filter(month == "05" | month == "06") %>%
        group_by(state) %>%
        summarize(mean = mean(Average_Ground, na.rm = T),
                  sd = sd(Average_Ground, na.rm = T))
UMBS_avg_under_late <- UMBS %>%
        filter(month == "07" | month == "08" | month == "09") %>%
        group_by(state) %>%
        summarize(mean = mean(Average_Ground, na.rm = T),
                  sd = sd(Average_Ground, na.rm = T))

UMBS_avg_sun_early <- UMBS %>%
        filter(month == "05" | month == "06") %>%
        group_by(state) %>%
        summarize(mean = mean(Percent_Sunlight, na.rm = T),
                  sd = sd(Percent_Sunlight, na.rm = T))
UMBS_avg_sun_late <- UMBS %>%
        filter(month == "07" | month == "08" | month == "09") %>%
        group_by(state) %>%
        summarize(mean = mean(Percent_Sunlight, na.rm = T),
                  sd = sd(Percent_Sunlight, na.rm = T))
