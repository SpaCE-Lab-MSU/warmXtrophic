# TITLE: PAR data cleanup & merge
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the L0 folder in the shared Google drive
# DATA OUTPUT: Clean data files are in the L1 PAR folder
# PROJECT: warmXtrophic
# DATE: July 2020

# clear all existing data
rm(list=ls())

# load in packages and source in functions
library(tidyverse)

source("~/warmXtrophic/R/L1/PAR_functions_L1.R")

# set working directory
L0_dir<-Sys.getenv("L0DIR")
L1_dir<-Sys.getenv("L1DIR")

#######################################################################
#    KBS
#######################################################################

# read in the data
KBS_17 <- read.csv(file.path(L0_dir,"KBS/2017/KBS_par_2017.csv"))[,1:7]
KBS_18 <- read.csv(file.path(L0_dir,"KBS/2018/kbs_par_2018.csv"))[,1:9]
KBS_19 <- read.csv(file.path(L0_dir,"KBS/2019/KBS_par_2019.csv"))[1:120,1:9]
KBS_20 <- read.csv(file.path(L0_dir,"KBS/2020/kbs_par_2020.csv"))

# add in needed columns to KBS_17 and KBS_18 — compute the average understory PAR
KBS_17$Average_Ground <- (KBS_17$Understory.PAR + KBS_17$Understory.PAR.1)/2
KBS_17$Percent_Sunlight <- KBS_17$Average_Ground/KBS_17$Overstory.PAR
KBS_18$Average_Ground <- (KBS_18$Ground_1 + KBS_18$Gound_2)/2
KBS_18$Percent_Sunlight <- KBS_18$Average_Ground/KBS_18$Above_Biomass

# add data to a list to apply functions
list_kbs <- list(KBS_17 = KBS_17, KBS_18 = KBS_18, KBS_19 = KBS_19, KBS_20 = KBS_20)
list_kbs <- lapply(list_kbs, column_names)
list_kbs[1:3] <- lapply(list_kbs[1:3], remove_column, name=c('Julian.Date', 'Julian'))
list_kbs <- lapply(list_kbs, change_POSIX)

#######################################################################
#    UMBS
#######################################################################

# read in the data
UMBS_17 <- read.csv(file.path(L0_dir,"UMBS/2017/umbs_PAR_2017.csv"))
UMBS_18 <- read.csv(file.path(L0_dir,"UMBS/2018/UMBS_PAR_2018.csv"))[,1:7]
UMBS_19 <- read.csv(file.path(L0_dir,"UMBS/2019/UMBS_par_2019.csv"))[,1:9]
UMBS_20 <- read.csv(file.path(L0_dir,"UMBS/2020/UMBS_par_2020.csv"))

# add in needed columns to KBS_17 and KBS_18 — there is no second understory measurement
# still need to make sure the column names match the other dataframes so they can be merged
UMBS_17$Average_Ground <- UMBS_17$Understory.PAR
UMBS_17$Percent_Sunlight <- UMBS_17$Average_Ground/UMBS_17$Overstory.PAR
UMBS_17$Understory_2 <- NA
UMBS_18$Average_Ground <- UMBS_18$Understory.PAR
UMBS_18$Percent_Sunlight <- UMBS_18$Average_Ground/UMBS_18$Overstory
UMBS_18$Understory_2 <- NA

# fix site column for 2018
UMBS_18$Site <- "umbs"

# add data to a list to apply functions
list_umbs <- list(UMBS_17 = UMBS_17, UMBS_18 = UMBS_18, UMBS_19 = UMBS_19, UMBS_20 = UMBS_20)
list_umbs <- lapply(list_umbs, remove_column, name=c('Julian.Date', 'Julian', 'PAR'))
list_umbs <- lapply(list_umbs, column_names)
list_umbs <- lapply(list_umbs, change_POSIX)

# merge the data
kbs <- rbind(list_kbs$KBS_17,list_kbs$KBS_18,list_kbs$KBS_19,list_kbs$KBS_20)
umbs <- rbind(list_umbs$UMBS_17,list_umbs$UMBS_18,list_umbs$UMBS_19,list_umbs$UMBS_20)

# create csv files
write.csv(kbs, file.path(L1_dir, "PAR_data/KBS_PAR_L1.csv"))
write.csv(umbs, file.path(L1_dir, "PAR_data/UMBS_PAR_L1.csv"))


