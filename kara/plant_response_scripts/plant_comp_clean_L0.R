# TITLE:          Plant composition cleanup
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing plant comp data for all sites & years is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Source in needed functions
source("~/warmXtrophic/kara/plant_response_scripts/plant_comp_functions.R")

# Read in data (only need columns 1-7 for the umbs files)
kbs_2015 <- read.csv("L0/KBS/2015/kbs_plant_comp_2015.csv")
kbs_2016 <- read.csv("L0/KBS/2016/kbs_plant_comp_2016.csv")
kbs_2017 <- read.csv("L0/KBS/2017/kbs_plant_comp_2017.csv")
kbs_2018 <- read.csv("L0/KBS/2018/kbs_plant_comp_2018.csv")
kbs_2019 <- read.csv("L0/KBS/2019/kbs_plant_comp_2019.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_plant_comp_2020.csv")
umbs_2015 <- read.csv("L0/UMBS/2015/umbs_plant_comp_2015.csv")
umbs_2016 <- read.csv("L0/UMBS/2016/umbs_plant_comp_2016.csv")
umbs_2017 <- read.csv("L0/UMBS/2017/umbs_plant_comp_2017.csv")
umbs_2018 <- read.csv("L0/UMBS/2018/umbs_plant_comp_2018.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_plantcomp_2019.csv")[,1:7]
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_plantcomp_2020.csv")[,1:7]

# remove all empty rows for umbs_2019
umbs_2019 <- umbs_2019[-c(5047:6024), ]

# add site column for kbs & umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column name for umbs 2016
colnames(umbs_2016) <- sub("Percent_Cover", "Cover", colnames(umbs_2016))

# Remove uneeded columns 
comp_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
comp_list <- lapply(comp_list, remove_col, name=c("Julian", "Notes", "Quadrat", "Julian_Day"))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names

# Fixing species names
# Umsp (Ulsp?), Ruag (Rual, Ruac?), Smooth_oat (Arre?), Cofo (?)
comp_list <- lapply(comp_list, change_spp)
lapply(comp_list, spp_name) # looks good

# Save cleaned list of dataframes to the google drive
save(comp_list, file="L1/plant_composition/clean_plantcomp_L1.RData")

# Merge final data
comp_merge <- rbind(comp_list$kbs_2015, comp_list$kbs_2016, comp_list$kbs_2017, comp_list$kbs_2018, comp_list$kbs_2019, comp_list$kbs_2020, 
                    comp_list$umbs_2015, comp_list$umbs_2016, comp_list$umbs_2017, comp_list$umbs_2018, comp_list$umbs_2019, comp_list$umbs_2020)
write.csv(comp_merge, file="L1/plant_composition/final_plantcomp_L1.csv")
