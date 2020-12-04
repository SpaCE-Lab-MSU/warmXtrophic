# TITLE:          Plant composition cleanup
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    Clean csv files are uploaded to the L1 Google drive folder
# PROJECT:        warmXtrophic
# DATE:           November, 2020
# NOTES:          Only the years 2018 & 2020 for KBS and 2019 & 2020 are included
#                 in this script (for now) because previous years were cleaned already

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
kbs_2018 <- read.csv("L0/KBS/2018/kbs_plant_comp_2018.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_plant_comp_2020.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_plantcomp_2019.csv")[,1:7]
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_plantcomp_2020.csv")[,1:7]

# Remove uneeded columns 
comp_list <- list(kbs_2018=kbs_2018, kbs_2020=kbs_2020, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
comp_list <- lapply(comp_list, remove_col, name=c('Julian', 'Notes'))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names

# Fixing species names
# kbs_2020 Rubsp -> Rusp, Umsp?
# umbs_2019 remove empty, Ruag?, Sosp -> Spsp?, Syal -> Syla, Umsp?

