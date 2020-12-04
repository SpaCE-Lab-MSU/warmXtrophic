# TITLE:          Plant composition: merging data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           November, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
kbs_2015 <- read.csv("L1/plant_composition/kbs_plant_comp_2015.csv")
kbs_2016 <- read.csv("L1/plant_composition/kbs_plant_comp_2016.csv")
kbs_2017 <- read.csv("L1/plant_composition/kbs_plant_comp_2017.csv")
kbs_2019 <- read.csv("L1/plant_composition/kbs_plant_comp_2019.csv")