# TITLE:          Herbivory cleanup
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
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
source("~/warmXtrophic/scripts/herbivory_scripts/herbivory_functions.R")

# Read in data
kbs_2015 <- read.csv("L0/KBS/2015/kbs_leaf_herbivory_2015.csv")
kbs_2016 <- read.csv("L0/KBS/2016/kbs_leaf_herbivory_2016.csv")
kbs_2017 <- read.csv("L0/KBS/2017/kbs_leaf_herbivory_2017.csv")
kbs_2018 <- read.csv("L0/KBS/2018/kbs_leaf_herbivory_2018.csv")
kbs_2019 <- read.csv("L0/KBS/2019/kbs_leaf_herbivory_2019.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_herbivory_2020.csv")
umbs_2015 <- read.csv("L0/UMBS/2015/umbs_herbivory_2015.csv")
umbs_2016 <- read.csv("L0/UMBS/2016/umbs_leaf_herbivory_2016.csv")
umbs_2017 <- read.csv("L0/UMBS/2017/umbs_leaf_herbivory_2017.csv")
umbs_2018 <- read.csv("L0/UMBS/2018/umbs_leaf_herbivory_2018.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_herbivory_2019.csv")
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_herbivory_2020.csv")

# Put dataframes into a list so that functions can be applied
herb_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)

# Fix ID column elements in 2015 data
herb_list[1] <- lapply(herb_list[1], transform, 
                                   ID = format(as.Date(sub(".*_(\\d{4}_\\d{2}_\\d{2})_.*",
                                                           "\\1", ID), "%Y_%m_%d"), "%m/%d/%Y"))
herb_list[7] <- lapply(herb_list[7], transform, 
                       ID = format(as.Date(sub(".*_(\\d{4}_\\d{2}_\\d{2})_.*",
                                               "\\1", ID), "%Y_%m_%d"), "%m/%d/%Y"))

# Apply other cleaning functions
#herb_list <- lapply(herb_list, remove_col, name=c("Julian", "Notes"))
herb_list <- lapply(herb_list, change_date)

### note to self: remove column function doesn't work
# need to change ID column to date for 2015 files
# everything lower case 
# check for misspellings

