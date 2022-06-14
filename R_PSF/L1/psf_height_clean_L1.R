# TITLE: PSF experiment 2021 - Plant Height
# AUTHORS: Moriah Young
# COLLABORATORS: Kara Dobson
# DATA INPUT: raw data - psf_height.csv
# DATA OUTPUT: cleaned data - psf_height_L1.csv
# PROJECT: Plant soil feedback experiment 2021
# DATE: June, 2022

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(janitor)

# Set working directory - not working for Moriah
L0_dir <- Sys.getenv("L0DIR_PSF")
L1_dir <- Sys.getenv("L1DIR_PSF")
list.files(L1_dir)

# .Renviron not working for MLY; hard-coding in here
L0_dir <- "/Volumes/GoogleDrive/Shared\ drives/SpaCE_Lab_warmXtrophic/data/PSF_greenhouse_2021/data/L0"
L1_dir <- "/Volumes/GoogleDrive/Shared\ drives/SpaCE_Lab_warmXtrophic/data/PSF_greenhouse_2021/data/L1"
list.files(L0_dir)

### Read in csv files ###
meta <- read.csv(file.path(L0_dir, "psf_meta_data.csv"))
height <- read.csv(file.path(L0_dir, "psf_height_2021.csv"))

names(meta)[1] <- "plant_ID" #changing column name
height_1 <- merge(meta, height, all = TRUE) # merge meta data with biomass data
height_2 <- na.omit(height_1)

# write a new csv with the cleaned and merge data and upload to the shared google drive L1 folder
write.csv(height_2, file.path(L1_dir, "./psf_height_2021_L1.csv"), row.names=F)

