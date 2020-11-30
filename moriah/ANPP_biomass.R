# TITLE:          KBS ANPP Biomass Data Analysis
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           November, 2020

# Clear existing data
rm(list = ls())

# Load packages
library(tidyverse)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
biomass <- read.csv("L0/KBS/2020/kbs_ancillary_biomass_2020.csv")
biomass1 <- as.data.frame(biomass) # make into data frame
# Make sure that you create a new .csv file of the taxon_list.xlsx file IN the google shared folder
# to ensure that you have the most up to date species list
taxa <- read.csv("L2/taxon_uptodate.csv")

# Clean biomass data 
biomass2 <- biomass %>% # get ride of unneeded columns
        select(-dry_weight_g, -bag, -bag_size, -weight, -n_bags, -bag_weight, -bag_code, -notes)

names(biomass2)[names(biomass2)=="final_biomass_g"] <- "weight_g"

# Sort data by plot then weight
biomass.final <- with(biomass2, biomass2[order(plot, weight_g),])
View(biomass.final)

# Change date from factor to date
biomass.final$date <- as.Date(biomass.final$date,format="%m/%d/%Y")

table(unique(biomass.final$plot)) # Are there 19 plots?
unique(biomass.final$species) # Are all the species unique?

# Check for any misspelling of species code names and compare species codes 
# with the taxon lookup table (spp)
setdiff(unique(biomass.final$species), unique(taxa$code))

# Save a .csv file with the cleaned biomass
write.csv(biomass.final, file = "L0/KBS/2020/kbs_biomass_final.csv", row.names = FALSE)
#this will save a cleaned version of the KBS ANPP Biomass 2020 file in the shared folder of the warmXtrophic project
