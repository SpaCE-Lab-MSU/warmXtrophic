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
library(googlesheets)
library(gsheet)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
biomass <- read.csv("L0/KBS/2020/kbs_ancillary_ANPP_2020.csv")
taxa <- read.csv("/Users/moriahyoung/Desktop/taxon_list.csv")

biomass1 <- biomass %>% 
        select(-dry_weight_g, -bag, -bag.size, -weight, -X..of.bags, -bag.weight, -bag.code)
View(biomass1)

names(biomass1)[names(biomass1)=="final_biomass_g"] <- "weight_g"

# Sort data by plot then weight
biomass.final <- with(biomass1, biomass1[order(plot, weight_g),])
View(biomass.final)

table(unique(biomass.final$plot)) 
unique(biomass.final$species)

# Check for any misspelling of species code names and compare species codes 
# with the taxon lookup table (spp)
setdiff(unique(biomass.final$species), unique(taxa$code))
