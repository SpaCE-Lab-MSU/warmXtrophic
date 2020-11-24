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
library(googlesheets4)
library(gsheet)
library(googledrive)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
biomass <- read_sheet("https://docs.google.com/spreadsheets/d/1SNjYiEhtNelFB58c-cfGUNGWO7kreP05_lKXdD3-RLM/edit#gid=0")

#biomass <- read.csv("L0/KBS/2020/kbs_ancillary_ANPP_2020.csv")
#taxa <- read.csv("/Users/moriahyoung/Desktop/taxon_list.csv")

# Clean biomass data 
biomass1 <- as.data.frame(biomass)
biomass2 <- biomass %>% # get ride of unneeded columns
        select(-dry_weight_g, -bag, -bag_size, -weight, -n_bags, -bag_weight, -bag_code, -notes)

names(biomass2)[names(biomass2)=="final_biomass_g"] <- "weight_g"

# Sort data by plot then weight
biomass.final <- with(biomass2, biomass2[order(plot, weight_g),])
View(biomass.final)

table(unique(biomass.final$plot)) # Are there 19 plots?
unique(biomass.final$species) # Are all the species unique?

# Check for any misspelling of species code names and compare species codes 
# with the taxon lookup table (spp)
setdiff(unique(biomass.final$species), unique(taxa$code))

