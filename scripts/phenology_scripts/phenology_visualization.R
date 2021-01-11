# TITLE:          Phenology Data Visualization
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L1 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           January, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

############################### Read in data 
# cleaned phenology data from L1
phen_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/reproductive_phenology/final_flw_sd_L1.csv")
phen_data <- phen_data %>% 
        select(-X)

# plot level information from L2
plot <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L2/plot.csv")
# Change column name for from "plot" to "Plot"
colnames(plot) <- sub("plot", "Plot", colnames(plot))

# taxon data from L2
taxa <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L2/taxon.csv")

#############################
# merge cleaned phen_data with the plot level information
phen_data2 <- merge(phen_data, plot, by = "Plot")

