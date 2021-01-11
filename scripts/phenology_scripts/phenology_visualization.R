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

# Change column names to lowercase
names(phen_data) <- tolower(names(phen_data))
str(phen_data)

# plot level information from L2
plot_meta <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L2/plot.csv")

# taxon data from L2
taxa <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L2/taxon.csv")
# Change column name for from "code" to "Species" to match cleaned phenology data
colnames(taxa) <- sub("code", "species", colnames(taxa))

############################# Merge data and clean 
# merge cleaned phen_data with the plot level information
phen_data2 <- merge(phen_data, plot_meta, by = "plot")
phen_data3 <- merge(phen_data2, taxa, by = "species")

phen_data <- phen_data3 %>% 
        select(-old_name, -old_species, -site.y, -resolution)
colnames(phen_data) <- sub("site.x", "site", colnames(phen_data))

# Fix date column & add column for the year and julian day
phen_data$Date <- as.Date(phen_data$Date,format="%Y-%m-%d")
str(phen_data4)
phen_data$Year <- format(phen_data$Date,format="%Y")
phen_data$Julian <- format(phen_data$Date, "%j")

# Make julian column numeric
phen_data$Julian <- as.numeric(phen_data$Julian)
str(phen_data)


# This creates a data frame that returns the the first date of flower per species per plot
firstflower <- phen_data %>%
        group_by(plot, year, species, state, site, action) %>%
        summarize(julian = min(julian, na.rm=T))

