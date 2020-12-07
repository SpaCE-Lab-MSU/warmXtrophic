# TITLE:          Plant composition data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv file from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data & metadata
plant_comp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
meta <- read.csv("L2/plot.csv")
str(plant_comp) # for some reason, data column converted back to character

# Fix date column & add column for the year and day
plant_comp$Date <- as.Date(plant_comp$Date,format="%Y-%m-%d")
str(plant_comp)
plant_comp$Year <- format(plant_comp$Date,format="%Y")
plant_comp$month_day <- format(plant_comp$Date,format="%m/%d")

# Change column names to lowercase so they can be merged with metadata
names(plant_comp) <- tolower(names(plant_comp))

# Merge metadata with data
plant_comp_merge <- left_join(meta, plant_comp, by = "plot")

# Filter data to contain the date of first occurance for each species
filter_comp <- plant_comp_merge %>%
  group_by(state, species, year) %>%
  filter(date == min(date)) %>%
  select(cover, state, plot, species, month_day, year)

###### fix this plot ######
# Function to make a plot for any species
greenup_plot <- function(spp) { 
  greenup_spp <- subset(filter_comp, species == spp)
  return(ggplot(greenup_spp, aes(x = year, y = month_day)) +
           geom_line(aes(color = state)) +
           geom_point(aes(color = state)) +
           labs(x = "Year", y = "Date of Greenup") +
           theme_classic())
}
greenup_plot("Cest")
