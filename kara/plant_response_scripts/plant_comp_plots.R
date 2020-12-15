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
library(plotrix)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data & metadata
plant_comp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
meta <- read.csv("L2/plot.csv")
str(plant_comp) # for some reason, date column converted back to character

# Fix date column & add column for the year and julian day
plant_comp$Date <- as.Date(plant_comp$Date,format="%Y-%m-%d")
str(plant_comp)
plant_comp$Year <- format(plant_comp$Date,format="%Y")
plant_comp$Julian <- format(plant_comp$Date, "%j")

# Make julian column numeric
plant_comp$Julian <- as.numeric(plant_comp$Julian)
str(plant_comp)

# Change column names to lowercase so they can be merged with metadata
names(plant_comp) <- tolower(names(plant_comp))

# Merge metadata with data
plant_comp_merge <- left_join(meta, plant_comp, by = "plot")
str(plant_comp_merge)

# Filter data to contain the date of first occurance for each species
filter_comp <- plant_comp_merge %>%
  group_by(plot, year, species, state, site) %>%
  summarize(julian = min(julian))

sum_comp <- filter_comp %>%
  group_by(state, species, year, site) %>%
  summarize(avg_julian = mean(julian, na.rm = TRUE),
            se = std.error(julian, na.rm = TRUE))

# Function to make a plot for any species
greenup_plot <- function(spp, loc) { 
  greenup_spp <- subset(sum_comp, species == spp & site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", alpha = 0.5, stat = "identity") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Julian Day of Greenup") +
           scale_fill_manual(values = c("blue4", "red")) +
           theme_classic())
}
greenup_plot("Popr", "umbs")

