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
library(plotrix)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

############################### Read in data 
# cleaned phenology data from L1
phen_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/phenology/final_flw_sd_L1.csv", stringsAsFactors=FALSE)
phen_data <- phen_data %>% 
        select(-X) # get rid of "X" column that shows up

# Create separate data frames for flowering and seeding
phen_flwr <- subset(phen_data, action == "flower")
phen_sd <- subset(phen_data, action == "seed")

# This creates a data frame that returns the first date of flower per species per plot
# Filter data to contain the date of first occurrence for each species

FirstFlower <- phen_flwr %>%
        group_by(plot, year, species, state, site, action) %>%
        summarize(julian = min(julian, na.rm=T))

FirstFlwr <- phen_flwr %>% 
        group_by(year, state, site, action) %>% 
        summerize(julian = min(julian, na.rm=T))

sum_FirstFlower <- FirstFlower %>%
        group_by(site, state, species, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE))

FirstFlower_plot <- function(spp, loc) { 
        FirstFlower_spp <- subset(sum_FirstFlower, species == spp & site == loc)
        return(ggplot(FirstFlower_spp, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Day of First Flower", title = spp, subtitle = loc) +
                       scale_fill_manual(values = c("blue", "darkred")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

FirstFlower_plot("Popr", "umbs")
FirstFlower_plot("Popr", "kbs")
FirstFlower_plot("Eugr", "kbs")
FirstFlower_plot("Soca", "kbs")

# Calculate the average date of first flower for all ambient and warmed plots per year per site. Plot.

# create a function that returns plots (1 for each year at each site) for a given site and year comparing
# warmed vs ambient plots for average first date of flower (add by functional groups next?)

# calculate the average date of first flower for each plot
