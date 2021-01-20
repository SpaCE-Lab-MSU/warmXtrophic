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
library(cowplot)

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

# This creates a data frame that returns the mean julian date of first flower         
sum_FirstFlower <- FirstFlower %>%
        group_by(site, state, species, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE))

sum_FirstFlwr <- FirstFlower %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE)) 
# this gives the average julian day of first flower for each site and year for warmed and ambient

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

# This creates a function that returns plots for a given site and year for average first date of flower comparing ambient vs warmed plots
sum_FirstFlwr_plot <- function(loc, yr) { 
        FirstFlwr_sub <- subset(sum_FirstFlwr, site == loc & year == yr)
        return(ggplot(FirstFlwr_sub, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Day of First Flower", title = yr, subtitle = loc) +
                       scale_fill_manual(values = c("blue", "darkred")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

# kbs plots
kbs_2015 <- sum_FirstFlwr_plot("kbs", "2015")
kbs_2016 <- sum_FirstFlwr_plot("kbs", "2016")
kbs_2017 <- sum_FirstFlwr_plot("kbs", "2017")
kbs_2018 <- sum_FirstFlwr_plot("kbs", "2018")
kbs_2019 <- sum_FirstFlwr_plot("kbs", "2019")
kbs_2020 <- sum_FirstFlwr_plot("kbs", "2020")

plot_grid(kbs_2015, kbs_2016, kbs_2017, kbs_2018, kbs_2019, kbs_2020,
          ncol = 3, nrow = 2)

# umbs plots
umbs_2016 <- sum_FirstFlwr_plot("umbs", "2016")
umbs_2017 <- sum_FirstFlwr_plot("umbs", "2017")
umbs_2018 <- sum_FirstFlwr_plot("umbs", "2018")
umbs_2019 <- sum_FirstFlwr_plot("umbs", "2019")
umbs_2020 <- sum_FirstFlwr_plot("umbs", "2020")

plot_grid(umbs_2016, umbs_2017, umbs_2018, umbs_2019, umbs_2020,
          ncol = 3, nrow = 2)

# calculate the average date of first flower for each plot