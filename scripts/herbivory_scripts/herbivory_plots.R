# TITLE:          Herbivory plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 herbivory folder
# DATA OUTPUT:    Plots for herbivory for each species and per site
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
herb <- read.csv("L1/herbivory/final_herbivory_L1.csv")
str(herb) # for some reason, date column converted back to character

# Fix date column & add column for the year and julian day
herb$date <- as.Date(herb$date,format="%Y-%m-%d")
str(herb)

# Remove weird X column
herb <- subset(herb, select = -X)



#### Total herb by site and species (ignoring insecticide) ####
sum_herb <- herb %>%
  group_by(site, state, species, year) %>%
  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
            se = std.error(p_eaten, na.rm = TRUE))

# Function to make a plot for any species
herb_plot <- function(spp, loc) { 
  herb_spp <- subset(sum_herb, species == spp & site == loc)
  return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity") +
           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Average Percent of Leaf Eaten", title = spp) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
herb_plot("Popr", "umbs")
herb_plot("Eugr", "kbs")
herb_plot("Soca", "kbs")


############### FIX #################
#### Total herb by site and species with insecticide treatment####
sum_herb_in <- herb %>%
  group_by(site, treatment_key, year) %>%
  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
            se = std.error(p_eaten, na.rm = TRUE))

# Function to make a plot for any species
herb_plot_in <- function(loc) { 
  herb_spp <- subset(sum_herb_in, site == loc)
  return(ggplot(herb_spp, aes(x = treatment_key, y = avg_eaten, fill = treatment_key)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity") +
           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                         position = "identity") +
           labs(x = "Treatment", y = "Average Percent of Leaf Eaten", title = loc) +
           scale_fill_manual(values = c("#abd9e9", "#2c7bb6", "#fdae61", "#d7191c")) +
           theme_classic())
}
herb_plot_in("umbs")
herb_plot_in("kbs")



#### Total herb by site ####
sum_herb_all <- herb %>%
  group_by(site, state, year) %>%
  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
            se = std.error(p_eaten, na.rm = TRUE))

# Plot for all species between warmed and ambient
herb_plot_all <- function(loc) { 
  herb_spp <- subset(sum_herb_all, site == loc)
  return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity") +
           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Average Percent of Leaf Eaten", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
herb_plot_all("umbs")
herb_plot_all("kbs")
