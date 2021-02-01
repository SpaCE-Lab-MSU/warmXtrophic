# TITLE:          Greenup plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    Plots for greenup data for each species and per site
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

# Read in plant comp data
greenup <- read.csv("L1/greenup/final_greenup_L1.csv")
str(greenup)

# remove uneeded X column
greenup$X <- NULL


# filter data to contain the averages and std error for each site & species
sum_green_spp <- greenup %>%
  group_by(site, state, species, year) %>%
  summarize(avg_julian = mean(half_cover_date, na.rm = TRUE),
            se = std.error(half_cover_date, na.rm = TRUE))

# Function to make a plot for any species
greenup_plot <- function(spp, loc) { 
  greenup_spp <- subset(sum_green_spp, species == spp & site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Julian Day of Greenup", title = spp) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           #coord_cartesian(ylim = c(100, 250)) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
greenup_plot("Popr", "umbs")
greenup_plot("Eugr", "kbs")
greenup_plot("Soca", "kbs")



# filter data to contain the averages and std error for each site
sum_green_site <- greenup %>%
  group_by(site, state, year) %>%
  summarize(avg_julian = mean(half_cover_date, na.rm = TRUE),
            se = std.error(half_cover_date, na.rm = TRUE))

# Plot for all species between warmed and ambient
greenup_plot_all <- function(loc) { 
  greenup_spp <- subset(sum_green_site, site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Julian Day of Greenup", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(100, 200)) +
           theme_classic())
}
greenup_plot_all("umbs")
greenup_plot_all("kbs")



# by plant origin (native/exotic)
sum_green_org <- greenup %>%
  group_by(site, origin, state) %>%
  summarize(avg_julian = mean(half_cover_date, na.rm = TRUE),
            se = std.error(half_cover_date, na.rm = TRUE))
sum_green_org <- subset(sum_green_org, origin == "Exotic" | origin == "Native")

greenup_plot_org <- function(loc) { 
  greenup_spp <- subset(sum_green_org, site == loc)
  return(ggplot(greenup_spp, aes(x = origin, y = avg_julian, fill = state)) +
           #facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = "State", y = "Julian Day of Greenup", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(100, 200)) +
           theme_classic())
}
greenup_plot_org("umbs")
greenup_plot_org("kbs")



# by plant growth type (forb, graminoid, shrub)
sum_green_habit <- greenup %>%
  group_by(site, growth_habit, state) %>%
  summarize(avg_julian = mean(half_cover_date, na.rm = TRUE),
            se = std.error(half_cover_date, na.rm = TRUE))
sum_green_habit <- subset(sum_green_habit, growth_habit == "Forb" | growth_habit == "Graminoid" | growth_habit == "Shrub")

greenup_plot_habit <- function(loc) { 
  greenup_spp <- subset(sum_green_habit, site == loc)
  return(ggplot(greenup_spp, aes(x = growth_habit, y = avg_julian, fill = state)) +
           #facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = "State", y = "Julian Day of Greenup", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(100, 200)) +
           theme_classic())
}
greenup_plot_habit("umbs")
greenup_plot_habit("kbs")  
