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
phen_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/reproductive_phenology/final_flw_sd_L1.csv")
phen_data <- phen_data %>% 
        select(-X)

# Change column names to lowercase
names(phen_data) <- tolower(names(phen_data))
str(phen_data)

# Fix date column & add column for the year and julian day
phen_data$date <- as.Date(phen_data$date, format="%Y-%m-%d")
str(phen_data)
phen_data$year <- format(phen_data$date,format="%Y")
phen_data$julian <- format(phen_data$date, "%j")

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

# Change column names to lowercase (again)
names(phen_data) <- tolower(names(phen_data))
str(phen_data)

# Make julian column numeric
phen_data$julian <- as.numeric(phen_data$julian)
str(phen_data)

# Create separate data frames for flowering and seeding
phen_data_flwr <- subset(phen_data, action == "flower")
phen_data_sd <- subset(phen_data, action == "seed")

# This creates a data frame that returns the first date of flower per species per plot
# Filter data to contain the date of first occurrence for each species

FirstFlower <- phen_data_flwr %>%
        group_by(plot, year, species, state, site, action) %>%
        summarize(julian = min(julian, na.rm=T))

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
FirstFlower_plot("Cest", "umbs")
FirstFlower_plot("Eugr", "kbs")
FirstFlower_plot("Soca", "kbs")

# create a function that returns plots (1 for each year at each site) for a given site and year comparing
# warmed vs ambient plots for average first date of flower (add by functional groups next?)

# calculate the average date of first flower for each plot
