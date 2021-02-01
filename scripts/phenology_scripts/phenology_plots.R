# TITLE:          Phenology Data Visualization
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
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

############################### Read in data #################################3
# cleaned phenology data from L1
phen_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/phenology/final_flw_sd_L1.csv", stringsAsFactors=FALSE)
phen_data <- phen_data %>% 
        select(-X) # get rid of "X" column that shows up
View(phen_data) # take a look at the data to see if looks good

# Create separate data frames for flowering and seeding
phen_flwr <- subset(phen_data, action == "flower")
phen_sd <- subset(phen_data, action == "seed")

###################### Flowering ##########################################

#### First Flower #####

# Filter data to contain the date of first flower for each species at each plot
FirstFlower_all <- phen_flwr %>%
        group_by(plot, year, species, state, site, action, origin) %>%
        summarize(julian = min(julian, na.rm=T))

# Filter FirstFlower_all data set to calculate the mean and sd for julian date of first flower for each species at ambient
# and warmed plots by YEAR
sum_FirstFlower <- FirstFlower_all %>%
        group_by(site, state, species, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE))

# This creates function so that you can look at a specific species at either kbs or umbs and it's mean julian day of first flower for every year
# of data collection
FirstFlower_plot <- function(spp, loc) { 
        FirstFlower_spp <- subset(sum_FirstFlower, species == spp & site == loc)
        return(ggplot(FirstFlower_spp, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Day of First Flower", title = spp, subtitle = loc) +
                       coord_cartesian(ylim = c(100, 250)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

FirstFlower_plot("Popr", "umbs")
FirstFlower_plot("Popr", "kbs")
FirstFlower_plot("Eugr", "kbs")
FirstFlower_plot("Soca", "kbs")

# This filters the FirstFlower_all data set to calculate the average julian day of first flower combining all species for each site 
# and year for warmed and ambient
sum_FirstFlwr_state <- FirstFlower_all %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE)) 

# This creates a function that returns plots for a given site and year for mean julian day of first flower comparing ambient vs warmed plots
sum_FirstFlwr_plot <- function(loc) { 
        FirstFlwr_sub <- subset(sum_FirstFlwr_state, site == loc)
        return(ggplot(FirstFlwr_sub, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Day of First Flower", title = loc) +
                       coord_cartesian(ylim = c(150, 200)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

sum_FirstFlwr_plot("kbs")
sum_FirstFlwr_plot("umbs")

# We want to see the graphs for kbs and umbs side by side, so using "cowplot" to do this
#kbs <- sum_FirstFlwr_plot("kbs")
#umbs <- sum_FirstFlwr_plot("umbs")

#plot_grid(kbs, umbs,
#         ncol = 2, nrow = 1)

# Filter FirstFlower_all by plant origin (native/exotic)
sum_flwr_org <- FirstFlower_all %>%
        group_by(site, origin, state, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE))
sum_flwr_org <- subset(sum_flwr_org, origin == "Exotic" | origin == "Native")
View(sum_flwr_org)

flwr_org_plot <- function(loc) { 
        flwr_spp <- subset(sum_flwr_org, site == loc)
        return(ggplot(flwr_spp, aes(x = origin, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "dodge", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = position_dodge(0.9)) +
                       labs(x = "State", y = "Julian Day of First Flower", title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       #coord_cartesian(ylim = c(100, 200)) +
                       theme_grey())
}
flwr_org_plot("kbs")
flwr_org_plot("umbs")

##### Flowering Duration #####

# Filter data to calculate the average duration of flowering time for each species at every plot and year
flwr_duration <- phen_flwr %>% 
        group_by(site, plot, species, year, state, action, origin) %>%
        summarise(flwr_duration = max(julian) - min(julian)) 
#summarise(result = diff(range(julian)))

# Filter flwr_duration data frame to calculate the mean duration by native vs exotic 
sum_flwrduration_org <- flwr_duration %>% 
        group_by(site, state, action, origin) %>% 
        summarise(mean_duration = mean(flwr_duration))
View(sum_flwr_duration) 

# Filter flwr_duration data set by state and year and calculate mean duration of flowering
sum_flwrduration_state <- flwr_duration %>% 
        group_by(site, plot, state, action, year) %>% 
        summarise(mean_duration = mean(flwr_duration))

sum_FlwrDurState_plot <- function(loc) { 
        flwr_Duration1 <- subset(sum_flwrduration_state, site == loc)
        return(ggplot(flwr_Duration1, aes(x = state, y = mean_duration, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       #geom_errorbar(aes(ymin = mean_duration - se, ymax = mean_duration + se), width = 0.2,
                                     #position = "identity") +
                       labs(x = "State", y = "Average Julian Days of Flowering", title = loc) +
                       #coord_cartesian(ylim = c(150, 200)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

sum_FlwrDurState_plot("kbs")
sum_FlwrDurState_plot("umbs")

##################### Seeding #########################

# Filter data to contain the date of first flower for each species at each site and year
FirstSeed_all <- phen_sd %>%
        group_by(plot, year, species, state, site, action) %>%
        summarize(julian = min(julian, na.rm=T))

# Filter FirstSeed_all data set to calculate the mean and sd for julian date of first seed for each species at ambient
# and warmed plots by YEAR        
sum_FirstSeed <- FirstSeed_all %>%
        group_by(site, state, species, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE))

# This filters the FirstSeed_all data set to calculate the average julian day of first seed combining all species for each site 
# and year for warmed and ambient
sum_FirstSeed_state <- FirstSeed_all %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE)) 

# This creates function so that you can look at a specific species at either kbs or umbs and it's mean julian day of first seed for every year
# of data collection
FirstSeed_plot <- function(spp, loc) { 
        FirstSeed_spp <- subset(sum_FirstSeed, species == spp & site == loc)
        return(ggplot(FirstSeed_spp, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Day of First Seed", title = spp, subtitle = loc) +
                       #coord_cartesian(ylim = c(150, 300)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

FirstSeed_plot("Popr", "umbs")
FirstSeed_plot("Popr", "kbs")
FirstSeed_plot("Eugr", "kbs")
FirstSeed_plot("Soca", "kbs")

# This creates a function that returns plots for a given site and all years for mean julien day of first seed 
# comparing ambient vs warmed plots
sum_FirstSeed_plot <- function(loc) { 
        FirstSeed_sub <- subset(sum_FirstSeed_state, site == loc)
        return(ggplot(FirstSeed_sub, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Day of First Seed", title = loc) +
                       coord_cartesian(ylim = c(150, 250)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme_grey())
}

sum_FirstSeed_plot("kbs")
sum_FirstSeed_plot("umbs")

