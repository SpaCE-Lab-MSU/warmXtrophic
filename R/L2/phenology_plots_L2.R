# TITLE:          warmXtrophic Project: Phenology Data Visualization
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L1 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           January, 2020; updated July, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(plotrix)
library(cowplot)
library(ggpubr)

# Set working directory
Sys.getenv("L1DIR")
L1_dir <- Sys.getenv("L1DIR")
L2_dir <- Sys.getenv("L2DIR")
list.files(L1_dir)

############################### Read in data #################################3
# cleaned phenology data from L1
phen_data <- read.csv(file.path(L1_dir, "phenology/final_flwr_sd_L1.csv"))
phen_data <- phen_data %>% 
        select(-X) # get rid of "X" column that shows up
View(phen_data) # take a look at the data to see if looks good

# Create separate data frames for flowering and seeding
phen_flwr <- subset(phen_data, action == "flower")
phen_sd <- subset(phen_data, action == "seed")

flwr_plot <- read.csv(file.path(L1_dir, "phenology/final_flwr_plot_L1.csv"))
flwr_species <- read.csv(file.path(L1_dir, "phenology/final_flwr_species_L1.csv")) 
sd_plot <- read.csv(file.path(L1_dir, "phenology/final_sd_plot_L1.csv"))
sd_species <- read.csv(file.path(L1_dir, "phenology/final_sd_species_L1.csv"))

# Set ggplot2 plotting
# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(14))
theme_update(axis.text.x = element_text(size = 12, angle = 90),
             axis.text.y = element_text(size = 12))

###################### Flowering ##########################################

#### Median Flower ####
MedFlwr_all <- phen_flwr %>%
        group_by(plot, year, species, state, site, action, origin) %>%
        summarize(julian = median(julian, na.rm=T))

sum_MedFlwr <- MedFlwr_all %>%
        group_by(site, state, year) %>%
        summarize(med_julian = median(julian, na.rm=TRUE),
                  se = std.error(julian, na.rm = TRUE))
                  
sum_MedFlwr_plot <- function(loc) { 
        MedFlwr_sub <- subset(sum_MedFlwr, site == loc)
        return(ggplot(MedFlwr_sub, aes(x = state, y = med_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = med_julian - se, ymax = med_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       coord_cartesian(ylim = c(150, NA)))
}

sum_MedFlwr_plot("kbs")
sum_MedFlwr_plot("umbs")

# Create a plot where both kbs and umbs are on the same image
all_Med_u <- sum_MedFlwr_plot("umbs")
all_Med_k <- sum_MedFlwr_plot("kbs")

final_Med_all <- ggarrange(all_Med_k, all_Med_u, nrow = 2, legend = "none")
annotate_figure(final_Med_all,
                left = text_grob("Julian Day of Median Flower", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))


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
                       geom_bar(position = "identity", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = "State", y = "Julian Date", title = spp, subtitle = loc) +
                       coord_cartesian(ylim = c(120, NA)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")))
}

FirstFlower_plot("Popr", "umbs")
FirstFlower_plot("Popr", "kbs")

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
                       geom_bar(position = "identity", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       coord_cartesian(ylim = c(150, NA)))
}

sum_FirstFlwr_plot("kbs")
sum_FirstFlwr_plot("umbs")

# Create a plot where both kbs and umbs are on the same image
all_u <- sum_FirstFlwr_plot("umbs")
all_k <- sum_FirstFlwr_plot("kbs")

final_all <- ggarrange(all_k, all_u, nrow = 2, legend = "none")
annotate_figure(final_all,
                left = text_grob("Julian Day of First Flower", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))

# boxplot (again, taking from Kara)
firstflwr_plot_box <- function(loc) { 
        phen_spp <- subset(FirstFlower_all, site == loc)
        return(ggplot(phen_spp, aes(x = state, y = julian, fill = state)) +
                       #facet_grid(.~year) +
                       geom_boxplot(color = "black") +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       coord_cartesian(ylim = c(0, 300)) +
                       theme(legend.position = "none") +
                       geom_jitter(shape=16, position=position_jitter(0.2)))
}

all_u <- firstflwr_plot_box("umbs")
all_k <- firstflwr_plot_box("kbs")

all_u #idk this looks weird
all_k

# We want to see the graphs for kbs and umbs side by side, so using "cowplot" to do this
#kbs <- sum_FirstFlwr_plot("kbs")
#umbs <- sum_FirstFlwr_plot("umbs")

#plot_grid(kbs, umbs,
#         ncol = 2, nrow = 1)

# Filter FirstFlower_all by plant origin (native/exotic)
sum_flwr_org <- FirstFlower_all %>%
        group_by(site, origin, state) %>%
        summarize(avg_julian = mean(julian, na.rm = TRUE),
                  se = std.error(julian, na.rm = TRUE))
sum_flwr_org <- subset(sum_flwr_org, origin == "Exotic" | origin == "Native")
View(sum_flwr_org)

flwr_org_plot <- function(loc) { 
        flwr_spp <- subset(sum_flwr_org, site == loc)
        return(ggplot(flwr_spp, aes(x = origin, y = avg_julian, fill = state)) +
                       #facet_grid(.~year) +
                       geom_bar(position = "dodge", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = position_dodge(0.9)) +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("Exotic" = "E", "Native" = "N")) +
                       coord_cartesian(ylim = c(100, NA)))
}

orgin_u <- flwr_org_plot("umbs")
orgin_k <- flwr_org_plot("kbs")

final_orgin <- ggarrange(orgin_k, orgin_u, ncol = 2, legend = "none")
annotate_figure(final_orgin,
                left = text_grob("Julian Day of First Flower", color = "black", rot = 90),
                bottom = text_grob("Origin", color = "black"))

##### Flowering Duration #####

# Filter data to calculate the duration of flowering time for each species at every plot and year
flwr_duration <- phen_flwr %>% 
        group_by(site, plot, species, year, state, action, origin) %>%
        summarise(flwr_duration = max(julian) - min(julian)) 
#summarise(result = diff(range(julian)))

# Filter flwr_duration data frame to calculate the mean duration by native vs exotic 
sum_flwrduration_org <- flwr_duration %>% 
        group_by(site, state, action, origin) %>% 
        summarise(mean_duration = mean(flwr_duration))
View(sum_flwrduration_org) 

# Filter flwr_duration data set by state and year and calculate mean duration of flowering
sum_flwrduration_state <- flwr_duration %>% 
        group_by(site, state, action, year) %>% 
        summarise(mean_duration = mean(flwr_duration),
                  se = std.error(flwr_duration, na.rm = TRUE))

sum_FlwrDurState_plot <- function(loc) { 
        flwr_Duration <- subset(sum_flwrduration_state, site == loc)
        return(ggplot(flwr_Duration, aes(x = state, y = mean_duration, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = mean_duration - se, ymax = mean_duration + se), width = 0.2,
                                     position = "identity") +
                       labs(x = NULL, y = NULL, title = loc) +
                       coord_cartesian(ylim = c(0, NA)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")))
}

all_k_dur <- sum_FlwrDurState_plot("kbs")
all_u_dur <- sum_FlwrDurState_plot("umbs")

final_all_dur <- ggarrange(all_k_dur, all_u_dur, nrow = 2, legend = "none")
annotate_figure(final_all_dur,
                left = text_grob("Julian Day of Mean Flower Duration", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))

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
                       labs(x = "State", y = "Julian Date", title = spp, subtitle = loc) +
                       #coord_cartesian(ylim = c(150, 300)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")))
}

FirstSeed_plot("Popr", "umbs")
FirstSeed_plot("Popr", "kbs")

# This creates a function that returns plots for a given site and all years for mean julien day of first seed 
# comparing ambient vs warmed plots
sum_FirstSeed_plot <- function(loc) { 
        FirstSeed_sub <- subset(sum_FirstSeed_state, site == loc)
        return(ggplot(FirstSeed_sub, aes(x = state, y = avg_julian, fill = state)) +
                       facet_grid(.~year) +
                       geom_bar(position = "identity", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                                     position = "identity") +
                       labs(x = NULL, y = NULL, title = loc) +
                       coord_cartesian(ylim = c(170, NA)) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")))
}

all_k_sd <- sum_FirstSeed_plot("kbs")
all_u_sd <- sum_FirstSeed_plot("umbs")

final_all_sd <- ggarrange(all_k_sd, all_u_sd, nrow = 2, legend = "none")
annotate_figure(final_all_sd,
                left = text_grob("Julian Day of First Seed", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))

