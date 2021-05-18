# TITLE:          Carbon and Nitrogen Data Visualization
# AUTHORS:        Kara Dobson, Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN data is uploaded to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           March, 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(plotrix)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
cn <- read.csv("L1/final_CN_L1.csv")
str(cn)

### carbon ###
ggplot(cn, aes(x = species, y = carbon, fill = state)) +
        #facet_grid(.~site) +
        geom_boxplot(color = "black", outlier.shape = NA) +
        labs(x = "Species", y = "Average Carbon Content") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
        geom_jitter(shape=16, position=position_jitterdodge(), alpha = 0.6, aes(colour = state)) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        coord_cartesian(ylim = c(42, 53)) +
        theme_classic()

### nitrogen ###
ggplot(cn, aes(x = species, y = nitrogen, fill = state)) +
        #facet_grid(.~site) +
        geom_boxplot(color = "black", outlier.shape = NA) +
        labs(x = "Species", y = "Average Nitrogen Content") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
        geom_jitter(shape=16, position=position_jitterdodge(), alpha = 0.6, aes(colour = state)) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_classic()
