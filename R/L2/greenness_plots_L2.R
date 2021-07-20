# TITLE:          warmXtrohpic - Greenness
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    Plots
# PROJECT:        warmXtrophic
# DATE:           July, 2021

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)

# Set working directory
L1_dir<-Sys.getenv("L1DIR")

# Read in data
green <- read.csv(file.path(L1_dir, "Greenness/greenness_L1.csv"))

# Take individual plant average
green2 <- green %>%
        group_by(plot, plant_number, state) %>%
        summarize(greenness = mean(greenness, na.rm = TRUE))

# Boxplot
ggplot(green2, aes(x = state, y = greenness, fill = state)) +
        geom_boxplot(color = "black") +
        labs(x = "Treatment", y = "Greenness level") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        geom_jitter(shape=16, position=position_jitterdodge(), alpha = 0.6, aes(colour = state)) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_classic()
