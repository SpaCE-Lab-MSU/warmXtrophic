# TITLE:          Solidago canadensis data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           November, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(plotrix)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
anpp <- read.csv("L0/KBS/2020/kbs_ancillary_ANPP_2020.csv")
heights <- read.csv("L0/KBS/2020/kbs_ancillary_heights_2020.csv")
stem_data <- read.csv("L0/KBS/2020/kbs_indiv_soca_stem_data_2020.csv")
stem_counts <- read.csv("L0/KBS/2020/kbs_soca_stem_counts_2020.csv")


############# Heights ###############
# Check that no species names are mispelled
unique(heights$species) # looks good

# Clean height data
heights_averages <- heights %>%
  filter(observation == "five") %>%
  filter(treatment == "ambient" | treatment == "warmed") %>%
  filter(species == "Soca") %>%
  group_by(treatment) %>%
  summarize(average_height = mean(height_cm, na.rm = TRUE),
            se = std.error(height_cm, na.rm = TRUE))

# Bar plot of goldenrod height between ambient & warmed treatments
ggplot(heights_averages, aes(x = treatment, y = average_height)) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  geom_errorbar(aes(ymin = average_height - se, ymax = average_height + se), width = 0.2,
                position = "identity") +
  labs(y = "Goldenrod Height (cm)", x = "Treatment") +
  scale_x_discrete(breaks=c("ambient","warmed"),
                   labels=c("Ambient", "Warmed")) +
  theme_classic()


############# Stem counts ###############
# Clean the data
stem_counts_average <- stem_counts %>%
  group_by(treatment) %>%
  summarize(average_stem_count = mean(stem_count, na.rm = TRUE),
            se = std.error(stem_count, na.rm = TRUE))

# Bar plot of goldenrod height between ambient & warmed treatments
# Ancillary kept for now - why is there such a large difference btwn ambient + ancillary?
ggplot(stem_counts_average, aes(x = treatment, y = average_stem_count)) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  geom_errorbar(aes(ymin = average_stem_count - se, ymax = average_stem_count + se), width = 0.2,
                position = "identity") +
  labs(y = "Number of Goldenrod Stems", x = "Treatment") +
  scale_x_discrete(breaks=c("ambient","ancillary", "warmed"),
                   labels=c("Ambient", "Ancillary", "Warmed")) +
  theme_classic()
