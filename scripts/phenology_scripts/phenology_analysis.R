# TITLE:          Phenology Data Analysis
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L1 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           February, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(vegan)

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

kbs_flwr <- subset(phen_flwr, site == "kbs")
umbs_flwr <- subset(phen_flwr, site == "umbs")

# HA Plants in warmed plots flower earlier than those in ambient plots
# H0 Plants in warmed and ambient plots flower at the same time

# state = categorical
# julian day = numerical 
# t - test? 

# Filter data to contain the date of first flower for each species at each plot
umbs_firstflwr <- umbs_flwr %>%
        group_by(plot, year, species, state, site, action, origin) %>%
        summarize(julian = min(julian, na.rm=T))

t.test(data = umbs_firstflwr, julian ~ state)

lm(julian ~ state, data = umbs_firstflwr)
summary(lm(julian ~ state, data = umbs_firstflwr))
plot(lm(julian ~ state, data = umbs_firstflwr))

kbs_firstflwr <- kbs_flwr %>%
        group_by(plot, year, species, state, site, action, origin) %>%
        summarize(julian = min(julian, na.rm=T))

t.test(data = kbs_firstflwr, julian ~ state)


