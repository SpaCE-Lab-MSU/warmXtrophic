# TITLE:          Plant composition data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(lme4)
library(nlme)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data & metadata
plant_comp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")

# First date by site
filter_comp <- plant_comp %>%
  group_by(plot, year, species, state, insecticide, treatment_key, site) %>%
  summarize(julian = min(julian))


##### attempting different models #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(julian~state*insecticide, data = filter_comp)
summary(lm1)

# mixed effects model -> after running lme with plot as random effect, may not be needed (plot doesn't affect intercepts in coef)
# below does not give p values from lme4, so I switched to nlme instead
#lme1 <- lmer(julian~state*insecticide + (1 | plot), data=filter_comp)
#summary(lme1)
lme1 <- lme(julian~state*insecticide, random=~1|plot, data = filter_comp)
summary(lme1)
coef(lme1)

# note to self: include species as random effect?
# no sig difference between warmed and ambient with or without insecticide
# could be due to the chambers not influencing warming in early spring as much as in summer?

