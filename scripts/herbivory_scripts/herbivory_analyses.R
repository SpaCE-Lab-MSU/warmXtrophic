# TITLE:          Herbivory data analysis
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
herb <- read.csv("L1/herbivory/final_herbivory_L1.csv")


##### attempting different models #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(p_eaten~state*insecticide, data = herb)
summary(lm1)

# mixed effects model (plot random)
lme1 <- lme(p_eaten~state*insecticide, random=~1|plot, data = herb)
summary(lme1)
coef(lme1)

# mixed effects model (plot and species random)
lme2 <- lme(p_eaten~state*insecticide, random=~1|plot/species, data = herb)
summary(lme2)
coef(lme2)
