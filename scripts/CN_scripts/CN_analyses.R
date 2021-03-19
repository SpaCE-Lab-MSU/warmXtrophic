# TITLE:          Carbon and Nitrogen Data Analysis
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
library(lmerTest)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
cn <- read.csv("L1/final_CN_L1.csv")

# create dataframes for kbs and umbs
cn_kbs <- subset(cn, site == "kbs")
cn_umbs <- subset(cn, site == "umbs")

# check species at each site
unique(cn_kbs$species)
unique(cn_umbs$species)

# data visualization check
hist(cn_kbs$carbon)
qqnorm(cn_kbs$carbon)
shapiro.test(cn_kbs$carbon)

hist(cn_kbs$nitrogen[cn_umbs$species == "Cest"])
qqnorm(cn_kbs$nitrogen[cn_umbs$species == "Cest"])
shapiro.test(cn_kbs$nitrogen[cn_umbs$species == "Cest"])

hist(cn_kbs$nitrogen[cn_umbs$species == "Popr"])
qqnorm(cn_kbs$nitrogen[cn_umbs$species == "Popr"])
shapiro.test(cn_kbs$nitrogen[cn_umbs$species == "Popr"])

# simple linear modeal
m1 <- lm(carbon ~ state, data = cn_kbs)
summary(m1)

m2 <- lm(nitrogen ~ state, data = cn_kbs)
summary(m2)
