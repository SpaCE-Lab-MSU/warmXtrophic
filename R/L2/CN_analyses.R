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
library(bbmle)
library(lme4)

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

# separate dataframes for each species
cn_cest_umbs <- subset(cn_umbs, species == "Cest")
cn_popr_umbs <- subset(cn_umbs, species == "Popr")

# data visualization check
# KBS - Soca
hist(cn_kbs$carbon)
qqnorm(cn_kbs$carbon)
shapiro.test(cn_kbs$carbon)

hist(cn_kbs$nitrogen)
qqnorm(cn_kbs$nitrogen)
shapiro.test(cn_kbs$nitrogen)

 # UMBS - Cest
hist(cn_cest_umbs$carbon)
qqnorm(cn_cest_umbs$carbon)
shapiro.test(cn_cest_umbs$carbon)

hist(cn_cest_umbs$nitrogen)
qqnorm(cn_cest_umbs$nitrogen)
shapiro.test(cn_cest_umbs$nitrogen)

# UMBS - Popr
hist(cn_popr_umbs$carbon)
qqnorm(cn_popr_umbs$carbon)
shapiro.test(cn_popr_umbs$carbon)

hist(cn_popr_umbs$nitrogen)
qqnorm(cn_popr_umbs$nitrogen)
shapiro.test(cn_popr_umbs$nitrogen)


### Model comparison ###
# KBS - Soca
m1a <- lm(carbon ~ state, data = cn_kbs)
m1b <- lm(carbon ~ state + insecticide, data = cn_kbs)
m1c <- lmer(carbon ~ state + (1|plot), data = cn_kbs)
m1d <- lmer(carbon ~ state + insecticide + (1|plot), data = cn_kbs)
AICctab(m1a, m1b, m1c, m1d, weights = T)
summary(m1a) # not including plot
summary(m1c) # plot as a random effect


m2a <- lm(nitrogen ~ state, data = cn_kbs)
m2b <- lm(nitrogen ~ state + insecticide, data = cn_kbs)
m2c <- lmer(nitrogen ~ state + (1|plot), data = cn_kbs)
m2d <- lmer(nitrogen ~ state + insecticide + (1|plot), data = cn_kbs)
AICctab(m2a, m2b, m2c, m2d, weights = T)
summary(m2a) # not including plot
summary(m2c) # plot as a random effect

# UMBS - Cest
m3a <- lm(carbon ~ state, data = cn_cest_umbs)
m3b <- lm(carbon ~ state + insecticide, data = cn_cest_umbs)
m3c <- lmer(carbon ~ state + (1|plot), data = cn_cest_umbs)
m3d <- lmer(carbon ~ state + insecticide + (1|plot), data = cn_cest_umbs)
AICctab(m3a, m3b, m3c, m3d, weights = T)
summary(m3a)
summary(m3c)

m4a <- lm(nitrogen ~ state, data = cn_cest_umbs)
m4b <- lm(nitrogen ~ state + insecticide, data = cn_cest_umbs)
m4c <- lmer(nitrogen ~ state + (1|plot), data = cn_cest_umbs)
m4d <- lmer(nitrogen ~ state + insecticide + (1|plot), data = cn_cest_umbs)
AICctab(m4a, m4b, m4c, m4d, weights = T)
summary(m4a)
summary(m4c)

# UMBS - Popr
m5a <- lm(carbon ~ state, data = cn_popr_umbs)
m5b <- lm(carbon ~ state + insecticide, data = cn_popr_umbs)
m5c <- lmer(carbon ~ state + (1|plot), data = cn_popr_umbs)
m5d <- lmer(carbon ~ state + insecticide + (1|plot), data = cn_popr_umbs)
AICctab(m5a, m5b, m5c, m5d, weights = T)
summary(m5a)
summary(m5c)

m6a <- lm(nitrogen ~ state, data = cn_popr_umbs)
m6b <- lm(nitrogen ~ state + insecticide, data = cn_popr_umbs)
m6c <- lmer(nitrogen ~ state + (1|plot), data = cn_popr_umbs)
m6d <- lmer(nitrogen ~ state + insecticide + (1|plot), data = cn_popr_umbs)
AICctab(m6a, m6b, m6c, m6d, weights = T)
summary(m6a)
summary(m6c)

