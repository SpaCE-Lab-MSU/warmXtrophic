# TITLE: Phenology data analysis
# AUTHORS: Moriah Young
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Kara Dobson
# DATA INPUT: CSV files are located in the phenology folder in the shared Google drive
# DATA OUTPUT: Stats results
# PROJECT: warmXtrophic
# DATE: February 2020

##########################################################################################
# Set Up
# clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(vegan)
library(car)
library(rstatix)
library(scales)
library(fitdistrplus)
library(moments)# for calculating skewness of data
library(ggpubr)
library(jtools) # summ() function
library(predictmeans)
library(olsrr)
library(car)
library(fitdistrplus)
library(ggpubr)
library(interactions)
library(sjPlot)
library(effects)
library(glmmTMB)
library(GGally) # ggpairs() function

# Set working directory
Sys.getenv("L1DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L1_dir)

##########################################################################################
# Read in data
# cleaned phenology data from L1
phen_data <- read.csv(file.path(L1_dir, "phenology/final_flwr_sd_L1.csv"))
phen_data$X <- NULL # get rid of "X" column that shows up
View(phen_data) # take a look at the data to see if looks good

# Order warm and ambient so that warm shows up first in plotting (and is default is red = warm; blue = ambient). First make it a factor
phen_data$state <- as.factor(phen_data$state)
levels(phen_data$state)
# [1] "ambient" "warmed" 
phen_data$state <- factor(phen_data$state, levels(phen_data$state)[c(2,1)])
levels(phen_data$state)
# [1] "warmed"  "ambient"

# flowering data
# species level data for flowering
flwr_spp <- read.csv(file.path(L1_dir, "phenology/final_flwr_species_L1.csv"))
flwr_spp$X <- NULL

# plot level data for flowering
flwr_plot <- read_csv(file.path(L1_dir, "phenology/final_flwr_plot_L1.csv"))
flwr_plot$X1 <- NULL

# seed set data
# species level data for seed set
seed_spp <- read.csv(file.path(L1_dir, "phenology/final_sd_species_L1.csv"))
seed_spp$X <- NULL

# plot level data for seed set
seed_plot <- read.csv(file.path(L1_dir, "phenology/final_sd_plot_L1.csv"))
seed_plot$X <- NULL

##########################################################################################
# Data Exploration
umbs_firstflwr_spp <- subset(flwr_spp, site == "umbs") # pull out umbs only data
hist(umbs_firstflwr_spp$julian_median)
hist(umbs_firstflwr_spp$julian_min)

umbs_firstflwr_plot <- subset(flwr_plot, site == "umbs") # pull out umbs only data
hist(umbs_firstflwr_plot$julian_median)
hist(umbs_firstflwr_plot$julian_min)

# Response variable:
# julian_median

# Predictor variables:
# state ("ambient" or "warmed")

# Grouping factor:
# year

umbs_firstflwr_plot$julianMedian_centered = umbs_firstflwr_plot$julian_median - mean(umbs_firstflwr_plot$julian_median)
umbs_firstflwr_plot$julianMedian_centered

plot(julianMedian_centered ~ state, data = umbs_firstflwr_plot)
##########################################################################################
# Centering and transforming data


##########################################################################################
# Plot key relationships


##########################################################################################
# Fit models


##########################################################################################
# Compare models


##########################################################################################
# Evaluate models using residuals





