# TITLE:          Plant composition data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           March 2021


# Main questions (for now): how does plant composition differ between native & exotic species over time?
    # in general, how does relative % cover change over time btwn each treatment?

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(ggplot2)
library(lme4)
library(olsrr)
library(predictmeans)
library(car)
library(fitdistrplus)
library(ggpubr)
library(rstatix)
library(vegan)
library(interactions)
library(sjPlot)
library(effects)
library(glmmTMB)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# read in plant comp data
comp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
  
# check variable types
str(comp)

# adding sequential year variable starting at 1: this is because 2016... are large numbers compare with other values in the dataset. We can always label axes with these real years.
comp$year1<-comp$year
comp$year[comp$year == 2015] <- 1
comp$year[comp$year == 2016] <- 1
comp$year[comp$year == 2017] <- 2
comp$year[comp$year == 2018] <- 3
comp$year[comp$year == 2019] <- 4
comp$year[comp$year == 2020] <- 5

# getting relative % cover for comparisions between native & exotic #
# average sub-quadrats for plots
comp_org <- subset(comp, origin == "Exotic" | origin == "Native")
quad.mn <- aggregate(cover ~ plot*origin*species*year*site*state, data=comp_org, FUN=mean, na.rm=T)
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"

head(quad.mn)

# convert cover to relative abundance 
# first get summed cover for all plants per plot
cov.sum = aggregate(quad.mn ~ plot*origin*year*site*state, data=quad.mn, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="quad.mn"]<-"cov.sum"
head(cov.sum)
comp2 <- merge(quad.mn,cov.sum, by=c("plot","origin","year","site"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
comp2$relab <- comp2$quad.mn/comp2$cov.sum
summary(comp2)

# create dataframes for kbs and umbs - remember that these contain species within plots
comp_kbs <- subset(comp2, site == "kbs")
comp_umbs <- subset(comp2, site == "umbs")


# checking for normality in raw data
hist(log(comp_kbs$relab))
qqnorm(log(comp_kbs$relab))
shapiro.test(log(comp_kbs$relab))

hist(comp_kbs$relab[comp_kbs$state == "ambient"])
hist(comp_kbs$relab[green_kbsp$state == "warmed"])