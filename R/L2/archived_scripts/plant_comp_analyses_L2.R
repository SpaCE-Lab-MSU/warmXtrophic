# TITLE:          Plant composition data analysis
# AUTHORS:        Kara Dobson, Moriah Young
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

# Set working directory
Sys.getenv("L1DIR")
L1_dir <- Sys.getenv("L1DIR")
L2_dir <- Sys.getenv("L2DIR")
list.files(L1_dir)

# read in plant comp data
comp <- read.csv(file.path(L1_dir, "plant_composition/final_plantcomp_L1.csv"))
  
# check variable types
str(comp)

# adding sequential year variable starting at 1: this is because 2016... are large numbers compare with other 
# values in the dataset. We can always label axes with these real years.
comp$year_factor[comp$year == 2015] <- 1
comp$year_factor[comp$year == 2016] <- 1
comp$year_factor[comp$year == 2017] <- 2
comp$year_factor[comp$year == 2018] <- 3
comp$year_factor[comp$year == 2019] <- 4
comp$year_factor[comp$year == 2020] <- 5

# calculating total composition sums - proxy for most common species
comp$cover <- as.numeric(comp$cover)
comp2 <- comp
comp2 <- comp2[!comp2$species %in% c("Litter", "Vert_Litter","Animal_Disturbance","Bare_Ground"), ]
comp_yearly_totals <- comp2 %>%
        filter(year == 2021 & state == "ambient") %>%
        group_by(site, species) %>%
        summarize(comp_sum =sum(cover, na.rm = T)) %>%
        arrange(site, desc(comp_sum))
tab_df(comp_yearly_totals)

# getting relative % cover for comparisons between native & exotic #
# most code from Kileigh's old script
# average sub-quadrats for plots
comp_org <- subset(comp, origin == "Exotic" | origin == "Native")
quad.mn <- aggregate(cover ~ plot*origin*species*year*site*state, data=comp_org, FUN=mean, na.rm=T) #quad.mn = quadrat mean
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"
head(quad.mn)

# convert cover to relative abundance 
# first get summed cover for all plants per plot
cover.sum = aggregate(quad.mn ~ plot*origin*year*site*state, data=quad.mn, FUN=sum, na.rm=T)
names(cover.sum)[names(cover.sum)=="quad.mn"]<-"cover.sum"
head(cover.sum)
comp2 <- merge(quad.mn,cover.sum, by=c("plot","origin","year","site", "state"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
comp2$relabun <- comp2$quad.mn/comp2$cover.sum
summary(comp2)

# create dataframes for kbs and umbs - remember that these contain species within plots
comp_kbs <- subset(comp2, site == "kbs")
comp_umbs <- subset(comp2, site == "umbs")


# checking for normality in raw data
hist(log(comp_kbs$relabun))
qqnorm(log(comp_kbs$relabun))
shapiro.test(log(comp_kbs$relabun))

hist(comp_kbs$relabun[comp_kbs$state == "ambient"])
hist(comp_kbs$relabun[comp_kbs$state == "warmed"])
