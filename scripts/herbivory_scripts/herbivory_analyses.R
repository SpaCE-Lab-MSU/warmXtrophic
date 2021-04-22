# TITLE:          Herbivory data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021 ; updated April 2021


##### Main questions ######
# Is herbivory different between warmed and ambient treatments? Hypothesis: Ambient plants will have higher herbivory
  # only run analyses on plots with no insecticide
  # include year as a treatment (is this difference seen each year)?
  # species + plot as random effects?
# Is herbivory different between warmed/ambient for native vs exotic? same for growth habit


# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(lmerTest)
library(olsrr)
library(predictmeans)
library(car)
library(fitdistrplus)
library(MASS)
library(pscl)
library(lmtest)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data & metadata
herb <- read.csv("L1/herbivory/final_herbivory_L1.csv")

# changing scale of years
herb$year1<-herb$year
herb$year[herb$year == 2015] <- 1
herb$year[herb$year == 2016] <- 2
herb$year[herb$year == 2017] <- 3
herb$year[herb$year == 2018] <- 4
herb$year[herb$year == 2019] <- 5
herb$year[herb$year == 2020] <- 6

# Remove NAs
!complete.cases(herb)
herb <- herb[complete.cases(herb),]

# create dataframes for kbs and umbs only for plots with no insecticide
herb_kbs <- subset(herb, site == "kbs" & insecticide == "insects")
herb_umbs <- subset(herb, site == "umbs" & insecticide == "insects")

# How much of the data is zeros?
100*sum(herb$p_eaten == 0)/nrow(herb) #69% - thats a lot! probably have to use a zero-inflated model,
# but I'll still check for normality & try some transformations below



####### kbs #########
### determining distribution ###
# first, checking for normality
hist(herb_kbs$p_eaten)
qqnorm(herb_kbs$p_eaten)
shapiro.test(herb_kbs$p_eaten)
fit <- lm(p_eaten~state, data = herb_kbs)
qqPlot(fit)
hist(herb_kbs$p_eaten[herb_kbs$state == "ambient"])
hist(herb_kbs$p_eaten[herb_kbs$state == "warmed"])
# not normal, attempting to transform data below

# log transform
herb_kbs$p_log <- log(herb_kbs$p_eaten+1)
hist(herb_kbs$p_log)
qqnorm(herb_kbs$p_log)
shapiro.test(herb_kbs$p_log) # NAs - data contains 0s
# mean centering p_eaten
herb_kbs$p_scaled <- herb_kbs$p_log - mean(herb_kbs$p_log)
hist(herb_kbs$p_scaled)
hist(herb_kbs$p_scaled[herb_kbs$state == "ambient"])
hist(herb_kbs$p_scaled[herb_kbs$state == "warmed"])
qqnorm(herb_kbs$p_scaled)
shapiro.test(herb_kbs$p_scaled)
# square root?
herb_kbs$p_sqrt <- sqrt(herb_kbs$p_eaten)
hist(herb_kbs$p_sqrt)


# transformations are a no-go
# mean and var of non-zero counts
herb_kbs %>%
  dplyr::filter(p_eaten != "0") %>%
  dplyr::summarize(mean_eaten = mean(p_eaten, na.rm=T), var_eaten = var(p_eaten, na.rm=T))
# variance is also > mean, so can't be poisson
# I'll try zero-inflated negative binomial due to an excess of zeros

# zero-inflated negative binomial
# is state the variable that predicts the excess zeros?
# this is probably the right one since the 0's in the data are real counts
m.neg1 <- zeroinfl(p_eaten ~ state | state,
               dist = 'negbin',
               data = herb_kbs)
summary(m.neg1)

# is species the variable that predicts excess zeros?
m.neg2 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_kbs)
summary(m.neg2)

# check dispersion
E <- resid(m.neg1, type = "pearson")
N  <- nrow(herb_kbs)
p  <- length(coef(m.neg1)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # pretty close to one, not bad

E2 <- resid(m.neg2, type = "pearson")
N2 <- nrow(herb_kbs)
p2  <- length(coef(m.neg2)) + 1 
sum(E2^2) / (N2 - p2) # not as good as the first one

# comparing the two models with a likelihood ratio test
lrtest(m.neg1, m.neg2) #m.neg2 is better, but m.neg1 makes more sense for the data



####### umbs #########
### determining distribution ###
# first, checking for normality
hist(herb_umbs$p_eaten)
qqnorm(herb_umbs$p_eaten)
shapiro.test(herb_umbs$p_eaten)
fit <- lm(p_eaten~state, data = herb_umbs)
qqPlot(fit)
hist(herb_umbs$p_eaten[herb_umbs$state == "ambient"])
hist(herb_umbs$p_eaten[herb_umbs$state == "warmed"])
# not normal- attempting to transform data below

# log transform 
herb_umbs$p_log <- log(herb_umbs$p_eaten)
hist(herb_umbs$p_log)
qqnorm(herb_umbs$p_log)
shapiro.test(herb_umbs$p_log) # NAs - data contains 0s

# transformations are a no-go
# mean and var of non-zero counts
herb_umbs %>%
        dplyr::filter(p_eaten != "0") %>%
        dplyr::summarize(mean_eaten = mean(p_eaten, na.rm=T), var_eaten = var(p_eaten, na.rm=T))
# variance is also > mean, so can't be poisson
# I'll try zero-inflated negative binomial due to an excess of zeros

# zero-inflated negative binomial
# is state the variable that predicts the excess zeros?
# this is probably the right one since the 0's in the data are real counts
m.neg3 <- zeroinfl(p_eaten ~ state | state,
                   dist = 'negbin',
                   data = herb_umbs)
summary(m.neg3)

# is species the variable that predicts excess zeros?
m.neg4 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_umbs)
summary(m.neg4)

# check dispersion
E3 <- resid(m.neg3, type = "pearson")
N3  <- nrow(herb_umbs)
p3  <- length(coef(m.neg3)) + 1 # '+1' is due to theta
sum(E3^2) / (N3 - p3) # a little low

E4 <- resid(m.neg4, type = "pearson")
N4 <- nrow(herb_umbs)
p4  <- length(coef(m.neg4)) + 1 
sum(E4^2) / (N4 - p4) # pretty close to one

# comparing the two models with a likelihood ratio test
lrtest(m.neg3, m.neg4) #m.neg4 is better, but m.neg3 makes more sense for the data





###### running analyses ########
## partially taken from kileigh's old code ##
moda <- lmer(p_eaten ~ state*year + (1|species) + (1|plot), herb_kbs)
modb <- lmer(p_eaten ~ state + year + (1|species) + (1|plot), herb_kbs)
modc <- lmer(p_eaten ~ state + (1|year) + (1|species) + (1|plot), herb_kbs)
anova(moda,modb,modc)
summary(moda)
anova(moda)
emmeans(moda, list(pairwise ~ state*year), adjust = "tukey") # only shows 2017?

# these fail
permanova.lmer(moda)
permanova.lmer(moda, drop=FALSE)

