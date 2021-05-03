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
herb <- herb[complete.cases(herb),]

# create dataframes for kbs and umbs only for plots with no insecticide
herb_kbs <- subset(herb, site == "kbs" & insecticide == "insects")
herb_umbs <- subset(herb, site == "umbs" & insecticide == "insects")

# only keep species that were recorded in both warmed and ambient plots
# models below won't run if species are only recorded in either warmed or ambient plots
herb_kbs <- herb_kbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs <- herb_umbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))

# How much of the data is zeros?
100*sum(herb_kbs$p_eaten == 0)/nrow(herb_kbs) #68% - thats a lot! probably have to use a zero-inflated model,
# but I'll still check for normality & try some transformations below
100*sum(herb_umbs$p_eaten == 0)/nrow(herb_umbs) #60%

# checking to see if any species/state combos are all zeros
with(herb_kbs,table(species,state,p_eaten==0)) 
with(herb_umbs,table(species,state,p_eaten==0)) # yes for both


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
m1 <- zeroinfl(p_eaten ~ state,
               dist = 'negbin',
               data = herb_kbs)
summary(m1)

# interaction between state and species
m2 <- zeroinfl(p_eaten ~ state * species,
                   dist = 'negbin',
                   data = herb_kbs)
summary(m2) # NaNs produced due to complete separation

# state and species as separate fixed effects
m3 <- zeroinfl(p_eaten ~ state + species,
                     dist = 'negbin',
                     data = herb_kbs)
summary(m3) # NaNs produced due to complete separation

# is species the variable that predicts excess zeros?
m4 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_kbs)
summary(m4)

# likelihood ratio test
lrtest(m1, m2, m3, m4) # m2 has the highest loglik, but m3 may be better

# check dispersion
E <- resid(m2, type = "pearson")
N  <- nrow(herb_kbs)
p  <- length(coef(m2)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # pretty close to one, not bad

E2 <- resid(m3, type = "pearson")
N2 <- nrow(herb_kbs)
p2  <- length(coef(m3)) + 1 
sum(E2^2) / (N2 - p2) # almost the same as the prior model, a little worse




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
m5 <- zeroinfl(p_eaten ~ state | state,
                   dist = 'negbin',
                   data = herb_umbs)
summary(m5)

# interaction between state and species
m6 <- zeroinfl(p_eaten ~ state * species,
               dist = 'negbin',
               data = herb_umbs)
summary(m6) # NaNs produced due to complete separation

# state and species as separate fixed effects
m7 <- zeroinfl(p_eaten ~ state + species,
               dist = 'negbin',
               data = herb_umbs)
summary(m7) # NaNs produced due to complete separation

# is species the variable that predicts excess zeros?
m8 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_umbs)
summary(m8)

# likelihood ratio test
lrtest(m5, m6, m7, m8) # m6 has the highest loglik, but m7 may be better

# check dispersion
E3 <- resid(m6, type = "pearson")
N3  <- nrow(herb_umbs)
p3  <- length(coef(m6)) + 1 # '+1' is due to theta
sum(E3^2) / (N3 - p3) # pretty close to one

E4 <- resid(m7, type = "pearson")
N4 <- nrow(herb_umbs)
p4  <- length(coef(m7)) + 1 
sum(E4^2) / (N4 - p4) # closer than the prior model





# old code from kileighs analyses
###### running analyses ########
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

