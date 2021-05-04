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
library(emmeans)

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
herb_kbs <- herb_kbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs <- herb_umbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))

# checking to see if any species/state combos are all zeros
with(herb_kbs,table(species,state,p_eaten==0)) 
with(herb_umbs,table(species,state,p_eaten==0)) # looks good now, species were removed in herbivory_clean

# number of observation per species/state combo (to find rare species)
herb_kbs %>% count(state, species)
herb_umbs %>% count(state, species)

# removing rare species from KBS
herb_kbs <- herb_kbs[!grepl("Hype",herb_kbs$species),]
herb_kbs %>% count(state, species)

# How much of the data is zeros?
100*sum(herb_kbs$p_eaten == 0)/nrow(herb_kbs) #68% - thats a lot! probably have to use a zero-inflated model,
# but I'll still check for normality & try some transformations below
100*sum(herb_umbs$p_eaten == 0)/nrow(herb_umbs) #61%


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
# state as a fixed effect
m1 <- zeroinfl(p_eaten ~ state,
               dist = 'negbin',
               data = herb_kbs)
summary(m1)

# interaction between state and species
m2 <- zeroinfl(p_eaten ~ state * species,
                   dist = 'negbin',
                   data = herb_kbs)
summary(m2)

# state and species as separate fixed effects
m3 <- zeroinfl(p_eaten ~ state + species,
                     dist = 'negbin',
                     data = herb_kbs)
summary(m3)

# state, species and year as fixed effects
m4 <- zeroinfl(p_eaten ~ state + as.factor(year) + species,
               dist = 'negbin',
               data = herb_kbs)
summary(m4)

# interaction between state and year, + species
m5 <- zeroinfl(p_eaten ~ state * as.factor(year) + species,
               dist = 'negbin',
               data = herb_kbs)
summary(m5) # all NAs

# interaction between all 3
m6 <- zeroinfl(p_eaten ~ state * as.factor(year) * species,
               dist = 'negbin',
               data = herb_kbs)
summary(m6) # doesn't run

# is species the variable that predicts excess zeros?
m7 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_kbs)
summary(m7)

# likelihood ratio test
lrtest(m1, m2, m3, m4, m7) # model four

# check dispersion
E <- resid(m4, type = "pearson")
N  <- nrow(herb_kbs)
p  <- length(coef(m4)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # pretty close to one

# pairwise comparisons
emmeans(m4, ~ state + year + species)




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
m8 <- zeroinfl(p_eaten ~ state | state,
                   dist = 'negbin',
                   data = herb_umbs)
summary(m8)

# interaction between state and species
m9 <- zeroinfl(p_eaten ~ state * species,
               dist = 'negbin',
               data = herb_umbs)
summary(m9) # NaNs produced due to complete separation

# state and species as separate fixed effects
m10 <- zeroinfl(p_eaten ~ state + species,
               dist = 'negbin',
               data = herb_umbs)
summary(m10) # NaNs produced due to complete separation

# state, species and year as fixed effects
m11 <- zeroinfl(p_eaten ~ state + as.factor(year) + species,
               dist = 'negbin',
               data = herb_umbs)
summary(m11)

# interaction between state and year, + species
m12 <- zeroinfl(p_eaten ~ state * as.factor(year) + species,
               dist = 'negbin',
               data = herb_umbs)
summary(m12)

# interaction between all 3
m13 <- zeroinfl(p_eaten ~ state * as.factor(year) * species,
               dist = 'negbin',
               data = herb_umbs)
summary(m13) # doesn't run

# is species the variable that predicts excess zeros?
m14 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_umbs)
summary(m14)

# likelihood ratio test
lrtest(m8, m9, m10, m11, m12, m14) # model 11 or 12

# check dispersion
E2 <- resid(m11, type = "pearson")
N2  <- nrow(herb_umbs)
p2  <- length(coef(m11)) + 1 # '+1' is due to theta
sum(E2^2) / (N2 - p2) # pretty close to one

E3 <- resid(m12, type = "pearson")
p3  <- length(coef(m12)) + 1 
sum(E3^2) / (N2 - p3) # pretty close to one

#pairwise comparisons
emmeans(m11, ~ state + year + species)




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

