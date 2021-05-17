# TITLE:          Herbivory data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021 ; updated May 2021


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

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)
herb <- read.csv(file.path(L1_dir, "herbivory/final_herbivory_L1.csv"))

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

# number of observation per origin/state combo
herb_kbs %>% count(state, origin)
herb_umbs %>% count(state, origin)

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


### zero-inflated negative binomial ###

## models with state and year ##
# state as a fixed effect
kbs.m1 <- zeroinfl(p_eaten ~ state,
               dist = 'negbin',
               data = herb_kbs)

# year + state as separate fixed effects
kbs.m2 <- zeroinfl(p_eaten ~ state + as.factor(year),
               dist = 'negbin',
               data = herb_kbs)

# interaction between state and year
kbs.m3 <- zeroinfl(p_eaten ~ state * as.factor(year),
               dist = 'negbin',
               data = herb_kbs)


## models with state, species and year ##
# interaction between state and species
kbs.m4 <- zeroinfl(p_eaten ~ state * species,
                   dist = 'negbin',
                   data = herb_kbs)

# state and species as separate fixed effects
kbs.m5 <- zeroinfl(p_eaten ~ state + species,
                     dist = 'negbin',
                     data = herb_kbs)

# state, species and year as fixed effects
kbs.m6 <- zeroinfl(p_eaten ~ state + as.factor(year) + species,
               dist = 'negbin',
               data = herb_kbs)

# interaction between state and year, + species
kbs.m7 <- zeroinfl(p_eaten ~ state * as.factor(year) + species,
               dist = 'negbin',
               data = herb_kbs) # error

# interaction between state and species, + year
kbs.m8 <- zeroinfl(p_eaten ~ state * species + as.factor(year),
                   dist = 'negbin',
                   data = herb_kbs)

# interaction between all 3
kbs.m9 <- zeroinfl(p_eaten ~ state * as.factor(year) * species,
               dist = 'negbin',
               data = herb_kbs) # error


### models with state, origin and year ###
# state and origin as fixed effects
kbs.m10 <- zeroinfl(p_eaten ~ state + origin,
               dist = 'negbin',
               data = herb_kbs)

# native vs exotic * state
kbs.m11 <- zeroinfl(p_eaten ~ state * origin,
               dist = 'negbin',
               data = herb_kbs)

# native vs exotic * state
kbs.m12 <- zeroinfl(p_eaten ~ state * origin + as.factor(year),
                dist = 'negbin',
                data = herb_kbs)


### trying a model with a different predictor variable for 0's ###
# is species the variable that predicts excess zeros?
kbs.m13 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_kbs)


# likelihood ratio test
lrtest(kbs.m1, kbs.m2, kbs.m3, kbs.m4, kbs.m5, kbs.m6, kbs.m7, kbs.m8, kbs.m10, kbs.m11, kbs.m12, kbs.m13)

# check dispersion
E <- resid(kbs.m7, type = "pearson")
N  <- nrow(herb_kbs)
p  <- length(coef(kbs.m7)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # pretty close to one

# pairwise comparisons
emmeans(kbs.m7, ~ state + year + species)




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
shapiro.test(herb_umbs$p_log)


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
umbs.m1 <- zeroinfl(p_eaten ~ state | state,
                   dist = 'negbin',
                   data = herb_umbs)

# interaction between state and species
umbs.m2 <- zeroinfl(p_eaten ~ state * species,
               dist = 'negbin',
               data = herb_umbs)

# state and species as separate fixed effects
umbs.m3 <- zeroinfl(p_eaten ~ state + species,
               dist = 'negbin',
               data = herb_umbs)

# state, species and year as fixed effects
umbs.m4 <- zeroinfl(p_eaten ~ state + as.factor(year) + species,
               dist = 'negbin',
               data = herb_umbs)

# interaction between state and year, + species
umbs.m5 <- zeroinfl(p_eaten ~ state * as.factor(year) + species,
               dist = 'negbin',
               data = herb_umbs)

# interaction between all 3
umbs.m6 <- zeroinfl(p_eaten ~ state * as.factor(year) * species,
               dist = 'negbin',
               data = herb_umbs)

# is species the variable that predicts excess zeros?
umbs.m7 <- zeroinfl(p_eaten ~ state | species,
                   dist = 'negbin',
                   data = herb_umbs)

# likelihood ratio test
lrtest(umbs.m1, umbs.m2, umbs.m3, umbs.m4, umbs.m5, umbs.m7, umbs.m8)

# check dispersion
E2 <- resid(umbs.m7, type = "pearson")
N2  <- nrow(herb_umbs)
p2  <- length(coef(umbs.m7)) + 1 # '+1' is due to theta
sum(E2^2) / (N2 - p2) # pretty close to one

#pairwise comparisons
emmeans(umbs.m7, ~ state + year + species)




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

