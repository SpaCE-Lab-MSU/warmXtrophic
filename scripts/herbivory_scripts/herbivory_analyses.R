# TITLE:          Herbivory data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021


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
library(lme4)
library(olsrr)
library(predictmeans)
library(car)
library(fitdistrplus)
library(MASS)
library(pscl)

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

# create dataframes for kbs and umbs only for plots with no insecticide
herb_kbs <- subset(herb, site == "kbs" & insecticide == "insects")
herb_umbs <- subset(herb, site == "umbs" & insecticide == "insects")



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

# mean centering p_eaten
herb_kbs$p_scaled <- scale(herb_kbs$p_eaten, scale = F)
hist(herb_kbs$p_scaled)
hist(herb_kbs$p_scaled[herb_kbs$state == "ambient"])
hist(herb_kbs$p_scaled[herb_kbs$state == "warmed"])
qqnorm(herb_kbs$p_scaled)
shapiro.test(herb_kbs$p_scaled)

# log transform 
herb_kbs$p_log <- log(herb_kbs$p_eaten)
hist(herb_kbs$p_log)
qqnorm(herb_kbs$p_log)
shapiro.test(herb_kbs$p_log) # NAs - data contains 0s

# inverse transform 
herb_kbs$p_inv <- 1/(herb_kbs$p_eaten)
hist(herb_kbs$p_inv)
qqnorm(herb_kbs$p_inv)
shapiro.test(herb_kbs$p_inv) # NA- data contains 0s

# square root transform 
herb_kbs$p_sqrt <- sqrt(herb_kbs$p_eaten)
hist(herb_kbs$p_sqrt)
qqnorm(herb_kbs$p_sqrt)
shapiro.test(herb_kbs$p_sqrt)

# cubed root transform 
herb_kbs$p_cubed <- (herb_kbs$p_eaten)^(1/3)
hist(herb_kbs$p_cubed)
qqnorm(herb_kbs$p_cubed)
shapiro.test(herb_kbs$p_cubed)


##### trying different distributions #######
descdist(herb_kbs$p_eaten, discrete = FALSE)
# doesn't really look close to any
# variance is also > mean, so can't be poisson
# I'll try negative binomial due to an excess of zeros

# mean and var of non-zero counts
herb_kbs %>%
  dplyr::filter(p_eaten != "0") %>%
  dplyr::summarize(mean_eaten = mean(p_eaten, na.rm=T), var_eaten = var(p_eaten, na.rm=T))

# using glm
fit <- lm(p_eaten~state, data = herb_kbs)
residual <- fit$residuals
hist(residual)
neg.binom <- glm.nb(p_eaten~state, data = herb_kbs)
hist(neg.binom$residuals)

# need to work on zero-inflated model more
# zero-inflatex negative binomial
mod1 <- zeroinfl(p_eaten ~ state, data = herb_kbs, dist = "negbin")
mod1


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

# inverse transform 
herb_umbs$p_inv <- 1/(herb_umbs$p_eaten)
hist(herb_umbs$p_inv)
qqnorm(herb_umbs$p_inv)
shapiro.test(herb_umbs$p_inv) # NA- data contains 0s

# square root transform 
herb_umbs$p_sqrt <- sqrt(herb_umbs$p_eaten)
hist(herb_umbs$p_sqrt)
qqnorm(herb_umbs$p_sqrt)
shapiro.test(herb_umbs$p_sqrt)

# cubed root transform 
herb_umbs$p_cubed <- (herb_umbs$p_eaten)^(1/3)
hist(herb_umbs$p_cubed)
qqnorm(herb_umbs$p_cubed)
shapiro.test(herb_umbs$p_cubed)






##### my attempt before finding Kileigh's script #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(p_eaten~state*year, data = herb_kbs)
summary(lm1)

# mixed effects model (species random)
lme1 <- lme(p_eaten~state*year, random=~1|species, data = herb_kbs)
summary(lme1)
coef(lme1)

