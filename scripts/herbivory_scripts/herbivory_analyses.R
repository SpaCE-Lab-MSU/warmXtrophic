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

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data & metadata
herb <- read.csv("L1/herbivory/final_herbivory_L1.csv")

# changing scale of years
herb$year[herb$year == 2015] <- 1
herb$year[herb$year == 2016] <- 2
herb$year[herb$year == 2017] <- 3
herb$year[herb$year == 2018] <- 4
herb$year[herb$year == 2019] <- 5
herb$year[herb$year == 2020] <- 6

# create dataframes for kbs and umbs only for plots with no insecticide
final_kbs <- subset(herb, site == "kbs" & insecticide == "insects")
final_umbs <- subset(herb, site == "umbs" & insecticide == "insects")



###### statistical analysis #########
# first, checking that residuals are normal
# kbs
hist(final_kbs$p_eaten)
qqnorm(final_kbs$p_eaten)
shapiro.test(final_kbs$p_eaten)
fit <- lm(p_eaten~state, data = final_kbs)
qqPlot(fit)
hist(final_kbs$p_eaten[final_kbs$state == "ambient"])
hist(final_kbs$p_eaten[final_kbs$state == "warmed"])
# not normal, attempting to transform data below

# mean centering p_eaten
final_kbs$p_scaled <- scale(final_kbs$p_eaten, scale = F)
hist(final_kbs$p_scaled)
hist(final_kbs$p_scaled[final_kbs$state == "ambient"])
hist(final_kbs$p_scaled[final_kbs$state == "warmed"])
qqnorm(final_kbs$p_scaled)
shapiro.test(final_kbs$p_scaled)

# log transform 
final_kbs$p_log <- log(final_kbs$p_eaten)
hist(final_kbs$p_log)
qqnorm(final_kbs$p_log)
shapiro.test(final_kbs$p_log) # NAs - data contains 0s

# inverse transform 
final_kbs$p_inv <- 1/(final_kbs$p_eaten)
hist(final_kbs$p_inv)
qqnorm(final_kbs$p_inv)
shapiro.test(final_kbs$p_inv) # NA- data contains 0s

# square root transform 
final_kbs$p_sqrt <- sqrt(final_kbs$p_eaten)
hist(final_kbs$p_sqrt)
qqnorm(final_kbs$p_sqrt)
shapiro.test(final_kbs$p_sqrt)

# cubed root transform 
final_kbs$p_cubed <- (final_kbs$p_eaten)^(1/3)
hist(final_kbs$p_cubed)
qqnorm(final_kbs$p_cubed)
shapiro.test(final_kbs$p_cubed)

# previous attempts at normality on residuals
#fit <- lm(p_eaten~state*year, data = final_kbs)
#residual <- fit$residuals
#shapiro.test(residual)
#ols_plot_resid_qq(fit)
#hist(residual)


# umbs
hist(final_umbs$p_eaten)
qqnorm(final_umbs$p_eaten)
shapiro.test(final_umbs$p_eaten)
fit <- lm(p_eaten~state, data = final_umbs)
qqPlot(fit)
hist(final_umbs$p_eaten[final_umbs$state == "ambient"])
hist(final_umbs$p_eaten[final_umbs$state == "warmed"])
# not normal- attempting to transform data below

# log transform 
final_umbs$p_log <- log(final_umbs$p_eaten)
hist(final_umbs$p_log)
qqnorm(final_umbs$p_log)
shapiro.test(final_umbs$p_log) # NAs - data contains 0s

# inverse transform 
final_umbs$p_inv <- 1/(final_umbs$p_eaten)
hist(final_umbs$p_inv)
qqnorm(final_umbs$p_inv)
shapiro.test(final_umbs$p_inv) # NA- data contains 0s

# square root transform 
final_umbs$p_sqrt <- sqrt(final_umbs$p_eaten)
hist(final_umbs$p_sqrt)
qqnorm(final_umbs$p_sqrt)
shapiro.test(final_umbs$p_sqrt)

# cubed root transform 
final_umbs$p_cubed <- (final_umbs$p_eaten)^(1/3)
hist(final_umbs$p_cubed)
qqnorm(final_umbs$p_cubed)
shapiro.test(final_umbs$p_cubed)

# previous attempts
#fit2 <- lm(p_eaten~state*year, data = final_umbs)
#residual2 <- fit2$residuals
#shapiro.test(residual2)
#ols_plot_resid_qq(fit2)
#hist(residual2)



## partially taken from kileigh's old code ##
moda <- lmer(p_eaten ~ state*year + (1|species) + (1|plot), final_kbs)
modb <- lmer(p_eaten ~ state + year + (1|species) + (1|plot), final_kbs)
modc <- lmer(p_eaten ~ state + (1|year) + (1|species) + (1|plot), final_kbs)
anova(moda,modb,modc)
summary(moda)
anova(moda)
emmeans(moda, list(pairwise ~ state*year), adjust = "tukey") # only shows 2017?

# these fail
permanova.lmer(moda)
permanova.lmer(moda, drop=FALSE)



##### my attempt before finding Kileigh's script #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(p_eaten~state*year, data = final_kbs)
summary(lm1)

# mixed effects model (species random)
lme1 <- lme(p_eaten~state*year, random=~1|species, data = final_kbs)
summary(lme1)
coef(lme1)

