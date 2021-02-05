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
library(nlme)
library(olsrr)
library(predictmeans)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data & metadata
herb <- read.csv("L1/herbivory/final_herbivory_L1.csv")

# create dataframes for kbs and umbs only for plots with no insecticide
final_kbs <- subset(herb, site == "kbs" & insecticide == "insects")
final_umbs <- subset(herb, site == "umbs" & insecticide == "insects")



###### statistical analysis #########
# first, checking that residuals are normal
fit <- lm(p_eaten~state*year, data = final_kbs)
residual <- fit$residuals
shapiro.test(residual)
ols_plot_resid_qq(fit)
hist(residual)
# not normal - square root and cubed root don't fix it, and can't take the log or inverse due to 0's in data

fit2 <- lm(p_eaten~state*year, data = final_umbs)
residual2 <- fit2$residuals
shapiro.test(residual2)
ols_plot_resid_qq(fit2)
hist(residual2)
# also not normal



## partially taken from kileigh's old code ##
moda <- lmer(p_eaten ~ state*year + (1|species) + (1|plot), final_kbs)
modb <- lmer(p_eaten ~ state*year + (1|species), final_kbs)
anova(moda,modb) # moda better - lower AIC and BIC
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

