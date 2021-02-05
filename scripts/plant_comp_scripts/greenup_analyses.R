# TITLE:          Greenup data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021


##### Main questions ######
# Is date of greenup different between ambient and warmed treatments? Hypothesis: greenup is earlier for warmed treatments
  # include year as a treatment (is this difference seen each year)? insecticide? 
  # species + plot as random effects?
# Is date of greenup different between warmed/ambient for native vs exotic? same for growth habit


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

# Read in plant comp data
greenup <- read.csv("L1/greenup/final_greenup_L1.csv")
str(greenup)

# create dataframes for kbs and umbs
final_kbs <- subset(greenup, site == "kbs")
final_umbs <- subset(greenup, site == "umbs")



###### statistical analysis #########
# first, checking that residuals are normal
fit <- lm(half_cover_date~state*year + insecticide, data = final_kbs)
residual <- fit$residuals
shapiro.test(residual)
ols_plot_resid_qq(fit)
hist(residual)
# not normal - square root, cubed root, log and inverse don't seem to work

fit2 <- lm(half_cover_date~state*year + insecticide, data = final_umbs)
residual2 <- fit2$residuals
shapiro.test(residual2)
ols_plot_resid_hist(fit2)
ols_plot_resid_qq(fit2)
hist(residual2)
# also not normal


## partially taken from kileighs old models ##
# do we need plot as a random effect?
moda <- lmer(half_cover_date ~ state*year + insecticide + (1|species) + (1|plot), final_kbs)
modb <- lmer(half_cover_date ~ state*year + insecticide + (1|species), final_kbs)
anova(modb, moda) #no? because lower BIC?
summary(modb)
anova(modb)
emmeans(modb, specs = pairwise ~ state + year, type = "response", adjust = "tukey") # only shows 2017

# from kileigh's code
confint(modb, method="boot", nsim=999)
difflsmeans(modb, test.effs=NULL, ddf="Satterthwaite")

# these both fail - cluster setup failed
permanova.lmer(modb)
permanova.lmer(modb, drop=FALSE)



##### my attempt before finding Kileigh's script #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(half_cover_date~state*year+insecticide, data = final_kbs)
summary(lm1)

lm2 <- lm(half_cover_date~state*year+insecticide, data = final_umbs)
summary(lm2)

# mixed effects model -> after running lme with plot as random effect, may not be needed (plot doesn't affect intercepts in coef)
# below does not give p values from lme4, so I switched to nlme instead
lme2 <- lme(half_cover_date~state*year+insecticide, random=~1|species, data = final_kbs)
summary(lme2)
coef(lme2)

lme3 <- lme(half_cover_date~state*year+insecticide, random=~1|species, data = final_umbs)
summary(lme3)
coef(lme3)

