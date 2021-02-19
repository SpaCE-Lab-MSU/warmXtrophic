# TITLE:          Greenup data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021


##### Main questions ######
# Is date of greenup different between ambient and warmed treatments? Hypothesis: greenup is earlier for warmed treatments
# state  = fixed, date = response
  # include year as a treatment (is this difference seen each year)? insecticide? 
  # species + plot as random effects?
# Is date of greenup different between warmed/ambient for native vs exotic? same for growth habit


# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(lme4)
library(olsrr)
library(predictmeans)
library(car)
library(fitdistrplus)
library(ggpubr)
library(rstatix)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data
greenup <- read.csv("L1/greenup/final_greenup_L1.csv")
str(greenup)

# changing scale of years
greenup$year[greenup$year == 2015] <- 1
greenup$year[greenup$year == 2016] <- 2
greenup$year[greenup$year == 2017] <- 3
greenup$year[greenup$year == 2018] <- 4
greenup$year[greenup$year == 2019] <- 5
greenup$year[greenup$year == 2020] <- 6

# create dataframes for kbs and umbs
final_kbs <- subset(greenup, site == "kbs")
final_umbs <- subset(greenup, site == "umbs")



######### kbs ############
#### determining distribution ####
# first, checking for normality
hist(final_kbs$half_cover_date)
qqnorm(final_kbs$half_cover_date)
shapiro.test(final_kbs$half_cover_date)
fit <- lm(half_cover_date~state, data = final_kbs)
qqPlot(fit)
hist(final_kbs$half_cover_date[final_kbs$state == "ambient"])
hist(final_kbs$half_cover_date[final_kbs$state == "warmed"])
# data isn't normal - below I try to transform the data

# mean centering half_cover_date
final_kbs$date_scaled <- scale(final_kbs$half_cover_date, scale = F)
hist(final_kbs$date_scaled)
hist(final_kbs$date_scaled[final_kbs$state == "ambient"])
hist(final_kbs$date_scaled[final_kbs$state == "warmed"])
qqnorm(final_kbs$date_scaled)
shapiro.test(final_kbs$date_scaled)
# still not normal

# log transform 
final_kbs$date_log <- log(final_kbs$half_cover_date)
hist(final_kbs$date_log)
qqnorm(final_kbs$date_log)
shapiro.test(final_kbs$date_log)

# inverse transform 
final_kbs$date_inv <- 1/(final_kbs$half_cover_date)
hist(final_kbs$date_inv)
qqnorm(final_kbs$date_inv)
shapiro.test(final_kbs$date_inv)

# square root transform 
final_kbs$date_sqrt <- sqrt(final_kbs$half_cover_date)
hist(final_kbs$date_sqrt)
qqnorm(final_kbs$date_sqrt)
shapiro.test(final_kbs$date_sqrt)

# cubed root transform 
final_kbs$date_cubed <- (final_kbs$half_cover_date)^(1/3)
hist(final_kbs$date_cubed)
qqnorm(final_kbs$date_cubed)
shapiro.test(final_kbs$date_cubed)


##### trying different distributions #######
# found this method through stackoverflow
descdist(final_kbs$half_cover_date, discrete = FALSE) # looks closest to uniform
fit.unif <- fitdist(final_kbs$half_cover_date, "unif")
plot(fit.unif)
# uniform is closest, but isn't included in glmer so I'll go with poisson (which still makes sense)
fit <- lm(half_cover_date~state, data = final_kbs)
residual <- fit$residuals
hist(residual)
pois <- glm(half_cover_date~state, data = final_kbs, family="poisson")
hist(pois$residuals)
# still doesn't look very good
# including parametric & non-parametric models below


###### running analyses ########
## partially taken from kileighs old models - parametric tests ##
# generalized linear models for poisson distribution with:
# state, year and insecticide as fixed (w interaction btwn state and year) & species and plot as random effects
moda <- glmer(half_cover_date ~ state*year + insecticide + (1|species) + (1|plot),
              data=final_kbs, family = poisson)
# state, year and insecticide as separate fixed effects & species and plot as random effects
modb <- glmer(half_cover_date ~ state + year + insecticide + (1|species) + (1|plot),
              data=final_kbs, family = poisson)
# state and insecticide as fixed effects & year, species and plot as random effects
modc <- glmer(half_cover_date ~ state + insecticide + (1|year) + (1|species) + (1|plot),
              data=final_kbs, family = poisson)
anova(moda, modb, modc)
summary(moda)
anova(moda)

## non-parametric ##
friedman_kbs <- final_kbs %>% 
  friedman_test(half_cover_date ~ state | plot)


# from kileigh's code
confint(modb, method="boot", nsim=999)
difflsmeans(modb, test.effs=NULL, ddf="Satterthwaite")





########### umbs ##############
#### determining distribution ####
# first, checking normality
hist(final_umbs$half_cover_date)
qqnorm(final_umbs$half_cover_date)
shapiro.test(final_umbs$half_cover_date)
fit2 <- lm(half_cover_date~state, data = final_umbs)
qqPlot(fit2)
hist(final_umbs$half_cover_date[final_kbs$state == "ambient"])
hist(final_umbs$half_cover_date[final_kbs$state == "warmed"])
# when separated by state the histograms are just a bit right skewed

# mean centering half_cover_date
final_umbs$date_scaled <- scale(final_umbs$half_cover_date, scale = F)
hist(final_umbs$date_scaled)
hist(final_umbs$date_scaled[final_kbs$state == "ambient"])
hist(final_umbs$date_scaled[final_kbs$state == "warmed"])
qqnorm(final_umbs$date_scaled)
shapiro.test(final_umbs$date_scaled)
# still not normal

# log transform 
final_umbs$date_log <- log(final_umbs$half_cover_date)
hist(final_umbs$date_log)
qqnorm(final_umbs$date_log)
shapiro.test(final_umbs$date_log)
# this looks pretty good but shapiro wilk test still is below 0.05 (how important is this?)

# inverse transform 
final_umbs$date_inv <- 1/(final_umbs$half_cover_date)
hist(final_umbs$date_inv)
qqnorm(final_umbs$date_inv)
shapiro.test(final_umbs$date_inv)
# also looks better, still below 0.05 for shapiro wilk

# square root transform 
final_umbs$date_sqrt <- sqrt(final_umbs$half_cover_date)
hist(final_umbs$date_sqrt)
qqnorm(final_umbs$date_sqrt)
shapiro.test(final_umbs$date_sqrt)

# cubed root transform 
final_umbs$date_cubed <- (final_umbs$half_cover_date)^(1/3)
hist(final_umbs$date_cubed)
qqnorm(final_umbs$date_cubed)
shapiro.test(final_umbs$date_cubed)


##### trying different distributions ######
descdist(final_umbs$half_cover_date, discrete = FALSE) # maybe gamma?
fit.gamma <- fitdist(final_kbs$half_cover_date, "gamma")
plot(fit.gamma)

# using glm
residual2 <- fit2$residuals
hist(residual2)
gamma <- glm(half_cover_date~state, data = final_umbs, family = "Gamma")
hist(gamma$residuals)


###### running analyses ########
## partially taken from kileighs old models ##
modd <- glmer(half_cover_date ~ state*year + insecticide + (1|species) + (1|plot),
              data=final_umbs, family = poisson)
mode <- glmer(half_cover_date ~ state + year + insecticide + (1|species) + (1|plot),
              data=final_umbs, family = poisson)
modf <- glmer(half_cover_date ~ state + insecticide + (1|year) + (1|species) + (1|plot),
              data=final_umbs, family = poisson)
anova(modd, mode, modf)
summary(modd)
anova(modd)
#emmeans(modb, specs = pairwise ~ state, type = "response", adjust = "tukey") # only shows 2017

# from kileigh's code
confint(modd, method="boot", nsim=999)
difflsmeans(modd, test.effs=NULL, ddf="Satterthwaite")

# these both fail - cluster setup failed
#permanova.lmer(modd)
#permanova.lmer(modd, drop=FALSE)



##### my attempt before finding Kileigh's script #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(half_cover_date~state*year+insecticide, data = final_kbs)
summary(lm1)

lm2 <- lm(half_cover_date~state*year+insecticide, data = final_umbs)
summary(lm2)

# mixed effects model
lme2 <- lme(half_cover_date~state*year+insecticide, random=~1|species, data = final_kbs)
summary(lme2)
coef(lme2)

lme3 <- lme(half_cover_date~state*year+insecticide, random=~1|species, data = final_umbs)
summary(lme3)
coef(lme3)

