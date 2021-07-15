# TITLE:          Carbon and Nitrogen Data Analysis
# AUTHORS:        Kara Dobson, Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN data is uploaded to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           July, 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(bbmle)
library(lme4)
library(fitdistrplus)

# Set working directory
L1_dir<-Sys.getenv("L1DIR")

# Read in data
cn <- read.csv(file.path(L1_dir, "CN/CN_L1.csv"))

# check species in dataframe
with(cn,table(cn$site,cn$species)) # keep sites in same dataframe for now bc unique spp at each site

# removing NAs - two rows have NAs for both N and C, so this takes care of both
cn <- cn[!is.na(cn$carbon), ]


# carbon data visualization
descdist(cn$carbon, discrete = FALSE)
hist(cn$carbon)
qqnorm(cn$carbon)
shapiro.test(cn$carbon)

# logistic distribution?
c.fit.logis <- fitdist(cn$carbon, "logis")
plot(c.fit.logis)

# normal distribution?
c.fit.norm <- fitdist(cn$carbon, "norm")
plot(c.fit.norm)

par(mfrow=c(2,2))
plot.legend <- c("Logistic", "Normal")
denscomp(list(c.fit.logis, c.fit.norm), legendtext = plot.legend)
cdfcomp (list(c.fit.logis, c.fit.norm), legendtext = plot.legend)
qqcomp  (list(c.fit.logis, c.fit.norm), legendtext = plot.legend)
ppcomp  (list(c.fit.logis, c.fit.norm), legendtext = plot.legend)
gofstat(list(c.fit.logis, c.fit.norm), fitnames = c("Logistic", "Normal")) #Normal looks best


# nitrogen data visualization
descdist(cn$nitrogen, discrete = FALSE)
hist(cn$nitrogen)
qqnorm(cn$nitrogen)
shapiro.test(cn$nitrogen)

# logistic distribution?
n.fit.logis <- fitdist(cn$nitrogen, "logis")
plot(n.fit.logis)

# normal distribution?
n.fit.norm <- fitdist(cn$nitrogen, "norm")
plot(n.fit.norm)

par(mfrow=c(2,2))
plot.legend <- c("Logistic", "Normal")
denscomp(list(n.fit.logis, n.fit.norm), legendtext = plot.legend)
cdfcomp (list(n.fit.logis, n.fit.norm), legendtext = plot.legend)
qqcomp  (list(n.fit.logis, n.fit.norm), legendtext = plot.legend)
ppcomp  (list(n.fit.logis, n.fit.norm), legendtext = plot.legend)
gofstat(list(n.fit.logis, n.fit.norm), fitnames = c("Logistic", "Normal")) #Normal looks best

# check assumptions
m1 <- lmer(carbon ~ state + insecticide + (1|plot), data = cn, REML=F)
# updates end here - need to merge meta-data with cn data
# code below is old




### Model comparison ###
m1a <- lm(carbon ~ state, data = cn_kbs)
m1b <- lm(carbon ~ state + insecticide, data = cn_kbs)
m1c <- lmer(carbon ~ state + (1|plot), data = cn_kbs)
m1d <- lmer(carbon ~ state + insecticide + (1|plot), data = cn_kbs)
AICctab(m1a, m1b, m1c, m1d, weights = T)
summary(m1a) # not including plot
summary(m1c) # plot as a random effect


m2a <- lm(nitrogen ~ state, data = cn_kbs)
m2b <- lm(nitrogen ~ state + insecticide, data = cn_kbs)
m2c <- lmer(nitrogen ~ state + (1|plot), data = cn_kbs)
m2d <- lmer(nitrogen ~ state + insecticide + (1|plot), data = cn_kbs)
AICctab(m2a, m2b, m2c, m2d, weights = T)
summary(m2a) # not including plot
summary(m2c) # plot as a random effect

# UMBS - Cest
m3a <- lm(carbon ~ state, data = cn_cest_umbs)
m3b <- lm(carbon ~ state + insecticide, data = cn_cest_umbs)
m3c <- lmer(carbon ~ state + (1|plot), data = cn_cest_umbs)
m3d <- lmer(carbon ~ state + insecticide + (1|plot), data = cn_cest_umbs)
AICctab(m3a, m3b, m3c, m3d, weights = T)
summary(m3a)
summary(m3c)

m4a <- lm(nitrogen ~ state, data = cn_cest_umbs)
m4b <- lm(nitrogen ~ state + insecticide, data = cn_cest_umbs)
m4c <- lmer(nitrogen ~ state + (1|plot), data = cn_cest_umbs)
m4d <- lmer(nitrogen ~ state + insecticide + (1|plot), data = cn_cest_umbs)
AICctab(m4a, m4b, m4c, m4d, weights = T)
summary(m4a)
summary(m4c)

# UMBS - Popr
m5a <- lm(carbon ~ state, data = cn_popr_umbs)
m5b <- lm(carbon ~ state + insecticide, data = cn_popr_umbs)
m5c <- lmer(carbon ~ state + (1|plot), data = cn_popr_umbs)
m5d <- lmer(carbon ~ state + insecticide + (1|plot), data = cn_popr_umbs)
AICctab(m5a, m5b, m5c, m5d, weights = T)
summary(m5a)
summary(m5c)

m6a <- lm(nitrogen ~ state, data = cn_popr_umbs)
m6b <- lm(nitrogen ~ state + insecticide, data = cn_popr_umbs)
m6c <- lmer(nitrogen ~ state + (1|plot), data = cn_popr_umbs)
m6d <- lmer(nitrogen ~ state + insecticide + (1|plot), data = cn_popr_umbs)
AICctab(m6a, m6b, m6c, m6d, weights = T)
summary(m6a)
summary(m6c)

