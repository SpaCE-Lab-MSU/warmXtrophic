# TITLE:          Green-up data analysis
# AUTHORS:        Kara Dobson, Phoebe Zarnetske
# COLLABORATORS:  Moriah Young, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           updated April 2023
# NOTE:           This is a clean version of the original greenup analyses script, which contained tests on species
#                 and other tests that we are no longer using in the paper. That version can be found in the archived scripts


### data set-up ###
# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(ggplot2)
library(lmerTest)
library(olsrr)
library(predictmeans)
library(car)
library(fitdistrplus)
library(ggpubr)
library(rstatix)
library(vegan)
library(interactions)
library(sjPlot)
library(effects)
library(glmmTMB)
library(bbmle)
library(emmeans)
library(gtsummary)
library(knitr)
#install.packages('TMB',type='source')

# Set ggplot2 plots to bw: see here for more options: http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
theme_set(theme_bw(base_size = 14))

# Get data
L1_dir<-Sys.getenv("L1DIR")
L2_dir<-Sys.getenv("L2DIR")

# V2 below is plot level - its the median half cover date for all species per plot
# calculated in phenology_dates_L2
greenup <- read.csv(file.path(L2_dir,"greenup/final_greenup_plot_V2_L2.csv")) # V2 plot level greenup dates
greenup <- greenup %>% dplyr::select(-X) # get rid of "X" column
# species level green-up
# note: the species data frame is only used to demonstrate that green-up differs between species (for the supplement)
greenup_spp <- read.csv(file.path(L2_dir, "greenup/final_greenup_species_L2.csv")) 

# check variable types
str(greenup)

# Order warm and ambient so that warm shows up first in plotting
greenup$state<-as.factor(greenup$state)
levels(greenup$state)
greenup$state <- factor(greenup$state, levels(greenup$state)[c(2,1)])
levels(greenup$state)

# adding sequential year variable starting at 1: this is because 2016... are large numbers compare with other values in the dataset. We can always label axes with these real years.
greenup$year_factor[greenup$year == 2016] <- 1
greenup$year_factor[greenup$year == 2017] <- 2
greenup$year_factor[greenup$year == 2018] <- 3
greenup$year_factor[greenup$year == 2019] <- 4
greenup$year_factor[greenup$year == 2020] <- 5
greenup$year_factor[greenup$year == 2021] <- 6

greenup_spp$year_factor[greenup_spp$year == 2016] <- 1
greenup_spp$year_factor[greenup_spp$year == 2017] <- 2
greenup_spp$year_factor[greenup_spp$year == 2018] <- 3
greenup_spp$year_factor[greenup_spp$year == 2019] <- 4
greenup_spp$year_factor[greenup_spp$year == 2020] <- 5
greenup_spp$year_factor[greenup_spp$year == 2021] <- 6

# create dataframes for kbs and umbs - remember that these contain species within plots
green_kbs <- subset(greenup, site == "kbs")
green_umbs <- subset(greenup, site == "umbs")

# species specific dataframes for kbs and umbs
green_kbs_spp <- subset(greenup_spp, site == "kbs")
green_umbs_spp <- subset(greenup_spp, site == "umbs")

# checking raw data
hist(green_kbs$med_half_cover_date)
qqnorm(green_kbs$med_half_cover_date)
shapiro.test(green_kbs$med_half_cover_date)

hist(green_umbs$med_half_cover_date)
qqnorm(green_umbs$med_half_cover_date)
shapiro.test(green_umbs$med_half_cover_date)

hist(green_kbs_spp$spp_half_cover_date)
qqnorm(green_kbs_spp$spp_half_cover_date)
shapiro.test(green_kbs_spp$spp_half_cover_date)

hist(green_umbs_spp$spp_half_cover_date)
qqnorm(green_umbs_spp$spp_half_cover_date)
shapiro.test(green_umbs_spp$spp_half_cover_date)


# Leverage plots and detecting Outliers. https://www.statmethods.net/stats/rdiagnostics.html
# These illustrate whether certain data points have more leverage (more influence),
# and thus could be outliers. It's a way of detecting outliers. Leverage plots can help identify whether a
# point has high or low influence, based on its leverage and residual and determining model fit with and
# without the point in question. Ultimately you decide whether the points are outliers or not, based on the
# knowledge of the system and how much it changes the model when included vs. excluded from the data used to
# fit the model. Here is a good overview of the combination of leverage and residual:
# scroll down to sections beginning at "13.3 Unusual Observations": https://daviddalpiaz.github.io/appliedstats/model-diagnostics.html



### plot level green-up data checking ###
# KBS
fitp2<- lm(med_half_cover_date~state, data = green_kbs)
outlierTest(fitp2) # one outlier - 85; leaving for now because it seems true
#green_kbsp2 <- green_kbsp2[-(85),] # removing the outlier
qqPlot(fitp2, main="QQ Plot") 
hist(fitp2$residuals)
leveragePlots(fitp2)
leveneTest(residuals(fitp2) ~ green_kbs$state)
ols_test_normality(fitp2)
shapiro.test(resid(fitp2))

# UMBS
fitpu2<- lm(med_half_cover_date~state, data = green_umbs)
outlierTest(fitpu2) # no outliers
qqPlot(fitpu2, main="QQ Plot") 
hist(fitpu2$residuals)
leveragePlots(fitpu2)
leveneTest(residuals(fitpu2) ~ green_umbs$state)
ols_test_normality(fitpu2)
shapiro.test(resid(fitpu2))

# KBS state + year
fitp2_y<- lm(med_half_cover_date~state+year_factor, data = green_kbs)
outlierTest(fitp2_y) # no outliers
qqPlot(fitp2_y, main="QQ Plot") 
hist(fitp2_y$residuals)
leveragePlots(fitp2_y)
leveneTest(residuals(fitp2_y) ~ green_kbs$state)
leveneTest(residuals(fitp2_y) ~ green_kbs$year_factor) # doesn't work with numerical year
ols_test_normality(fitp2_y)
shapiro.test(resid(fitp2_y))

# UMBS state + year
fitpu2_y<- lm(med_half_cover_date~state+year, data = green_umbs)
outlierTest(fitpu2_y)
qqPlot(fitpu2_y, main="QQ Plot") 
hist(fitpu2_y$residuals)
leveragePlots(fitpu2_y)
leveneTest(residuals(fitpu2_y) ~ green_umbs$state)
ols_test_normality(fitpu2_y)
shapiro.test(resid(fitpu2_y))





###### green-up models #####
## KBS ##
# testing if year as continuous or year as a factor makes more sense (test models)
year.mod.test1 <- lmer(med_half_cover_date ~ state + year_factor+(1|plot), green_kbs, REML=FALSE)
year.mod.test2 <- lmer(med_half_cover_date ~ state + as.factor(year_factor)+(1|plot), green_kbs, REML=FALSE)
anova(year.mod.test1,year.mod.test2)
# going to use year as a factor
# I also think this makes the most sense because I don't think we expect there to be a much of a trend over time for green-up; maybe if we had a longer time span, but I don't think 6 years is enough to detect a trend over time and the results will be more influenced by between year variation in temp, precip, etc.

# plot-level models using the re-summarized data frame
mod1p <- lmer(med_half_cover_date ~ state + (1|plot), green_kbs, REML=FALSE)
mod2p <- lmer(med_half_cover_date ~ insecticide + (1|plot), green_kbs, REML=FALSE)
mod3p <- lmer(med_half_cover_date ~ insecticide + state + (1|plot), green_kbs, REML=FALSE)
mod4p <- lmer(med_half_cover_date ~ insecticide * state + (1|plot), green_kbs, REML=FALSE)
mod5p <- lmer(med_half_cover_date ~ state + as.factor(year_factor) + (1|plot), green_kbs, REML=FALSE)
mod6p <- lmer(med_half_cover_date ~ state + as.factor(year_factor) + insecticide + (1|plot), green_kbs, REML=FALSE)
mod7p <- lmer(med_half_cover_date ~ state * as.factor(year_factor) + (1|plot), green_kbs, REML=FALSE)
mod8p <- lmer(med_half_cover_date ~ state * as.factor(year_factor) + insecticide + (1|plot), green_kbs, REML=FALSE)
mod9p <- lmer(med_half_cover_date ~ state * insecticide + as.factor(year_factor) + (1|plot), green_kbs, REML=FALSE)
mod10p <- lmer(med_half_cover_date ~ state + insecticide * as.factor(year_factor) + (1|plot), green_kbs, REML=FALSE)
mod11p <- lmer(med_half_cover_date ~ state * insecticide * as.factor(year_factor) + (1|plot), green_kbs, REML=FALSE)

AICctab(mod1p, mod2p, mod3p, mod4p, mod5p, mod6p, mod7p, mod8p, mod9p, mod10p, mod11p,weights=T)

anova(mod1p, mod2p) #same, 2p very slightly better
anova(mod2p, mod3p) #3p
anova(mod3p, mod4p) #3p
anova(mod3p, mod5p) #5p
anova(mod5p, mod6p) #6p
anova(mod6p, mod7p) #7p
anova(mod7p, mod8p) #mod8p
anova(mod8p, mod9p) #mod8p (note: 9p is hypothesized model - going with 9 because it contains the interaction)
anova(mod9p, mod10p) #mod9p
anova(mod9p, mod11p) #mod11p slightly better - going with 9p for overall results, but can use 11p to talk about how these results vary by year

summary(mod9p)
anova(mod9p)

# comparisons
contrast(emmeans(mod9p, ~state*insecticide), "pairwise", simple = "each", combine = F, adjust = "mvt")
emmip(mod9p, insecticide~state)
# making a table
kable(anova(mod9p)) %>% kableExtra::kable_styling()

# adding in our temp data into some models
# note: including state, year, and temp data into a model leads to rank deficiency
# so below, we test for green-up as a function of just temp to see how real temp data affects green-up
green_kbs_amb <- green_kbs %>%
        filter(state == "ambient")
modtest1 <- lmer(med_half_cover_date ~ mean_temp + (1|plot), green_kbs_amb, REML=FALSE)
modtest2 <- lmer(med_half_cover_date ~ GDD_cumulative + (1|plot), green_kbs_amb, REML=FALSE)

AICtab(modtest1,modtest2)

anova(modtest1)
summary(modtest1)

# species model for supp
mod_spp_k <- lmer(spp_half_cover_date ~ state * insecticide + species + as.factor(year_factor) + (1|plot), green_kbs_spp, REML=FALSE)
anova(mod_spp_k)
# making a table
kable(anova(mod_spp_k)) %>% kableExtra::kable_styling()



## UMBS ##
# testing if year as continuous or year as a factor makes more sense (test models)
year.mod.test1u <- lmer(med_half_cover_date ~ state + year_factor+(1|plot), green_umbs, REML=FALSE)
year.mod.test2u <- lmer(med_half_cover_date ~ state + as.factor(year_factor)+(1|plot), green_umbs, REML=FALSE)
anova(year.mod.test1u,year.mod.test2u)
# going to use year as a factor

# plot-level models using the re-summarized data frame
mod1p_u <- lmer(med_half_cover_date ~ state + (1|plot), green_umbs, REML=FALSE)
mod2p_u <- lmer(med_half_cover_date ~ insecticide + (1|plot), green_umbs, REML=FALSE)
mod3p_u <- lmer(med_half_cover_date ~ insecticide + state + (1|plot), green_umbs, REML=FALSE)
mod4p_u <- lmer(med_half_cover_date ~ insecticide * state + (1|plot), green_umbs, REML=FALSE)
mod5p_u <- lmer(med_half_cover_date ~ state + as.factor(year_factor) + (1|plot), green_umbs, REML=FALSE)
mod6p_u <- lmer(med_half_cover_date ~ state + as.factor(year_factor) + insecticide + (1|plot), green_umbs, REML=FALSE)
mod7p_u <- lmer(med_half_cover_date ~ state * as.factor(year_factor) + (1|plot), green_umbs, REML=FALSE)
mod8p_u <- lmer(med_half_cover_date ~ state * as.factor(year_factor) + insecticide + (1|plot), green_umbs, REML=FALSE)
mod9p_u <- lmer(med_half_cover_date ~ state * insecticide + as.factor(year_factor) + (1|plot), green_umbs, REML=FALSE)
mod10p_u <- lmer(med_half_cover_date ~ state + insecticide * as.factor(year_factor) + (1|plot), green_umbs, REML=FALSE)
mod11p_u <- lmer(med_half_cover_date ~ state * insecticide * as.factor(year_factor) + (1|plot), green_umbs, REML=FALSE)

AICtab(mod1p_u, mod2p_u, mod3p_u, mod4p_u, mod5p_u, mod6p_u, mod7p_u, mod8p_u, mod9p_u, mod10p_u, mod11p_u,weights=T)

anova(mod1p_u, mod2p_u) #2p
anova(mod2p_u, mod3p_u) #2p
anova(mod2p_u, mod4p_u) #2p
anova(mod2p_u, mod5p_u) #mod5p
anova(mod5p_u, mod6p_u) #mod6p
anova(mod6p_u, mod7p_u) #mod6p_u
anova(mod6p_u, mod8p_u) #mod6p_u
anova(mod6p_u, mod9p_u) #mod6p_u 
anova(mod6p_u, mod10p_u) #mod6p_u
anova(mod6p_u, mod11p_u) #mod11p_u

summary(mod9p_u)
anova(mod9p_u)

# comparisons
contrast(emmeans(mod9p_u, ~state*insecticide), "pairwise", simple = "each", combine = F, adjust = "mvt")
emmip(mod9p, insecticide~state)
# making a table
kable(anova(mod9p_u)) %>% kableExtra::kable_styling()


# adding in our temp data into some models
# note: including state, year, and temp data into a model leads to rank deficiency
# so below, we test for green-up as a function of just temp to see how real temp data affects green-up
green_umbs_amb <- green_umbs %>%
        filter(state == "ambient")
modtest1u <- lmer(med_half_cover_date ~ mean_temp + (1|plot), green_umbs_amb, REML=FALSE)
modtest2u <- lmer(med_half_cover_date ~ GDD_cumulative + (1|plot), green_umbs_amb, REML=FALSE)

AICtab(modtest1u,modtest2u)

anova(modtest1u)
summary(modtest1u)

# species model for supp
mod_spp_u <- lmer(spp_half_cover_date ~ state * insecticide + species + as.factor(year_factor) + (1|plot), green_umbs_spp, REML=FALSE)
anova(mod_spp_u)
# making a table
kable(anova(mod_spp_u)) %>% kableExtra::kable_styling()



