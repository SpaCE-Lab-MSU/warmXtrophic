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
library(MuMIn)
#install.packages('TMB',type='source')

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

# create dataframes for kbs and umbs - remember that these contain species within plots
green_kbs <- subset(greenup, site == "kbs")
green_umbs <- subset(greenup, site == "umbs")

# species specific dataframes for kbs and umbs
green_kbs_spp <- subset(greenup_spp, site == "kbs")
green_umbs_spp <- subset(greenup_spp, site == "umbs")

# making dataframe containing only 2020 data
green_kbs_2020 <- green_kbs %>%
        filter(year == 2020)
green_umbs_2020 <- green_umbs %>%
        filter(year == 2020)

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

# KBS 2020
fitp2<- lm(med_half_cover_date~state, data = green_kbs_2020)
outlierTest(fitp2) # one outlier - 85; leaving for now because it seems true
#green_kbsp2 <- green_kbsp2[-(85),] # removing the outlier
qqPlot(fitp2, main="QQ Plot") 
hist(fitp2$residuals)
leveragePlots(fitp2)
leveneTest(residuals(fitp2) ~ green_kbs_2020$state)
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

# UMBS 2020
fitpu2<- lm(med_half_cover_date~state, data = green_umbs_2020)
outlierTest(fitpu2) # no outliers
qqPlot(fitpu2, main="QQ Plot") 
hist(fitpu2$residuals)
leveragePlots(fitpu2)
leveneTest(residuals(fitpu2) ~ green_umbs_2020$state)
ols_test_normality(fitpu2)
shapiro.test(resid(fitpu2))

# KBS state + year
fitp2_y<- lm(med_half_cover_date~state+as.factor(year), data = green_kbs)
outlierTest(fitp2_y) # no outliers
qqPlot(fitp2_y, main="QQ Plot") 
hist(fitp2_y$residuals)
leveragePlots(fitp2_y)
leveneTest(residuals(fitp2_y) ~ green_kbs$state)
leveneTest(residuals(fitp2_y) ~ green_kbs$year_factor) # doesn't work with numerical year
ols_test_normality(fitp2_y)
shapiro.test(resid(fitp2_y))

# UMBS state + year
fitpu2_y<- lm(med_half_cover_date~state+as.factor(year), data = green_umbs)
outlierTest(fitpu2_y)
qqPlot(fitpu2_y, main="QQ Plot") 
hist(fitpu2_y$residuals)
leveragePlots(fitpu2_y)
leveneTest(residuals(fitpu2_y) ~ green_umbs$state)
ols_test_normality(fitpu2_y)
shapiro.test(resid(fitpu2_y))



###### green-up models #####
## KBS ##

# plot level - final year model
mod_final_year <- lm(med_half_cover_date ~ state * insecticide, green_kbs_2020)
summary(mod_final_year)
anova(mod_final_year)

# plot-level model - all years
mod9p <- lmer(med_half_cover_date ~ state * insecticide * as.factor(year) + (1|plot), green_kbs, REML=FALSE)
summary(mod9p)
anova(mod9p)
# comparisons
contrast.k <- contrast(emmeans(mod9p, ~state*year), "pairwise", simple = "each", combine = F, adjust = "mvt")
contrast.k2 <- contrast(emmeans(mod9p, ~state*insecticide), "pairwise", simple = "each", combine = F, adjust = "mvt")
# making a table
kable(anova(mod9p), digits = 5) %>% kableExtra::kable_styling()
result.k = as.data.frame(contrast.k)
result.k <- result.k %>%
        mutate_if(is.numeric, round, digits=2)
kable(result.k) %>% kableExtra::kable_styling()
result.k2 = as.data.frame(contrast.k2)
result.k2 <- result.k2 %>%
        mutate_if(is.numeric, round, digits=2)
kable(result.k2) %>% kableExtra::kable_styling()

# adding in our temp data
# below, we test for green-up as a function of just temp to see how real temp data affects green-up
green_kbs_amb <- green_kbs %>%
        filter(state == "ambient")
modtest1 <- lmer(med_half_cover_date ~ mean_temp + (1|plot), green_kbs_amb, REML=FALSE)
anova(modtest1)
r.squaredGLMM(modtest1)
summary(modtest1)

# species model for supp
mod_spp_k <- lmer(spp_half_cover_date ~ state * insecticide * as.factor(year) + species + (1|plot), green_kbs_spp, REML=FALSE)
anova(mod_spp_k)
# making a table
kable(anova(mod_spp_k)) %>% kableExtra::kable_styling()



## UMBS ##

# plot level - final year model
modu_final_year <- lm(med_half_cover_date ~ state * insecticide, green_umbs_2020)
summary(modu_final_year)
anova(modu_final_year)

# plot-level - all years model
mod9p_u <- lmer(med_half_cover_date ~ state * insecticide * as.factor(year) + (1|plot), green_umbs, REML=FALSE)
summary(mod9p_u)
anova(mod9p_u) ### used in manuscript ###
# comparisons
contrast.u <- contrast(emmeans(mod9p_u, ~state*insecticide*as.factor(year)), "pairwise", simple = "each", combine = F, adjust = "mvt")
# making a table
kable(anova(mod9p_u), digits = 2) %>% kableExtra::kable_styling()
result = as.data.frame(contrast.u)
result <- result %>%
        mutate_if(is.numeric, round, digits=2)
kable(result) %>% kableExtra::kable_styling()

# adding in our temp data 
# below, we test for green-up as a function of just temp to see how real temp data affects green-up
green_umbs_amb <- green_umbs %>%
        filter(state == "ambient")
modtest1u <- lmer(med_half_cover_date ~ mean_temp + (1|plot), green_umbs_amb, REML=FALSE)
anova(modtest1u)
r.squaredGLMM(modtest1u)
summary(modtest1u)

# species model for supp
mod_spp_u <- lmer(spp_half_cover_date ~ state * insecticide * as.factor(year) + species + (1|plot), green_umbs_spp, REML=FALSE)
anova(mod_spp_u)
# making a table
kable(anova(mod_spp_u)) %>% kableExtra::kable_styling()


