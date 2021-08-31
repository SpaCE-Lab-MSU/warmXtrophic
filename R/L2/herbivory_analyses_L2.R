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
library(bbmle)

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)
herb <- read.csv(file.path(L1_dir, "herbivory/final_herbivory_L1.csv"))




################## Checking data for both KBS and UMBS #####################
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
herb_kbs_in <- subset(herb, site == "kbs")
herb_umbs_in <- subset(herb, site == "umbs")
# made separate dataframes for insects & no insects because the amount of herbivory measurements between
# each species differs with each, and is relevant for the below data checks

# only keep species that were recorded in both warmed and ambient plots
herb_kbs <- herb_kbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs <- herb_umbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_kbs_in <- herb_kbs_in %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs_in <- herb_umbs_in %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))

# checking to see if any species/state combos are all zeros
with(herb_kbs,table(species,state,p_eaten==0)) 
with(herb_umbs,table(species,state,p_eaten==0))
with(herb_kbs_in,table(species,state,p_eaten==0)) 
with(herb_umbs_in,table(species,state,p_eaten==0))

# number of observation per species/state combo (to find rare species)
herb_kbs %>% count(state, species)
herb_umbs %>% count(state, species)
herb_kbs_in %>% count(state, species)
herb_umbs_in %>% count(state, species)

# removing rare species
herb_kbs <- herb_kbs[!grepl("Hype",herb_kbs$species),]
herb_kbs_in <- herb_kbs_in[!grepl("Ceor",herb_kbs_in$species),]
herb_kbs_in <- herb_kbs_in[!grepl("Dagl",herb_kbs_in$species),]
herb_kbs_in <- herb_kbs_in[!grepl("Pore",herb_kbs_in$species),]
herb_kbs_in <- herb_kbs_in[!grepl("Trpr",herb_kbs_in$species),]



###################### KBS distribution check ########################
# How much of the data is zeros?
100*sum(herb_kbs$p_eaten == 0)/nrow(herb_kbs) #68% - thats a lot! probably have to use a zero-inflated model,
# but I'll still check for normality & try some transformations below
100*sum(herb_umbs$p_eaten == 0)/nrow(herb_umbs) #61%
100*sum(herb_kbs_in$p_eaten == 0)/nrow(herb_kbs_in) #70.5%
100*sum(herb_umbs_in$p_eaten == 0)/nrow(herb_umbs_in) #69.6%

### determining distribution ###
descdist(herb_kbs$p_eaten, discrete = FALSE)
# normal distribution?
hist(herb_kbs$p_eaten)
qqnorm(herb_kbs$p_eaten)
shapiro.test(herb_kbs$p_eaten)
fit <- lm(p_eaten~state, data = herb_kbs)
qqPlot(fit)

# looking at each treatment separately
hist(herb_kbs$p_eaten[herb_kbs$state == "ambient"])
hist(herb_kbs$p_eaten[herb_kbs$state == "warmed"])

# gamma distribution? - error message "the function mle failed to estimate the parameters"
#fit.gamma <- fitdist(herb_kbs$p_eaten, "gamma")
#plot(fit.gamma)

# lognormal distribution? - error message "values must be positive to fit a lognormal"
#fit.ln <- fitdist(herb_kbs$p_eaten, "lnorm")
#plot(fit.ln)

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
qqnorm(herb_kbs$p_sqrt)
shapiro.test(herb_kbs$p_sqrt)

# quick look at insecticide plots
hist(herb_kbs_in$p_eaten)

# transformations are a no-go
# mean and var of non-zero counts
herb_kbs %>%
        dplyr::filter(p_eaten != "0") %>%
        dplyr::summarize(mean_eaten = mean(p_eaten, na.rm=T), var_eaten = var(p_eaten, na.rm=T))
herb_kbs_in %>%
        dplyr::filter(p_eaten != "0") %>%
        dplyr::summarize(mean_eaten = mean(p_eaten, na.rm=T), var_eaten = var(p_eaten, na.rm=T))
# variance is also > mean, so can't be poisson
# I'll try zero-inflated negative binomial due to an excess of zeros



############### KBS zero-inflated negative binomial - no insecticide ################
## models with state and year ##
# state as a fixed effect
k.m1 <- zeroinfl(p_eaten ~ state,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m1)

# state and year as fixed effects
k.m2 <- zeroinfl(p_eaten ~ state + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m2)
lrtest(k.m1, k.m2) # model 2

# state and growth habit as fixed effects
herb_kbs <- within(herb_kbs, growth_habit <- relevel(factor(growth_habit), ref = "Forb")) # releveling so forb is the reference
k.m3 <- zeroinfl(p_eaten ~ state + growth_habit,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m3)
lrtest(k.m2, k.m3) # model 2

# state, growth habit, and year as fixed effects
k.m4 <- zeroinfl(p_eaten ~ state + growth_habit + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m4)
lrtest(k.m2, k.m4) # model 4

# interaction between state and growth habit as fixed effects
k.m5 <- zeroinfl(p_eaten ~ state * growth_habit,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m5)
lrtest(k.m4, k.m5) # model 4

# interaction between state and growth habit as fixed effects, plus year
k.m6 <- zeroinfl(p_eaten ~ state * growth_habit + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m6)
lrtest(k.m4, k.m6) # virtually the same, keeping model 4 because its simpler
# calculating effect size of graminoids vs forb herbivory - accounting for log link
exp(0.470803 + 1.234010*0) # 1.60128
exp(0.470803 + 1.234010*1) # 5.500357
# effect of herbivory:
5.500357 - 1.60128 # 3.899077

# interaction between state, growth habit, and year (year as a factor wouldn't work - non-finite value)
k.m7 <- zeroinfl(p_eaten ~ state * growth_habit * year,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m7)
lrtest(k.m4, k.m7) # model 4

# state and origin as fixed effects
herb_kbs <- within(herb_kbs, origin <- relevel(factor(origin), ref = "Native")) # releveling so native is the reference
k.m8 <- zeroinfl(p_eaten ~ state + origin,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m8)
lrtest(k.m4, k.m8) # model 4

# state, origin, and year as fixed effects
k.m9 <- zeroinfl(p_eaten ~ state + origin + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m9)
lrtest(k.m4, k.m9) # model 4

# interaction between state and origin as fixed effects
k.m10 <- zeroinfl(p_eaten ~ state * origin,
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m10)
lrtest(k.m4, k.m10) # model 4

# interaction between state and origin as fixed effects, plus year
k.m11 <- zeroinfl(p_eaten ~ state * origin + as.factor(year),
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m11)
lrtest(k.m4, k.m11) # model 4
exp(0.43056 + 0.37613*0) # 1.538119
exp(0.43056 + 0.37613*1) # 2.24048
# effect of herbivory:
2.24048 - 1.538119 # 0.702361

# interaction between state, origin, and year
k.m12 <- zeroinfl(p_eaten ~ state * origin * year,
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m12)
lrtest(k.m4,k.m12) # model 4

# just origin - testing to see w/o state
k.m12.2 <- zeroinfl(p_eaten ~ origin,
                    dist = 'negbin',
                    data = herb_kbs)
summary(k.m12.2)

# state and species as fixed effects
k.m13 <- zeroinfl(p_eaten ~ state + species,
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m13)
lrtest(k.m4, k.m13) # model 4

# state. species and year as fixed effects
k.m14 <- zeroinfl(p_eaten ~ state + species + as.factor(year),
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m14)
lrtest(k.m4, k.m14) # model 14
# calculating effect size - accounting for log link
exp(0.27490 + -0.22879*0) # 1.316399
exp(0.27490 + -0.22879*1) # 1.04719
# effect of herbivory:
1.04719 - 1.316399 # -0.269209

# interaction between state and species as fixed effects, plus year
k.m15 <- zeroinfl(p_eaten ~ state * species + as.factor(year),
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m15)
lrtest(k.m14, k.m15) # model 15 slightly better, going with 14 because its simpler

# checking models again
lrtest(k.m2, k.m4, k.m9, k.m14) # model 14 best - with species
res.k <- AIC(k.m1, k.m2, k.m3, k.m4, k.m5, k.m6, k.m7, k.m8, k.m9, k.m10, k.m11,k.m12,k.m13,k.m14,k.m15)

## interaction between state, species, and year - doesn't run
#m8 <- zeroinfl(p_eaten ~ state * species * as.factor(year),
#                     dist = 'negbin',
#                     data = herb_kbs)
#summary(m8)

# check dispersion
E <- resid(k.m14, type = "pearson")
N  <- nrow(herb_kbs)
p  <- length(coef(k.m14)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # a little overdispersed - is that okay?

# pairwise comparisons
emmeans(k.m14, ~ state + species + as.factor(year))



############### KBS zero-inflated negative binomial - insecticide ################
# zero-inflated negative binomial
# insecticide as fixed effect
k.m1.i <- zeroinfl(p_eaten ~ insecticide,
                   dist = 'negbin',
                   data = herb_kbs_in)
summary(k.m1.i)

# full model
k.m2.i <- zeroinfl(p_eaten ~ insecticide + state + species + as.factor (year),
                   dist = 'negbin',
                   data = herb_kbs_in)
summary(k.m2.i)

# full model w/ interaction term
k.m3.i <- zeroinfl(p_eaten ~ insecticide * state + species + as.factor (year),
                   dist = 'negbin',
                   data = herb_kbs_in)
summary(k.m3.i)
# calculating effect size - accounting for log link
exp(0.70150 + -0.23501*0) # 2.016776
exp(0.70150 + -0.23501*1) # 1.594388
# effect of herbivory:
1.594388 - 2.016776 # -0.422388




###################### UMBS distribution check ###########################
# first, checking for normality
descdist(herb_umbs$p_eaten, discrete = FALSE)
# normal distribution?
hist(herb_umbs$p_eaten)
qqnorm(herb_umbs$p_eaten)
shapiro.test(herb_umbs$p_eaten)
fit <- lm(p_eaten~state, data = herb_umbs)
qqPlot(fit)

# looking at each treatment separately
hist(herb_umbs$p_eaten[herb_umbs$state == "ambient"])
hist(herb_umbs$p_eaten[herb_umbs$state == "warmed"])

# gamma distribution? - error message "the function mle failed to estimate the parameters"
#fit.gamma <- fitdist(herb_umbs$p_eaten, "gamma")
#plot(fit.gamma)

# lognormal distribution? - error message "values must be positive to fit a lognormal"
#fit.ln <- fitdist(herb_umbs$p_eaten, "lnorm")
#plot(fit.ln)

# log transform
herb_umbs$p_log <- log(herb_umbs$p_eaten+1)
hist(herb_umbs$p_log)
qqnorm(herb_umbs$p_log)
shapiro.test(herb_umbs$p_log) # NAs - data contains 0s

# mean centering p_eaten
herb_umbs$p_scaled <- herb_umbs$p_log - mean(herb_umbs$p_log)
hist(herb_umbs$p_scaled)
hist(herb_umbs$p_scaled[herb_umbs$state == "ambient"])
hist(herb_umbs$p_scaled[herb_umbs$state == "warmed"])
qqnorm(herb_umbs$p_scaled)
shapiro.test(herb_umbs$p_scaled)

# square root?
herb_umbs$p_sqrt <- sqrt(herb_umbs$p_eaten)
hist(herb_umbs$p_sqrt)
qqnorm(herb_umbs$p_sqrt)
shapiro.test(herb_umbs$p_sqrt)

# transformations are a no-go
# mean and var of non-zero counts
herb_umbs %>%
        dplyr::filter(p_eaten != "0") %>%
        dplyr::summarize(mean_eaten = mean(p_eaten, na.rm=T), var_eaten = var(p_eaten, na.rm=T))
# variance is also > mean, so can't be poisson
# I'll try zero-inflated negative binomial due to an excess of zeros



############### UMBS zero-inflated negative binomial - no insecticide ################
# state as a fixed effect
u.m1 <- zeroinfl(p_eaten ~ state,
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m1)

# state and year as fixed effects
u.m2 <- zeroinfl(p_eaten ~ state + as.factor(year),
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m2)
lrtest(u.m1, u.m2) # model 2

# state and growth habit as fixed effects
herb_umbs <- within(herb_umbs, growth_habit <- relevel(factor(growth_habit), ref = "Forb")) # releveling so forb is the reference
u.m3 <- zeroinfl(p_eaten ~ state + growth_habit,
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m3)
lrtest(u.m2, u.m3) # model 2

# state, growth habit, and year as fixed effects
u.m4 <- zeroinfl(p_eaten ~ state + growth_habit + as.factor(year),
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m4)
lrtest(u.m2, u.m4) # model 4

# interaction between state and growth habit as fixed effects
u.m5 <- zeroinfl(p_eaten ~ state * growth_habit,
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m5)
lrtest(u.m4, u.m5) # model 4

# interaction between state and growth habit as fixed effects, plus year
u.m6 <- zeroinfl(p_eaten ~ state * growth_habit + as.factor(year),
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m6)
lrtest(u.m4, u.m6) # almost the same, going with model 4 because its simpler

# interaction between state, growth habit, and year (year as a factor wouldn't woru - non-finite value)
u.m7 <- zeroinfl(p_eaten ~ state * growth_habit * year,
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m7)
lrtest(u.m4, u.m7) # model 4

# state and origin as fixed effects
herb_umbs <- within(herb_umbs, origin <- relevel(factor(origin), ref = "Native")) # releveling so native is the reference
u.m8 <- zeroinfl(p_eaten ~ state + origin,
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m8)
lrtest(u.m4, u.m8) # model 4

# state, origin, and year as fixed effects
u.m9 <- zeroinfl(p_eaten ~ state + origin + as.factor(year),
                 dist = 'negbin',
                 data = herb_umbs)
summary(u.m9)
lrtest(u.m4, u.m9) # model 4

# interaction between state and origin as fixed effects
u.m10 <- zeroinfl(p_eaten ~ state * origin,
                  dist = 'negbin',
                  data = herb_umbs)
summary(u.m10)
lrtest(u.m4, u.m10) # model 4

# interaction between state and origin as fixed effects, plus year
u.m11 <- zeroinfl(p_eaten ~ state * origin + as.factor(year),
                  dist = 'negbin',
                  data = herb_umbs)
summary(u.m11)
lrtest(u.m4, u.m11) # model 4
exp(-0.26760 + 0.37613*0) # 1.538119
exp(-0.26760 + 0.37613*1) # 2.24048
# effect of herbivory:
2.24048 - 1.538119 # 0.702361

## interaction between state, origin, and year - doesn't work
#u.m12 <- zeroinfl(p_eaten ~ state * origin * as.factor(year),
#                   dist = 'negbin',
#                   data = herb_umbs)
#summary(u.m12)

# state and species as fixed effects
u.m13 <- zeroinfl(p_eaten ~ state + species,
                  dist = 'negbin',
                  data = herb_umbs)
summary(u.m13)
lrtest(u.m4, u.m13) # model 4

# state, species and year as fixed effects
u.m14 <- zeroinfl(p_eaten ~ state + species + as.factor(year),
                  dist = 'negbin',
                  data = herb_umbs)
summary(u.m14)
lrtest(u.m4, u.m14) # model 14
# calculating effect size - accounting for log link
exp(-0.40972 + 0.26343*0) # 0.6638361
exp(-0.40972 + 0.26343*1) # 0.8639071
# effect of herbivory:
0.8639071 - 0.6638361 # 0.200071

# interaction between state and species as fixed effects, plus year
u.m15 <- zeroinfl(p_eaten ~ state * species + as.factor(year),
                  dist = 'negbin',
                  data = herb_umbs)
summary(u.m15)
lrtest(u.m14, u.m15) # model 15 - might go with 14 because its simpler

## interaction between state, species, and year - doesn't run
#m8 <- zeroinfl(p_eaten ~ state * species * year,
#                     dist = 'negbin',
#                     data = herb_umbs)
#summary(m8)

# checking models again
lrtest(u.m2, u.m4, u.m9, u.m14) # model 14 best - with species
res.u <- AIC(u.m1, u.m2, u.m3, u.m4, u.m5, u.m6, u.m7, u.m9, u.m10, u.m11,u.m13,u.m14,u.m15)

# check dispersion - chose lowest loglik model for example
E <- resid(u.m14, type = "pearson")
N  <- nrow(herb_umbs)
p  <- length(coef(u.m14)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # pretty close to one

# pairwise comparisons
emmeans(u.m14, ~ state + species + as.factor(year))



############### UMBS zero-inflated negative binomial - insecticide ################




################# KBS leaf damage ########################
# How much of the data is zeros?
100*sum(herb_kbs$p_damage == 0)/nrow(herb_kbs) #34%
100*sum(herb_umbs$p_damage == 0)/nrow(herb_umbs) #53%
100*sum(herb_kbs_in$p_damage == 0)/nrow(herb_kbs_in) #35%
100*sum(herb_umbs_in$p_damage == 0)/nrow(herb_umbs_in) #57%

### determining distribution ###
descdist(herb_kbs$p_damage, discrete = FALSE)
# normal distribution?
hist(herb_kbs$p_damage)
qqnorm(herb_kbs$p_damage)
shapiro.test(herb_kbs$p_damage)
fit <- lm(p_damage~state, data = herb_kbs)
qqPlot(fit)

# looking at each treatment separately
hist(herb_kbs$p_damage[herb_kbs$state == "ambient"])
hist(herb_kbs$p_damage[herb_kbs$state == "warmed"])

# gamma distribution? - error message "the function mle failed to estimate the parameters"
#fit.gamma <- fitdist(herb_kbs$p_damage, "gamma")
#plot(fit.gamma)

# lognormal distribution? - error message "values must be positive to fit a lognormal"
#fit.ln <- fitdist(herb_kbs$p_damage, "lnorm")
#plot(fit.ln)

# log transform
herb_kbs$p_log_damage <- log(herb_kbs$p_damage+1)
hist(herb_kbs$p_log_damage)
qqnorm(herb_kbs$p_log_damage)
shapiro.test(herb_kbs$p_log_damage)

# mean centering p_damage
herb_kbs$p_scaled_damage <- herb_kbs$p_damage - mean(herb_kbs$p_damage)
hist(herb_kbs$p_scaled_damage)
hist(herb_kbs$p_scaled_damage[herb_kbs$state == "ambient"])
hist(herb_kbs$p_scaled_damage[herb_kbs$state == "warmed"])
qqnorm(herb_kbs$p_scaled_damage)
shapiro.test(herb_kbs$p_scaled_damage)

# square root?
herb_kbs$p_sqrt_damage <- sqrt(herb_kbs$p_damage)
hist(herb_kbs$p_sqrt_damage)
qqnorm(herb_kbs$p_sqrt_damage)
shapiro.test(herb_kbs$p_sqrt_damage)

# quick look at insecticide plots
hist(herb_kbs_in$p_damage)

# transformations are a no-go
# mean and var of non-zero counts
herb_kbs %>%
        dplyr::filter(p_damage != "0") %>%
        dplyr::summarize(mean_damage = mean(p_damage, na.rm=T), var_damage = var(p_damage, na.rm=T))
herb_kbs_in %>%
        dplyr::filter(p_damage != "0") %>%
        dplyr::summarize(mean_damage = mean(p_damage, na.rm=T), var_damage = var(p_damage, na.rm=T))
# variance is also > mean, so can't be poisson
# I'll try zero-inflated negative binomial due to an excess of zeros

### zero-inflated negative binomial ###
## models with state and year ##
# state as a fixed effect
k.m1.d <- zeroinfl(p_damage ~ state,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m1.d)

# state and year as fixed effects
k.m2.d <- zeroinfl(p_damage ~ state + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m2.d)
lrtest(k.m1.d, k.m2.d) # model2

# state and growth habit as fixed effects
herb_kbs <- within(herb_kbs, growth_habit <- relevel(factor(growth_habit), ref = "Forb")) # releveling so forb is the reference
k.m3.d <- zeroinfl(p_damage ~ state + growth_habit,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m3.d)
lrtest(k.m2.d, k.m3.d) # model 2

# state, growth habit, and year as fixed effects
k.m4.d <- zeroinfl(p_damage ~ state + growth_habit + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m4.d)
lrtest(k.m2.d, k.m4.d) # model 4

# interaction between state and growth habit as fixed effects
k.m5.d <- zeroinfl(p_damage ~ state * growth_habit,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m5.d)
lrtest(k.m4.d, k.m5.d) # model 4

# interaction between state and growth habit as fixed effects, plus year
k.m6.d <- zeroinfl(p_damage ~ state * growth_habit + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m6.d)
lrtest(k.m4.d, k.m6.d) # m6
# calculating effect size of graminoids vs forb herbivory - accounting for log link
exp(0.470803 + 1.234010*0) # 1.60128
exp(0.470803 + 1.234010*1) # 5.500357
# effect of herbivory:
5.500357 - 1.60128 # 3.899077

# interaction between state, growth habit, and year (year as a factor wouldn't work - non-finite value)
k.m7.d <- zeroinfl(p_damage ~ state * growth_habit * year,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m7.d)
lrtest(k.m4.d, k.m7.d) # model 4

# state and origin as fixed effects
herb_kbs <- within(herb_kbs, origin <- relevel(factor(origin), ref = "Native")) # releveling so native is the reference
k.m8.d <- zeroinfl(p_damage ~ state + origin,
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m8.d)
lrtest(k.m4.d, k.m8.d) # model 4

# state, origin, and year as fixed effects
k.m9.d <- zeroinfl(p_damage ~ state + origin + as.factor(year),
                 dist = 'negbin',
                 data = herb_kbs)
summary(k.m9.d)
lrtest(k.m4.d, k.m9.d) # model 4

# interaction between state and origin as fixed effects
k.m10.d <- zeroinfl(p_damage ~ state * origin,
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m10.d)
lrtest(k.m4.d, k.m10.d) # model 4

# interaction between state and origin as fixed effects, plus year
k.m11.d <- zeroinfl(p_damage ~ state * origin + as.factor(year),
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m11.d)
lrtest(k.m4.d, k.m11.d) # model 11
exp(0.43056 + 0.37613*0) # 1.538119
exp(0.43056 + 0.37613*1) # 2.24048
# effect of herbivory:
2.24048 - 1.538119 # 0.702361

# interaction between state, origin, and year
k.m12.d <- zeroinfl(p_damage ~ state * origin * year,
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m12.d)
lrtest(k.m11.d,k.m12.d) # model 11

# just origin - testing to see w/o state
k.m12.2.d <- zeroinfl(p_damage ~ origin,
                    dist = 'negbin',
                    data = herb_kbs)
summary(k.m12.2.d)

# state and species as fixed effects
k.m13.d <- zeroinfl(p_damage ~ state + species,
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m13.d)
lrtest(k.m11.d, k.m13.d) # model 11

# state. species and year as fixed effects
k.m14.d <- zeroinfl(p_damage ~ state + species + as.factor(year),
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m14.d)
lrtest(k.m11.d, k.m14.d) # model 14
# calculating effect size - accounting for log link
exp(0.27490 + -0.22879*0) # 1.316399
exp(0.27490 + -0.22879*1) # 1.04719
# effect of herbivory:
1.04719 - 1.316399 # -0.269209

# interaction between state and species as fixed effects, plus year
k.m15.d <- zeroinfl(p_damage ~ state * species + as.factor(year),
                  dist = 'negbin',
                  data = herb_kbs)
summary(k.m15.d)
lrtest(k.m14.d, k.m15.d) # model 15 slightly better

# checking models again
lrtest(k.m2.d, k.m4.d, k.m9.d, k.m14.d) # model 14 best - with species
res.k <- AIC(k.m1.d, k.m2.d, k.m3.d, k.m4.d, k.m5.d, k.m6.d, k.m7.d, k.m8.d, k.m9.d, k.m10.d, k.m11.d,k.m12.d,k.m13.d,k.m14.d,k.m15.d)
res.k


# check dispersion
E <- resid(k.m14, type = "pearson")
N  <- nrow(herb_kbs)
p  <- length(coef(k.m14)) + 1 # '+1' is due to theta
sum(E^2) / (N - p) # a little overdispersed - is that okay?

# pairwise comparisons
emmeans(k.m14, ~ state + species + as.factor(year))



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

