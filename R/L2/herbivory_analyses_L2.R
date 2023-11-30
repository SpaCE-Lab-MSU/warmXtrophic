# TITLE:          Herbivory data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021 ; updated May 2021; updated June 2022; updated April 2023


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
library(AER)
#library(countreg)
library(sjPlot)
library(glmmTMB)
library(glmmADMB)

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)
herb <- read.csv(file.path(L1_dir, "herbivory/final_herbivory_L1.csv"))
str(herb)

# classifying p_eaten_bins column as a character
herb$p_eaten_bins <- as.factor(herb$p_eaten_bins)
str(herb)




################## Checking data for both KBS and UMBS #####################
# changing scale of years
herb$year1<-herb$year
herb$year[herb$year == 2015] <- 1
herb$year[herb$year == 2016] <- 2
herb$year[herb$year == 2017] <- 3
herb$year[herb$year == 2018] <- 4
herb$year[herb$year == 2019] <- 5
herb$year[herb$year == 2020] <- 6
herb$year1 <- as.factor(herb$year1)

# plot-level herb totals
herb_plot <- herb %>%
        group_by(plot, state, site, year1) %>%
        summarize(plot_total = sum(p_eaten))

# create dataframes for kbs and umbs only
herb_kbs <- subset(herb, site == "kbs")
herb_umbs <- subset(herb, site == "umbs")
herb_kbs_plot <- subset(herb_plot, site == "kbs")
herb_umbs_plot <- subset(herb_plot, site == "umbs")
# made separate dataframes for insects & no insects because the amount of herbivory measurements between
# each species differs with each, and is relevant for the below data checks

# only keep species that were recorded in both warmed and ambient plots
herb_kbs <- herb_kbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs <- herb_umbs %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))

# checking to see if any species/state combos are all zeros
with(herb_kbs,table(species,state,p_eaten==0)) 
with(herb_umbs,table(species,state,p_eaten==0))

# number of observation per species/state combo (to find rare species)
herb_kbs %>% count(state, species)
herb_umbs %>% count(state, species)

# making date column a date
herb_kbs$date <- as.Date(herb_kbs$date)
herb_umbs$date <- as.Date(herb_umbs$date)

# determine one date per year to avoid replication (also bc some years only measured once while others didn't)
# can have multiple dates per year if measurements were taken over 2 days & have diff plots/species
unique(herb_kbs$date)
kbs_date <- unique(herb_kbs[c("species","date", "plot","year1")])
herb_kbs <- herb_kbs %>%
        filter(!(date == "2015-09-04"))
# keeping two 2017s and 2019s because diff species/plots
unique(herb_umbs$date)
umbs_date <- unique(herb_umbs[c("species","date", "plot", "year1")])
herb_umbs <- herb_umbs %>%
        filter(!(date == "2015-08-12" | date == "2020-08-24" & plot == "B4")) # keeping two 2020's because diff plots (except this one)

# separate dataframes for insecticide treatments
herb_kbs_in <- subset(herb_kbs, insecticide == "insects")
herb_umbs_in <- subset(herb_umbs, insecticide == "insects")
herb_kbs_noin <- subset(herb_kbs, insecticide == "no_insects")
herb_umbs_noin <- subset(herb_umbs, insecticide == "no_insects")

# only keep species that were recorded in both warmed and ambient plots
herb_kbs_in <- herb_kbs_in %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs_in <- herb_umbs_in %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_kbs_noin <- herb_kbs_noin %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
herb_umbs_noin <- herb_umbs_noin %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))




###################### KBS herbivory distribution check ########################
# How much of the data is zeros?
100*sum(herb_kbs$p_eaten == 0)/nrow(herb_kbs) #69% - thats a lot! probably have to use some type of zero-inflated model,
# but I'll still check for normality & try some transformations below
100*sum(herb_umbs$p_eaten == 0)/nrow(herb_umbs) #61%
100*sum(herb_kbs_in$p_eaten == 0)/nrow(herb_kbs_in) #72%
100*sum(herb_umbs_in$p_eaten == 0)/nrow(herb_umbs_in) #70%

### determining distribution ###
descdist(herb_kbs$p_eaten, discrete = FALSE)
# normal distribution?
hist(herb_kbs$p_eaten)
qqnorm(herb_kbs$p_eaten)
shapiro.test(herb_kbs$p_eaten)
fit <- lm(p_eaten~state, data = herb_kbs)
qqPlot(fit)
hist(resid(fit))

# looking at each treatment separately
hist(herb_kbs$p_eaten[herb_kbs$state == "ambient"])
hist(herb_kbs$p_eaten[herb_kbs$state == "warmed"])

# log transform
herb_kbs$p_log <- log(herb_kbs$p_eaten+1)
hist(herb_kbs$p_log)
fit_log <- lm(p_log~state, data = herb_kbs)
qqPlot(fit_log)
hist(resid(fit_log))

# mean centering p_eaten
herb_kbs$p_scaled <- herb_kbs$p_log - mean(herb_kbs$p_log)
hist(herb_kbs$p_scaled)
hist(herb_kbs$p_scaled[herb_kbs$state == "ambient"])
hist(herb_kbs$p_scaled[herb_kbs$state == "warmed"])
fit_scaled <- lm(p_scaled~state, data = herb_kbs)
qqPlot(fit_scaled)
hist(resid(fit_scaled))
qqnorm(herb_kbs$p_scaled)
shapiro.test(herb_kbs$p_scaled)

# square root?
herb_kbs$p_sqrt <- sqrt(herb_kbs$p_eaten)
hist(herb_kbs$p_sqrt)
fit_sqrt <- lm(p_sqrt~state, data = herb_kbs)
qqPlot(fit_sqrt)
hist(resid(fit_sqrt))
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
# I'll try hurdle models due to an excess of zeros
# Note: hurdle models determined due to one process producing zeros
# (i.e., eaten or not eaten)




############### KBS herbivory hurdle model  ################
# hypothesized model
# however, hurdle function can't have random effects
k.hyp <- hurdle(p_eaten ~ state * insecticide + year,
                data = herb_kbs,
                dist = "negbin", 
                zero.dist = "binomial")
summary(k.hyp)

# using glmmTMB to account for random effect
# first, make sure the output matched the hurdle function output
trunc.k <- glmmTMB(p_eaten ~ state * insecticide + year,
                   data=herb_kbs,
                   zi=~.,
                   family=truncated_nbinom2)
summary(trunc.k) #matches the k.hyp output

# adding in random effects of plant number nested within species nested within plot
full.model.k <- glmmTMB(p_eaten ~ state * insecticide + year + (1|plot/species/plant_number),
                   data=herb_kbs,
                   zi=~.,
                   family=truncated_nbinom2)
summary(full.model.k) # * used in paper * #
ggpredict(full.model.k, c("state","insecticide"))
# back transforming #
# note: in zero-inflated model, we're testing the probability of being zero
# negative estimate (<0) means fewer 0's, positive estimate (>0) means more 0's
# positive estimate means it is more likely to be zero (or, there is a reduced probability of being eaten)
# negative estimates means it is less likely to be zero (or, there is an increased probability of being eaten)
invlogit(0.16274) # 0.54
invlogit(0.16274+0.44553) # 0.65
0.65-0.54 # 0.11 - reduced herbivory decreased the probability of being eaten by 0.11
tab_model(full.model.k)

# species specific model (for the supplement)
full.model.sp.k <- glmmTMB(p_eaten ~ state * insecticide + species + year + (1|plot/species/plant_number),
                        data=herb_kbs,
                        zi=~.,
                        family=truncated_nbinom2)
summary(full.model.sp.k) 
car::Anova(full.model.sp.k)
tab_model(full.model.sp.k)

# temperature model
herb_kbs_amb <- herb_kbs %>%
        filter(state == "ambient")
temp.model.k <- glmmTMB(p_eaten ~ mean_temp,
                           data=herb_kbs_amb,
                           zi=~.,
                           family=truncated_nbinom2)
summary(temp.model.k)
ggpredict(temp.model.k, "mean_temp")
# back transforming #
# want to find probability change from temp of 15 to temp of 16
# ( (p2 - p1) / p1 ) * 100 for probability change from p1 to p2
( invlogit(9.8971 - 0.5890*16) - 
                invlogit(9.8971 - 0.5890*15) ) / 
        invlogit(9.8971 - 0.5890*15) # for every degree increase in temp, probability of being eaten increases by 0.17
tab_model(temp.model.k) 

# origin models (for the supplement) #
herb_kbs2 <- herb_kbs %>%
        filter(!(origin == 'Both')) 
k.m2.ho <- hurdle(p_eaten ~ state * origin + year, data = herb_kbs2, dist = "negbin", 
                  zero.dist = "binomial")
summary(k.m2.ho)

# growth form models (for supplement) #
herb_kbs3 <- herb_kbs %>%
        filter(!(growth_habit == ""))
k.m2.hg <- hurdle(p_eaten ~ state * growth_habit + year, data = herb_kbs3, dist = "negbin", 
                  zero.dist = "binomial")
summary(k.m2.hg)





###################### UMBS herbivory distribution check ###########################
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




############### UMBS herbivory hurdle model  ################
# hypothesized model
# however, hurdle function can't have random effects
u.hyp <- hurdle(p_eaten ~ state * insecticide + year, data = herb_umbs, dist = "negbin", 
                zero.dist = "binomial")
summary(u.hyp)

# using glmmTMB to account for random effect
# first, make sure the output matched the hurdle function output
trunc.u <- glmmTMB(p_eaten ~ state * insecticide + year,
                   data=herb_umbs,
                   zi=~.,
                   family=truncated_nbinom2)
summary(trunc.u) #matches the u.hyp output

# adding in random effects of plant number nested within species nested within plot
full.model.u <- glmmTMB(p_eaten ~ state * insecticide + year + (1|plot/species/plant_number),
                      data=herb_umbs,
                      zi=~.,
                      family=truncated_nbinom2)
summary(full.model.u) # * model used in paper * #
# back transforming #
exp(1.22351) # 3.40 - estimate for insects
exp(1.22351 - 0.59676) # 1.87 - estimate for no insects
3.4-1.87 # 1.53 - reduced herbivory decreased herbivory levels by 1.53%
# note: in zero-inflated model, we're testing the probability of being zero
# negative estimate (<0) means fewer 0's, positive estimate (>0) means more 0's
# so a positive estimate means it is more likely to be zero (or, there is a reduced probability of being eaten)
invlogit(0.47128) # 0.62
invlogit(0.47128-0.57265) # 0.47
0.62-0.47 # 0.15 - warming increased the probability of being eaten by 0.15
invlogit(0.47128) # 0.62
invlogit(0.47128+0.76031) # 0.77
0.77-0.62 # 0.15 - reduced herbivory decreased the probability of being eaten by 0.15
tab_model(full.model.u)

# species specific model (for the supplement)
full.model.sp.u <- glmmTMB(p_eaten ~ state * insecticide + species + year + (1|plot/species/plant_number),
                           data=herb_umbs,
                           zi=~.,
                           family=truncated_nbinom2)
summary(full.model.sp.u) 
tab_model(full.model.sp.u)

# temperature model
herb_umbs_amb <- herb_umbs %>%
        filter(state == "ambient")
temp.model.u <- glmmTMB(p_eaten ~ mean_temp,
                        data=herb_umbs_amb,
                        zi=~.,
                        family=truncated_nbinom2)
plot(ggpredict(temp.model.u, terms = "mean_temp"))
summary(temp.model.u)
# back transforming #
# want to find change from temp of 15 to temp of 16
( exp(20.7013 - 1.1594*16) - exp(20.7013 - 1.1594*15) /
                exp(20.7013 - 1.1594*15) )
tab_model(temp.model.u)

# origin models (for supplement) #
herb_umbs2 <- herb_umbs %>%
        filter(!(origin == 'Both' |
                         origin == ""))
u.m2.ho <- hurdle(p_eaten ~ state * origin + year, data = herb_umbs2, dist = "negbin", 
                  zero.dist = "binomial")
summary(u.m2.ho)

# growth form models (for supplement) #
u.m2.hg <- hurdle(p_eaten ~ state * growth_habit + year, data = herb_umbs, dist = "negbin", 
                  zero.dist = "binomial")
summary(u.m2.hg) #*used this output in the paper*#







############### code not used in manuscript ####################
# except some of the origin and growth form code below, in the supplement

################# KBS plot-level analyses #####################
# first, checking for normality
descdist(herb_kbs_plot$plot_total, discrete = FALSE)
hist(herb_kbs_plot$plot_total)
qqnorm(herb_kbs_plot$plot_total)
shapiro.test(herb_kbs_plot$plot_total)
fit <- lm(plot_total~state, data = herb_kbs_plot)
qqPlot(fit)
hist(resid(fit))
shapiro.test(resid(fit))

# log
descdist(log(herb_kbs_plot$plot_total), discrete = FALSE)
hist(log(herb_kbs_plot$plot_total))
qqnorm(log(herb_kbs_plot$plot_total))
shapiro.test(log(herb_kbs_plot$plot_total))
fit <- lm(log(plot_total)~state, data = herb_kbs_plot)
qqPlot(fit)
hist(resid(fit))
shapiro.test(resid(fit))

# sqrt
descdist(sqrt(herb_kbs_plot$plot_total), discrete = FALSE)
hist(sqrt(herb_kbs_plot$plot_total))
qqnorm(sqrt(herb_kbs_plot$plot_total))
shapiro.test(sqrt(herb_kbs_plot$plot_total))
fit <- lm(sqrt(plot_total)~state, data = herb_kbs_plot)
qqPlot(fit)
hist(resid(fit))
shapiro.test(resid(fit))

# going with log, trying some models
mod_plot1 <- lmer(log(plot_total) ~ state + (1|plot), data = herb_kbs_plot)
mod_plot2 <- lmer(log(plot_total) ~ state + year1 + (1|plot), data = herb_kbs_plot)
mod_plot3 <- lmer(log(plot_total) ~ state * year1 + (1|plot), data = herb_kbs_plot)
anova(mod_plot1, mod_plot2)
anova(mod_plot2, mod_plot3)
summary(mod_plot2)





############### old code testing different models ##################
# KBS #
# hurdle models for only herbivory plots
k.m1.h <- hurdle(p_eaten ~ state + species + year, data = herb_kbs_in, dist = "negbin", 
                 zero.dist = "binomial")
k.m2.h <- hurdle(p_eaten ~ state * species + year, data = herb_kbs_in, dist = "negbin", 
                 zero.dist = "binomial")
k.m3.h <- hurdle(p_eaten ~ state + year, data = herb_kbs_in, dist = "negbin", 
                 zero.dist = "binomial")
lrtest(k.m1.h,k.m2.h, k.m3.h)
AICtab(k.m1.h,k.m2.h,k.m3.h) #m1
summary(k.m1.h) #*used this output in the paper*#

# hurdle models for only reduced herbivory plots
k.m1.h2 <- hurdle(p_eaten ~ state + species + year, data = herb_kbs_noin, dist = "negbin", 
                 zero.dist = "binomial")
k.m2.h2 <- hurdle(p_eaten ~ state * species + year, data = herb_kbs_noin, dist = "negbin", 
                 zero.dist = "binomial")
k.m3.h2 <- hurdle(p_eaten ~ state + year, data = herb_kbs_noin, dist = "negbin", 
                 zero.dist = "binomial")
lrtest(k.m1.h2,k.m2.h2, k.m3.h2)
AICtab(k.m1.h2,k.m2.h2,k.m3.h2) #m1
summary(k.m1.h2) #*used this output in the paper*#

# effect of insecticide?
k.m.i <- hurdle(p_eaten ~ insecticide, data = herb_kbs, dist = "negbin", 
                zero.dist = "binomial")
k.m.i <- hurdle(p_eaten ~ state + insecticide, data = herb_kbs, dist = "negbin", 
                zero.dist = "binomial")
k.m.i <- hurdle(p_eaten ~ state + insecticide, data = herb_kbs, dist = "negbin", 
                zero.dist = "binomial")
summary(k.m.i)
t.test(p_eaten~insecticide, data=herb_kbs_in)

# different packages w/ random effects
# trying this structure to see if I can include random effects, but I can't
# figure out how to specify a negative binomial dist to the count data and a binomial dist to the second model
# could run two models, one with negative binomial and one with binomial?

# binomial response (1 eaten / 0 not eaten)
herb_kbs$species <- as.factor(herb_kbs$species)
binom.k <- glmmadmb(p_eaten ~ state * insecticide + year + (1 | species), 
                    data = herb_kbs, family = "binomial")
summary(binom.k)
# truncated negative binomial (amount if > 0)
# note: this one doesn't work
trunc.k <- glmmadmb(p_eaten ~ state * insecticide + year + (1 | species), 
                    data = herb_kbs, family = "truncnbinom")
summary(trunc.k)

# diff package
binom.k <- glmmTMB(p_eaten ~ state * insecticide + year + (1|species/plant_number),
                   data=herb_kbs,
                   zi=~.,
                   family=nbinom1)
trunc.k <- glmmTMB(p_eaten ~ state * insecticide + year,
                   data=herb_kbs,
                   zi=~.,
                   family=truncated_nbinom2)
summary(binom.k)
summary(trunc.k)
summary(fit_hurdle_random2)
means <- emmeans(k.m1.h, ~ state)
pairs(means, adjust = "none")
# calculating effect size for count model - accounting for log link
exp(0.73960 + -0.33980*0) # 2.095097
exp(0.73960 + -0.33980*1) # 1.491526
# effect:
1.491526 - 2.095097 # 0.603571 % less herbivory on warmed plants (compared to ambient)

# calculating effect size of zero hurdle model - accounting for logit link
exp(0.53933 + -0.21288*0)/(1+exp(0.53933 + -0.21288*0)) # 0.6316565
exp(0.53933 + -0.21288*1)/(1+exp(0.53933 + -0.21288*1)) # 0.5808954
# effect:
0.5808954 - 0.6316565 # -0.0507611, so a 5 % lesser chance of experiencing herb. for warmed plants


# calculating odds
expCoef <- exp(coef((k.m1.h)))
expCoef <- matrix(expCoef, ncol = 2)
colnames(expCoef) <- c("Count_model","Zero_hurdle_model")
expCoef
# baseline odds of having no herbivory is 1.7. The odds decrease to 0.808 if the plant is warmed
# among plants that are eaten, baseline amount eaten is 2.095. This decreases to 0.71 if the plant is warmed (I think)

sum(predict(k.m1.h, type = "prob")[,1]) # total # of zeros in the data
rootogram(k.m1.h) # where bins fall below the 0 line = underfitting (this seems okay though?)

# interpreting output: https://stackoverflow.com/questions/61847129/interpreting-zero-inflated-regression-summary
# In sum, your zero model calculates the probability that an observations is not zero
# the count model fits a model on those observations that are not zero.

### origin ###
herb_kbs2 <- herb_kbs %>%
        filter(!(origin == 'Both')) 
k.m1.ho <- hurdle(p_eaten ~ state + origin + year, data = herb_kbs2, dist = "negbin", 
                  zero.dist = "binomial")
k.m2.ho <- hurdle(p_eaten ~ state * origin + year, data = herb_kbs2, dist = "negbin", 
                  zero.dist = "binomial")
k.m3.ho <- hurdle(p_eaten ~ state + year, data = herb_kbs2, dist = "negbin", 
                  zero.dist = "binomial")
lrtest(k.m1.ho,k.m2.ho, k.m3.ho)
AICtab(k.m1.ho,k.m2.ho,k.m3.ho) # going w m2 because we're interested in the interactive effects of warming + origin

summary(k.m2.ho) #*used this output in the paper*#
means <- emmeans(k.m1.ho, ~ origin)
pairs(means, adjust = "none")

plot_model(k.m1.ho, type = "pred", terms = c("origin"))
# calculating effect size for count model - accounting for log link
exp(1.27701 + -0.54123*0) # 3.585902
exp(1.27701 + -0.54123*1) # 2.087109
# effect:
2.087109 - 3.585902 # 1.498793 % less herbivory on native plants (compared to exotic)

# calculating effect size of zero hurdle model - accounting for logit link
exp(-0.19397 + 0.98801*0)/(1+exp(-0.19397 + 0.98801*0)) # 0.451659
exp(-0.19397 + 0.98801*1)/(1+exp(-0.19397 + 0.98801*1)) # 0.6886981
# effect:
0.6886981 - 0.451659 # 24  % greater chance of experiencing herb. for native plants

### growth form ###
herb_kbs3 <- herb_kbs %>%
        filter(!(growth_habit == ""))
k.m1.hg <- hurdle(p_eaten ~ state + growth_habit + year, data = herb_kbs3, dist = "negbin", 
                  zero.dist = "binomial")
k.m2.hg <- hurdle(p_eaten ~ state * growth_habit + year, data = herb_kbs3, dist = "negbin", 
                  zero.dist = "binomial")
k.m3.hg <- hurdle(p_eaten ~ state + year, data = herb_kbs3, dist = "negbin", 
                  zero.dist = "binomial")
lrtest(k.m1.hg,k.m2.hg,k.m3.hg)
AICtab(k.m1.hg,k.m2.hg,k.m3.hg) # going w m2 because we're interested in the interactive effects of warming + origin

summary(k.m2.hg) #*used this output in the paper*#
plot_model(k.m2.hg, type = "pred", terms = c("state", "growth_habit"))


########## KBS models with binned data ###########
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# ordinal logistic regression
# state, species and year as fixed effects
k.m16 <- polr(p_eaten_bins ~ state + species + year, data = herb_kbs, Hess=TRUE)
summary(k.m16)
coeftest(k.m16)





# UMBS #
# hurdle mdeols
u.m1.h <- hurdle(p_eaten ~ state + species + year, data = herb_umbs, dist = "negbin", 
                 zero.dist = "binomial")
u.m2.h <- hurdle(p_eaten ~ state * species + year, data = herb_umbs, dist = "negbin", 
                 zero.dist = "binomial")
u.m3.h <- hurdle(p_eaten ~ state + year, data = herb_umbs, dist = "negbin", 
                 zero.dist = "binomial")
lrtest(u.m1.h,u.m2.h, u.m3.h)
AICtab(u.m1.h,u.m2.h,u.m3.h) #m1

summary(u.m2.h) #*used this output in the paper*#
# calculating effect size of zero hurdle model - accounting for logit link
exp(-1.6161 + 2.1877*0)/(1+exp(-1.6161 + 2.1877*0)) #0.1657434
exp(-1.6161 + 2.1877*1)/(1+exp(-1.6161 + 2.1877*1)) #0.6391323
# effect:
0.6391323 - 0.1657434 # 47 % greater chance of experiencing herb. for warmed plants

sum(predict(u.m1.h, type = "prob")[,1]) # total # of zeros in the data
rootogram(u.m1.h) # where bins fall below the 0 line = underfitting (this seems okay though?)

# effect of insecticide?
u.m.i <- hurdle(p_eaten ~ insecticide, data = herb_umbs_in, dist = "negbin", 
                zero.dist = "binomial")
summary(u.m.i)
t.test(p_eaten~insecticide, data=herb_umbs_in)

# interpreting output: https://stackoverflow.com/questions/61847129/interpreting-zero-inflated-regression-summary
# In sum, your zero model calculates the probability that an observations is not zero
# the count model fits a model on those observations that are not zero.

### origin ###
herb_umbs2 <- herb_umbs %>%
        filter(!(origin == 'Both' |
                         origin == ""))
herb_umbs2 <- within(herb_umbs2, origin <- relevel(factor(origin), ref = "Exotic"))
herb_umbs2 <- within(herb_umbs2, state <- relevel(factor(state), ref = "ambient"))
u.m1.ho <- hurdle(p_eaten ~ state + origin + year, data = herb_umbs2, dist = "negbin", 
                  zero.dist = "binomial")
u.m2.ho <- hurdle(p_eaten ~ state * origin + year, data = herb_umbs2, dist = "negbin", 
                  zero.dist = "binomial")
u.m3.ho <- hurdle(p_eaten ~ state + year, data = herb_umbs2, dist = "negbin", 
                  zero.dist = "binomial")
lrtest(u.m1.ho,u.m2.ho, u.m3.ho)
AICtab(u.m1.ho,u.m2.ho,u.m3.ho) # going w m2 because we're interested in the interactive effects of warming + origin

summary(u.m2.ho) #*used this output in the paper*#
means <- emmeans(u.m2.ho, ~ origin*state)
pairs(means, adjust = "none")
plot_model(u.m2.ho, type = "pred", terms = c("origin", "state"))

# think these calculations are wrong below
# calculating effect size for count model interaction - accounting for log link
exp(1.18908) # 3.284058 intercept
exp(-0.51126) # 0.5997394 warming
exp(-0.26926) # 0.7639446 exotic
exp(0.49803) # 1.645476 interaction
# effect:
3.284058 + 0.5997394 + 0.7639446 + 1.645476 # 6.293218 amount of herbivory on warmed+exotic plants

exp(0.91982) # 2.508839 intercept
exp(-0.01323) # 0.9868571 warming
exp(0.26926) # 1.308995 native
exp(-0.498033) # 0.6077249 interaction
# effect:
2.508839 + 0.9868571 + 1.308995 + 0.6077249 # 5.412416 amount of herbivory on warmed+native plants

### growth form ###
herb_umbs <- within(herb_umbs, growth_habit <- relevel(factor(growth_habit), ref = "Forb"))
u.m1.hg <- hurdle(p_eaten ~ state + growth_habit + year, data = herb_umbs, dist = "negbin", 
                  zero.dist = "binomial")
u.m2.hg <- hurdle(p_eaten ~ state * growth_habit + year, data = herb_umbs, dist = "negbin", 
                  zero.dist = "binomial")
u.m3.hg <- hurdle(p_eaten ~ state + year, data = herb_umbs, dist = "negbin", 
                  zero.dist = "binomial")
lrtest(u.m1.hg,u.m2.hg, u.m3.hg)
AICtab(u.m1.hg,u.m2.hg,u.m3.hg) # going w m2 because we're interested in the interactive effects of warming + origin

summary(u.m2.hg) #*used this output in the paper*#
means <- emmeans(u.m2.hg, ~ growth_habit*state)
pairs(means, adjust = "none")
plot_model(u.m2.hg, type = "pred", terms = c("growth_habit","state"))



