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

# Remove NAs
herb <- herb[complete.cases(herb),]

# plot-level herb totals
herb_plot <- herb %>%
        group_by(plot, state, site, year1) %>%
        summarize(plot_total = sum(p_eaten))

# create dataframes for kbs and umbs only for plots with no insecticide
herb_kbs <- subset(herb, site == "kbs" & insecticide == "insects")
herb_umbs <- subset(herb, site == "umbs" & insecticide == "insects")
herb_kbs_in <- subset(herb, site == "kbs")
herb_umbs_in <- subset(herb, site == "umbs")
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
# not doing this for now
# herb_kbs_in <- herb_kbs_in[!grepl("Pore",herb_kbs_in$species),]

# making date column a date
herb_kbs$date <- as.Date(herb_kbs$date)
herb_umbs$date <- as.Date(herb_umbs$date)
herb_kbs_in$date <- as.Date(herb_kbs_in$date)
herb_umbs_in$date <- as.Date(herb_umbs_in$date)

# determine one date per year to avoid replication (also bc some years only measured once while others didn't)
# can have multiple dates per year if measurements were taken over 2 days & have diff plots/species
unique(herb_kbs$date)
kbs_date <- unique(herb_kbs[c("species","date", "plot")])
herb_kbs <- herb_kbs %>%
        filter(!(date == "2015-09-04")) # keeping two 2019's because they're diff species

unique(herb_umbs$date)
umbs_date <- unique(herb_umbs[c("species","date", "plot")])
herb_umbs <- herb_umbs %>%
        filter(!(date == "2015-08-12" | date == "2020-08-24" & plot == "B4")) # keeping two 2020's bc diff plots

unique(herb_kbs_in$date)
kbs_in_date <- unique(herb_kbs_in[c("species","date", "plot")])
herb_kbs_in <- herb_kbs_in %>%
        filter(!(date == "2015-09-04")) # keeping two 2017s and 2019s because diff species/plots

unique(herb_umbs_in$date) 
umbs_in_date <- unique(herb_umbs_in[c("species","date", "plot")])
herb_umbs_in <- herb_umbs_in %>%
        filter(!(date == "2015-08-12")) # keeping two 2020's because diff plots


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
# I'll try zero-inflated negative binomial + hurdle models due to an excess of zeros
# Note: I think a hurdle model would be more appropriate here, since one process is producing zeros
# (i.e., eaten or not eaten)



############### KBS herbivory hurdle model  ################
# making a column for decimal version of herbivory
# I thought this would work for my test of a diff binomial hurdle model below, but it doesn't
herb_kbs$p_eaten_dec <- paste0("0.", herb_kbs$p_eaten)
herb_kbs$p_eaten_dec <- as.numeric(herb_kbs$p_eaten_dec)

# binomial response (1 eaten / 0 not eaten)
herb_kbs_in$species <- as.factor(herb_kbs_in$species)
binom.k <- glmmadmb(p_eaten ~ state * insecticide + year + (1 | species), 
                    data = herb_kbs_in, family = "binomial")
summary(binom.k)
# truncated negative binomial (amount if > 0)
trunc.k <- glmmadmb(p_eaten ~ state * insecticide + year + (1 | species), 
                    data = herb_kbs_in, family = "truncnbinom")
summary(trunc.k)

# hypothesized model
k.hyp <- hurdle(p_eaten ~ state * insecticide + year + species, data = herb_kbs_in, dist = "negbin", 
                zero.dist = "binomial")
summary(k.hyp)

# hurdle models
k.m1.h <- hurdle(p_eaten ~ state + species + year, data = herb_kbs, dist = "negbin", 
                 zero.dist = "binomial")
k.m2.h <- hurdle(p_eaten ~ state * species + year, data = herb_kbs, dist = "negbin", 
                 zero.dist = "binomial")
k.m3.h <- hurdle(p_eaten ~ state + year, data = herb_kbs, dist = "negbin", 
                 zero.dist = "binomial")
lrtest(k.m1.h,k.m2.h, k.m3.h)
AICtab(k.m1.h,k.m2.h,k.m3.h) #m1
summary(k.m1.h) #*used this output in the paper*#

# effect of insecticide?
k.m.i <- hurdle(p_eaten ~ insecticide, data = herb_kbs_in, dist = "negbin", 
                zero.dist = "binomial")
k.m.i <- hurdle(p_eaten ~ state + insecticide, data = herb_kbs_in, dist = "negbin", 
                zero.dist = "binomial")
k.m.i <- hurdle(p_eaten ~ state + insecticide, data = herb_kbs_in, dist = "negbin", 
                zero.dist = "binomial")
summary(k.m.i)
t.test(p_eaten~insecticide, data=herb_kbs_in)

# different package w/ random effects
# trying this structure to see if I can include random effects, but I can't
# figure out how to specify a negative binomial dist to the count data and a binomial dist to the second model
# could run two models, one with negative binomial and one with binomial?
# note: these don't work
fit_hurdle_random1 <- glmmTMB(p_eaten_dec ~ state + species + year + (1|plant_number),
                              data=herb_kbs,
                              zi=~state+species+year,
                              family=nbinom1)
fit_hurdle_random2 <- glmmTMB(p_eaten ~ state + species + year + (1|plant_number),
                              data=herb_kbs,
                              zi=~state+species+year,
                              family=truncated_nbinom1)

binom.k <- glmmTMB(p_eaten ~ state * insecticide + year + (1|species),
                   data=herb_kbs_in,
                   zi=~state * insecticide,
                   family=nbinom1)
trunc.k <- glmmTMB(p_eaten ~ state * insecticide + year + (1|species),
                   data=herb_kbs_in,
                   zi=~state * insecticide,
                   family=truncated_nbinom1)
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

## Questions
# Did I back transform correctly above & interpret the model right?
# I know the count model fits a model to the non-zero data while the zero hurdle calculates the prob that a observation is not zero
# for the count model, I used exp() to back transform due to log link, while the zero model used a logit link
# I'm confused on the percentage part - For the count model, I treated the effect as the same units as the data themselves, which are % leaf eaten
# so the effect of warming in the count model shows 0.60% less leaf eaten compared to ambient
# however, for the zero model, is it given as a true percentage, and therefore the decimal point needs moved?
# I did that above, to show 5% lesser change, but is it actually 0.05%?
# could also just report the odds ratio, shown below

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



############### UMBS herbivory hurdle model - no insecticide ################

# hypothesized model
u.hyp <- hurdle(p_eaten ~ state * insecticide + species + year, data = herb_umbs_in, dist = "negbin", 
                zero.dist = "binomial")
summary(u.hyp)

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



##### temp hurdle models - both sites ####
# merging kbs and umbs data
herb_comb <- rbind(herb_kbs,herb_umbs)

# testing the effect of temp
comb.m1.h <- hurdle(p_eaten ~ GDD_cumulative, data = herb_comb, dist = "negbin", 
                    zero.dist = "binomial")
comb.m2.h <- hurdle(p_eaten ~ mean_temp, data = herb_comb, dist = "negbin", 
                    zero.dist = "binomial")
comb.m3.h <- hurdle(p_eaten ~ GDD_cumulative + site, data = herb_comb, dist = "negbin", 
                    zero.dist = "binomial")
comb.m4.h <- hurdle(p_eaten ~ mean_temp + site, data = herb_comb, dist = "negbin", 
                    zero.dist = "binomial")
comb.m5.h <- hurdle(p_eaten ~ GDD_cumulative * site, data = herb_comb, dist = "negbin", 
                    zero.dist = "binomial")
comb.m6.h <- hurdle(p_eaten ~ mean_temp * site, data = herb_comb, dist = "negbin", 
                    zero.dist = "binomial")

AICtab(comb.m1.h,comb.m2.h,comb.m3.h,comb.m4.h,comb.m5.h,comb.m6.h)

# mod 4
summary(comb.m6.h)


### temp models - kbs ###
# testing the effect of temp w/ both models
kbstemp.m1.h <- hurdle(p_eaten ~ GDD_cumulative, data = herb_kbs, dist = "negbin", 
                       zero.dist = "binomial")
kbstemp.m2.h <- hurdle(p_eaten ~ mean_temp, data = herb_kbs, dist = "negbin", 
                       zero.dist = "binomial")

AICtab(kbstemp.m1.h,kbstemp.m2.h)
summary(kbstemp.m2.h)


### temp models - umbs ###
# testing the effect of temp w/ both models
umbstemp.m1.h <- hurdle(p_eaten ~ GDD_cumulative, data = herb_umbs, dist = "negbin", 
                        zero.dist = "binomial")
umbstemp.m2.h <- hurdle(p_eaten ~ mean_temp, data = herb_umbs, dist = "negbin", 
                        zero.dist = "binomial")

AICtab(umbstemp.m1.h,umbstemp.m2.h)
summary(umbstemp.m2.h)



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

