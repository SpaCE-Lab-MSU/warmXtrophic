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
library(vegan)
library(interactions)
library(sjPlot)
library(effects)
library(glmmTMB)
library(lmerTest)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Set ggplot2 plots to bw: see here for more options: http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements
theme_set(theme_bw(base_size = 14))
# Read in plant comp data - edit this in "plant_comp_clean_L0.R" so it's not an issue here with extra column.
greenup <- read.csv("L1/greenup/final_greenup_L1.csv")
# check variable types
str(greenup)
# KARA: omit this once fixed in plant_comp_clean_L0.R
# remove first column 
greenup$X<-NULL
greenup$species[greenup$species == "Bare_Ground"] <- NA
greenup$species[greenup$species == "Brown"] <- NA
greenup$species[greenup$species == "Litter"] <- NA
greenup$species[greenup$species == "Vert_Litter"] <- NA
greenup$species[greenup$species == "Animal_Disturbance"] <- NA
greenup <- na.omit(greenup)

# Order warm and ambient so that warm shows up first in plotting (and is default is red = warm; blue = ambient). First make it a factor.
greenup$state = with(greenup, reorder(class, hwy, median))


greenup$state<-as.factor(greenup$state)
levels(greenup$state)
greenup$state <- factor(greenup$state, levels(greenup$state)[c(2,1)])
levels(greenup$state)

# adding sequential year variable starting at 1: this is because 2015... are large numbers compare with other values in the dataset. We can always label axes with these real years.
greenup$year1<-greenup$year
greenup$year[greenup$year == 2015] <- 1
greenup$year[greenup$year == 2016] <- 2
greenup$year[greenup$year == 2017] <- 3
greenup$year[greenup$year == 2018] <- 4
greenup$year[greenup$year == 2019] <- 5
greenup$year[greenup$year == 2020] <- 6

# create dataframes for kbs and umbs - remember that these contain species within plots
green_kbs <- subset(greenup, site == "kbs")
green_umbs <- subset(greenup, site == "umbs")



# copied from the Rmd so I can extract the plots
mod7 <- lmer(spp_half_cover_date ~ state + year + insecticide + (1|species) + (1+year|plot), green_kbs, REML=FALSE)
anova(mod5, mod7)
summary(mod7)
# Yup, seems to matter but it is making this more complex, though not overly so because it's on the random effects structure only.
plot_model(mod7, sort.est = TRUE)
# these are the fixed predicted values:
mod7_plot <- plot_model(mod7, type = "pred", terms = c("year", "state", "insecticide"))
mod7_plot + labs(x = "Year", y = "Julian day of greenup", title = "Predicted values of half cover date")
# these are the random effects estimates
plot_model(mod7, type = "re", terms = c("species", "plot"))

mod8 <- lmer(spp_half_cover_date ~ state + species + year + (1|plot), green_kbs, REML=FALSE)
anova(mod5, mod8)
summary(mod8)
plot_model(mod8, sort.est = TRUE, colors = "gs")
# these are the fixed predicted values:
mod8_plot <- plot_model(mod8, type = "pred", terms = c("year", "state"))
mod8_plot + labs(x = "Year", y = "Julian day of greenup", title = "Predicted values of half cover date")
# these are the random effects estimates
plot_model(mod8, type = "re", terms = c("species"))
plot_model(mod8, type = "pred", terms = c("species", "state"))








# my old code #
######### kbs ############
#### data exploration & determining distribution ####
# first, checking for normality
hist(green_kbs$half_cover_date)
qqnorm(green_kbs$half_cover_date)
shapiro.test(green_kbs$half_cover_date)

# checking fit for date as a function of state
fit <- lm(half_cover_date~state, data = green_kbs)
hist(fit$residuals)
leveragePlots(fit)

# checking fit for date as a function of state and year
fit2 <- lm(half_cover_date~state+year, data = green_kbs)
hist(fit2$residuals)
leveragePlots(fit2)

# histograms for each treatment separately - look almost identical
hist(green_kbs$half_cover_date[green_kbs$state == "ambient"])
hist(green_kbs$half_cover_date[green_kbs$state == "warmed"])

# histograms for each year
hist(green_kbs$half_cover_date[green_kbs$year == 1])
hist(green_kbs$half_cover_date[green_kbs$year == 2])
hist(green_kbs$half_cover_date[green_kbs$year == 3])
hist(green_kbs$half_cover_date[green_kbs$year == 4])
hist(green_kbs$half_cover_date[green_kbs$year == 5])
hist(green_kbs$half_cover_date[green_kbs$year == 6])
# looks like the 225 spike is from 2018 and 2020 
kbs_2018 <- subset(green_kbs, year == 4) # many records on 235
kbs_2020 <- subset(green_kbs, year == 6) # records from 227 & 228


# trying data transformations
# mean centering half_cover_date
green_kbs$date_scaled <- scale(green_kbs$half_cover_date, scale = F)
hist(green_kbs$date_scaled)
hist(green_kbs$date_scaled[green_kbs$state == "ambient"])
hist(green_kbs$date_scaled[green_kbs$state == "warmed"])
qqnorm(green_kbs$date_scaled)
shapiro.test(green_kbs$date_scaled)
# still not normal

# log transform 
green_kbs$date_log <- log(green_kbs$half_cover_date)
hist(green_kbs$date_log)
qqnorm(green_kbs$date_log)
shapiro.test(green_kbs$date_log)

# inverse transform 
green_kbs$date_inv <- 1/(green_kbs$half_cover_date)
hist(green_kbs$date_inv)
qqnorm(green_kbs$date_inv)
shapiro.test(green_kbs$date_inv)

# square root transform 
green_kbs$date_sqrt <- sqrt(green_kbs$half_cover_date)
hist(green_kbs$date_sqrt)
qqnorm(green_kbs$date_sqrt)
shapiro.test(green_kbs$date_sqrt)

# cubed root transform 
green_kbs$date_cubed <- (green_kbs$half_cover_date)^(1/3)
hist(green_kbs$date_cubed)
qqnorm(green_kbs$date_cubed)
shapiro.test(green_kbs$date_cubed)


##### trying different distributions #######
# found this method through stackoverflow
descdist(green_kbs$half_cover_date, discrete = FALSE) # looks closest to uniform
fit.unif <- fitdist(green_kbs$half_cover_date, "unif")
plot(fit.unif)
# uniform is closest, but isn't included in glmer so I'll go with poisson (which still makes sense)
fit <- lm(half_cover_date~state, data = green_kbs)
residual <- fit$residuals
hist(residual)
pois <- glm(half_cover_date~state, data = green_kbs, family="poisson")
hist(pois$residuals)
# still doesn't look very good
# including parametric & non-parametric models below


###### running analyses ########
## partially taken from kileighs old models - parametric tests ##
# generalized linear models for poisson distribution with:
# state, year and insecticide as fixed (w interaction btwn state and year) & species and plot as random effects
moda <- glmer(half_cover_date ~ state*year + insecticide + (1|species) + (1|plot),
              data=green_kbs, family = poisson)
# state, year and insecticide as separate fixed effects & species and plot as random effects
modb <- glmer(half_cover_date ~ state + year + insecticide + (1|species) + (1|plot),
              data=green_kbs, family = poisson)
# state and insecticide as fixed effects & year, species and plot as random effects
modc <- glmer(half_cover_date ~ state + insecticide + (1|year) + (1|species) + (1|plot),
              data=green_kbs, family = poisson)
anova(moda, modb, modc)
summary(moda)
anova(moda)

## non-parametric ##
friedman_kbs <- green_kbs %>% 
  friedman_test(half_cover_date ~ state)

## permanova 
per1 <- adonis2(green_kbs$half_cover_date ~ state*year + insecticide, data = green_kbs)
per1
per2 <- adonis(formula = green_kbs$half_cover_date ~ state*year + insecticide, strata = green_kbs$plot, data = green_kbs)
per2

# from kileigh's code
confint(modb, method="boot", nsim=999)
difflsmeans(modb, test.effs=NULL, ddf="Satterthwaite")





########### umbs ##############
#### determining distribution ####
# first, checking normality
hist(green_umbs$half_cover_date)
qqnorm(green_umbs$half_cover_date)
shapiro.test(green_umbs$half_cover_date)
fit2 <- lm(half_cover_date~state, data = green_umbs)
qqPlot(fit2)
hist(green_umbs$half_cover_date[green_kbs$state == "ambient"])
hist(green_umbs$half_cover_date[green_kbs$state == "warmed"])
# when separated by state the histograms are just a bit right skewed

# mean centering half_cover_date
green_umbs$date_scaled <- scale(green_umbs$half_cover_date, scale = F)
hist(green_umbs$date_scaled)
hist(green_umbs$date_scaled[green_kbs$state == "ambient"])
hist(green_umbs$date_scaled[green_kbs$state == "warmed"])
qqnorm(green_umbs$date_scaled)
shapiro.test(green_umbs$date_scaled)
# still not normal

# log transform 
green_umbs$date_log <- log(green_umbs$half_cover_date)
hist(green_umbs$date_log)
qqnorm(green_umbs$date_log)
shapiro.test(green_umbs$date_log)
# this looks pretty good but shapiro wilk test still is below 0.05 (how important is this?)

# inverse transform 
green_umbs$date_inv <- 1/(green_umbs$half_cover_date)
hist(green_umbs$date_inv)
qqnorm(green_umbs$date_inv)
shapiro.test(green_umbs$date_inv)
# also looks better, still below 0.05 for shapiro wilk

# square root transform 
green_umbs$date_sqrt <- sqrt(green_umbs$half_cover_date)
hist(green_umbs$date_sqrt)
qqnorm(green_umbs$date_sqrt)
shapiro.test(green_umbs$date_sqrt)

# cubed root transform 
green_umbs$date_cubed <- (green_umbs$half_cover_date)^(1/3)
hist(green_umbs$date_cubed)
qqnorm(green_umbs$date_cubed)
shapiro.test(green_umbs$date_cubed)


##### trying different distributions ######
descdist(green_umbs$half_cover_date, discrete = FALSE) # maybe gamma?
fit.gamma <- fitdist(green_kbs$half_cover_date, "gamma")
plot(fit.gamma)

# using glm
residual2 <- fit2$residuals
hist(residual2)
gamma <- glm(half_cover_date~state, data = green_umbs, family = "Gamma")
hist(gamma$residuals)


###### running analyses ########
## partially taken from kileighs old models ##
modd <- glmer(half_cover_date ~ state*year + insecticide + (1|species) + (1|plot),
              data=green_umbs, family = poisson)
mode <- glmer(half_cover_date ~ state + year + insecticide + (1|species) + (1|plot),
              data=green_umbs, family = poisson)
modf <- glmer(half_cover_date ~ state + insecticide + (1|year) + (1|species) + (1|plot),
              data=green_umbs, family = poisson)
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
lm1 <- lm(half_cover_date~state*year+insecticide, data = green_kbs)
summary(lm1)

lm2 <- lm(half_cover_date~state*year+insecticide, data = green_umbs)
summary(lm2)

# mixed effects model
lme2 <- lme(half_cover_date~state*year+insecticide, random=~1|species, data = green_kbs)
summary(lme2)
coef(lme2)

lme3 <- lme(half_cover_date~state*year+insecticide, random=~1|species, data = green_umbs)
summary(lme3)
coef(lme3)




# copied from the Rmd so I can extract the plots
mod7 <- lmer(spp_half_cover_date ~ state + year + insecticide + (1|species) + (1+year|plot), green_kbs, REML=FALSE)
anova(mod5, mod7)
summary(mod7)
# Yup, seems to matter but it is making this more complex, though not overly so because it's on the random effects structure only.
plot_model(mod7, sort.est = TRUE)
# these are the fixed predicted values:
plot_model(mod7, type = "pred", terms = c("year", "state", "insecticide"))
# these are the random effects estimates
plot_model(mod7, type = "re", terms = c("species", "plot"))

mod8 <- lmer(spp_half_cover_date ~ state + year + species + (1|plot), green_kbs, REML=FALSE)
anova(mod5, mod8)
summary(mod8)
plot_model(mod8, sort.est = TRUE)
# these are the fixed predicted values:
plot_model(mod8, type = "pred", terms = c("year", "state", "species"))
# these are the random effects estimates
plot_model(mod8, type = "re", terms = c("species"))

