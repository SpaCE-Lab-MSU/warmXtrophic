# TITLE:          Phenology Data Analysis
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L1 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           February, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(emmeans)
library(vegan)
library(car)
library(rstatix)
library(scales)
library(fitdistrplus)
library(moments) # for calculating skewness of data

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

############################### Read in data #################################
# cleaned phenology data from L1
phen_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/phenology/final_flw_sd_L1.csv", stringsAsFactors=FALSE)
phen_data$X <- NULL # get rid of "X" column that shows up
View(phen_data) # take a look at the data to see if looks good

# make a column that breaks down years as 1, 2, 3, 4, 5, 6 and into factors
phen_data$year_factor <- 
        ifelse(phen_data$year == 2015, "1",
               ifelse(phen_data$year == 2016, "2",
                      ifelse(phen_data$year == 2017, "3",
                             ifelse(phen_data$year == 2018, "4",
                                    ifelse(phen_data$year == 2019, "5",
                                           ifelse(phen_data$year == 2020, "6", NA))))))

# Create separate data frames for flowering and seeding
phen_flwr <- subset(phen_data, action == "flower")
phen_sd <- subset(phen_data, action == "seed")

# Separate data frame by site for flower
kbs_flwr <- subset(phen_flwr, site == "kbs")
umbs_flwr <- subset(phen_flwr, site == "umbs")

# Separate data frame by site for seed set
kbs_sd <- subset(phen_sd, site == "kbs")
umbs_sd <- subset(phen_sd, site == "umbs")

# HA Plants in warmed plots flower earlier than those in ambient plots
# H0 Plants in warmed and ambient plots flower at the same time

# Filter data to contain the date of first flower for each species at each plot
FirstFlower_all <- phen_flwr %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, year_factor, treatment_key) %>%
        summarize(julian = min(julian, na.rm=T))

MedianFlwr_all <- phen_flwr %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, year_factor, treatment_key) %>%
        summarize(julian = median(julian, na.rm=T))

##### UMBS #####
umbs_firstflwr <- subset(FirstFlower_all, site == "umbs") # pull out only umbs data
# trying to scale the julian dates
umbs_firstflwr <- rescale(umbs_firstflwr$julian, to = c(0, max(umbs_firstflwr$julian)))

# umbs median flower
umbs_flwr_med <- subset(MedianFlwr_all, site == "umbs") # pull out only umbs data

#### Looking at the data
# first flower
hist(umbs_firstflwr$julian) # poisson?
qqnorm(umbs_firstflwr$julian)
fit1 <- lm(julian ~ state, data = umbs_firstflwr)
qqPlot(fit1)
shapiro.test(umbs_firstflwr$julian) 

descdist(umbs_firstflwr$julian, discrete = FALSE) # uniform? normal?

skewness(umbs_firstflwr$julian)

umbs_firstflwr$sqrt_julian <- sqrt(umbs_firstflwr$julian)
hist(umbs_firstflwr$sqrt_julian)
skewness(umbs_firstflwr$sqrt_julian, na.rm = TRUE)

# median flower
hist(umbs_flwr_med$julian)
qqnorm(umbs_flwr_med$julian)
fit2 <- lm(julian ~ state, data = umbs_flwr_med)
qqPlot(fit2)
shapiro.test(umbs_flwr_med$julian)

descdist(umbs_flwr_med$julian, discrete = FALSE) # uniform?

skewness(umbs_flwr_med$julian, na.rm = TRUE)
# [1] 0.3292553 slight positive skew

umbs_flwr_med$sqrt_julian <- sqrt(umbs_flwr_med$julian)
hist(umbs_flwr_med$sqrt_julian)
skewness(umbs_flwr_med$sqrt_julian, na.rm = TRUE)

ggdensity(umbs_flwr_med, x = "sqrt_julian", fill = "lightgray", title = "CONT") +
        stat_overlay_normal_density(color = "red", linetype = "dashed") 
# there's that weird dip around 14 on the x-axis

#### Poisson distribution but this can't be the right model...
pois <- glm(sqrt_julian ~ state, data = umbs_flwr_med, family="poisson")
hist(pois$residuals)

# state, year and insecticide are fixed effects with interaction btw state and year - species and plot are random effects
# poisson dist to fit umbs median flwr
# there's integers in this data so can't use poisson? 
umbsMED_pois <- glmer(julian ~ state*year_factor + insecticide + (1|species) + (1|plot), data = umbs_flwr_med, family = poisson)
summary(umbsMED_pois)
plot(resid(umbsMED_pois)) # looks like there's a pattern so probably not poisson

# poisson dist to fit umbs first flwr 
umbsFF_pois <- glmer(julian ~ state*year_factor + insecticide + (1|species) + (1|plot), data = umbs_firstflwr, family = poisson)
summary(umbsFF_pois)
plot(resid(umbsFF_pois))

#### Normal distribution
umbsMED_norm <- lmer(sqrt_julian ~ state*year_factor + insecticide + (1|species) + (1|plot), data = umbs_flwr_med,
                     REML=FALSE)
summary(umbsMED_norm)

umbsFF_norm <- lmer(sqrt_julian ~ state*year_factor + insecticide + (1|species) + (1|plot), data = umbs_firstflwr,
                    REML=FALSE)
summary(umbsFF_norm)

AIC(umbs_poisFF, umbsFF_norm)
#             df      AIC
#umbs_poisFF 13 4734.759
#umbsFF_norm 14 4673.664  lower AIC, but more degrees of freedom
        
#### Trying out some models

model1 <- lmer(julian ~ state + (1|species), data = umbs_firstflwr, REML=FALSE)

model2 <- lmer(julian ~ state*origin + (1|species) + (1|year_factor), data = umbs_firstflwr, REML=FALSE)

model3 <- lmer(julian ~ state + origin + (1|species), data = umbs_firstflwr, REML=FALSE)

anova(model1, model2, model3)

#### Updated from Kileigh's script
#should plot be a random effect?
moda <- lmer(sqrt_julian ~ state*origin + insecticide + (1|species) + (1|plot), umbs_flwr_med, REML=FALSE)
mod1 <- lmer(sqrt_julian ~ state*origin + insecticide + (1|species), umbs_flwr_med, REML=FALSE)
anova(mod1,moda)
AIC(mod1, moda)

#Do we need insects?
mod2 <- lmer(sqrt_julian ~ state*origin + (1|species), umbs_flwr_med, REML=FALSE)
anova(mod2,mod1)

#Do we need interaction term?
mod3 <- lmer(sqrt_julian ~ state + origin + (1|species), umbs_flwr_med, REML=FALSE)
anova(mod3, mod2)
anova(mod3)
summary(mod3)
confint(mod3, method="boot", nsim=999)
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")

# Friedman test
as.data.frame(umbs_firstflwr)
umbs_fried <- umbs_firstflwr %>% 
        friedman.test(julian ~ state |year)

##### KBS #####

# kbs first flower
kbs_flwr_sum <- subset(FirstFlower_all, site == "kbs")

kbs_firstflwr <- kbs_flwr_sum %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, year_factor) %>%
        summarize(julian = min(julian, na.rm=T))

hist(kbs_firstflwr$julian)
hist(kbs_flwr$julian)
qqnorm(kbs_firstflwr$julian)
shapiro.test(kbs_firstflwr$julian)


