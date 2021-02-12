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
library(lme4)
library(lmerTest)
library(emmeans)
library(vegan)
library(car)
library(rstatix)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

############################### Read in data #################################3
# cleaned phenology data from L1
phen_data <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/phenology/final_flw_sd_L1.csv", stringsAsFactors=FALSE)
phen_data <- phen_data %>% 
        select(-X) # get rid of "X" column that shows up
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
        group_by(plot, year, species, state, site, action, origin, insecticide, year_factor) %>%
        summarize(julian = min(julian, na.rm=T))

##### UMBS #####
# umbs first flower
umbs_flwr_sum <- subset(FirstFlower_all, site == "umbs")

umbs_firstflwr <- umbs_flwr_sum %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, year_factor) %>%
        summarize(julian = min(julian, na.rm=T))

#### Looking at the data
hist(umbs_firstflwr$julian) # poisson?
hist(umbs_flwr$julian)
qqnorm(umbs_firstflwr$julian) # this looks pretty normal to me?
shapiro.test(umbs_firstflwr$julian) # this test says yes to normality 

umbs_pois <- glmer(julian ~ state + (1|species), data = umbs_firstflwr, family = poisson)
summary(umbs_pois)
plot(resid(umbs_pois)) # looks like there's a pattern so probably not poisson

#### Trying out some models

model1 <- lmer(julian ~ state + (1|species), data = umbs_firstflwr, REML=FALSE)
model2 <- lmer(julian ~ state*origin + (1|species) + (1|year_factor), data = umbs_firstflwr, REML=FALSE)
model3 <- lmer(julian ~ state + origin + (1|species), data = umbs_firstflwr, REML=FALSE)

anova(model1, model2, model3)

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


