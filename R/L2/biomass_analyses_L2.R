# TITLE:          warmXtrophic biomass and plant composition canalyses
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    analyses on biomass data
# PROJECT:        warmXtrophic
# DATE:           Feb 2022

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(lmerTest)
library(car)
library(bbmle)
library(sjPlot)
library(emmeans)

# Set working directory 
L1_dir <- Sys.getenv("L1DIR")

# load data
# note: we also have 2020 ancillary biomass (no warming treatment), so only analyzing warming effect with 2021 harvested biomass
kbs_biomass_21 <- read.csv(file.path(L1_dir, "ANPP/kbs_biomass_2021_L1.csv"))
umbs_biomass_21 <- read.csv(file.path(L1_dir, "ANPP/umbs_biomass_2021_L1.csv"))
kbs_biomass_21 <- kbs_biomass_21 %>% dplyr::select(-X) # get rid of "X" column that shows up (could fix this in cleaning script)
umbs_biomass_21 <- umbs_biomass_21 %>% dplyr::select(-X)

# making separate dataframe for biomass - easier in analyses
kbs_biomass_only <- kbs_biomass_21 %>%
        select(-cover) %>%
        drop_na(weight_g)

# removing uninformative species
kbs_biomass_live <- kbs_biomass_only[!grepl("Litter", kbs_biomass_only$species),]

# keeping species that are found in W and A plots, not just one or the other
xtabs(weight_g ~ state + species, data=kbs_biomass_live) # some species have 0 biomass in one treatment
kbs_biomass_live2 <- kbs_biomass_live %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
xtabs(weight_g ~ state + species, data=kbs_biomass_live2)

#########################################
# KBS

#### Data exploration ###
hist(kbs_biomass_only$weight_g)
qqnorm(kbs_biomass_only$weight_g)
shapiro.test(kbs_biomass_only$weight_g)
# very right skewed

#### Data exploration ### - cleaned up dataframe
hist(kbs_biomass_live2$weight_g)
qqnorm(kbs_biomass_live2$weight_g)
shapiro.test(kbs_biomass_live2$weight_g)
# very right skewed, looks about the same as above

# histograms for each treatment separately - plot level
hist(kbs_biomass_live2$weight_g[kbs_biomass_only$state == "ambient"])
hist(kbs_biomass_live2$weight_g[kbs_biomass_only$state == "warmed"])

# checking individual species
hist(kbs_biomass_live2$weight_g[kbs_biomass_live2$species == "Elre"])
hist(kbs_biomass_live2$weight_g[kbs_biomass_live2$species == "Soca"])
hist(kbs_biomass_live2$weight_g[kbs_biomass_live2$species == "Popr"])
# still kinda skewed for some of these

# histograms for each species
ggplot(data = kbs_biomass_live2, aes(x = weight_g, fill=state)) + 
        geom_histogram(alpha=0.5, binwidth=10) + 
        scale_fill_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        facet_wrap(~species)

# density plot
ggplot(data = kbs_biomass_live2, aes(x = weight_g, fill=state)) +
        geom_density(alpha=0.5) +
        scale_fill_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_minimal()

# leverage plots
fit_k <- lm(weight_g ~ state, data = kbs_biomass_live2)
outlierTest(fit_k) # three outliers, all Soca
qqPlot(fit_k, main="QQ Plot") 
hist(fit_k$residuals)
leveragePlots(fit_k)

fit1_k <- lm(weight_g ~ state + species, data = kbs_biomass_live2)
outlierTest(fit1_k) # three outliers, all Soca
hist(fit1_k$residuals)
qqPlot(fit1_k, main="QQ Plot") 
leveragePlots(fit1_k)

fit2_k <- lm(log(weight_g) ~ state + species + insecticide, data = kbs_biomass_live2)
outlierTest(fit2_k) # no outliers
hist(fit2_k$residuals)
qqPlot(fit2_k, main="QQ Plot") 
leveragePlots(fit2_k)
shapiro.test(resid(fit2_k))
# log transformation looks good

# Assumption checking - log transformation
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(fit_2k)
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(fit2_k) ~ kbs_biomass_live2$state)
leveneTest(residuals(fit2_k) ~ kbs_biomass_live2$species)
leveneTest(residuals(fit2_k) ~ kbs_biomass_live2$insecticide)
# Assumption not met for species - ignoring for now
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals - did this above


### Data analyses ###
# data seems normal after accounting for variation in species
mod1_k <- lmer(log(weight_g) ~ state + species + insecticide + (1|plot), kbs_biomass_live2, REML=FALSE)
mod2_k <- lmer(log(weight_g) ~ state * species + insecticide + (1|plot), kbs_biomass_live2, REML=FALSE)
mod3_k <- lmer(log(weight_g) ~ state + species + (1|plot), kbs_biomass_live2, REML=FALSE)
mod4_k <- lmer(log(weight_g) ~ state * species + (1|plot), kbs_biomass_live2, REML=FALSE)
mod5_k <- lmer(log(weight_g) ~ state + (1|plot/species), kbs_biomass_live2, REML=FALSE)
mod6_k <- lmer(log(weight_g) ~ state + insecticide + species + (1|plot), kbs_biomass_live2, REML=FALSE)
mod7_k <- lmer(log(weight_g) ~ state * insecticide + species + (1|plot), kbs_biomass_live2, REML=FALSE)
mod8_k <- lmer(log(weight_g) ~ state + (1|plot) + (1|species), kbs_biomass_live2, REML=FALSE)
mod9_k <- lm(log(weight_g) ~ state, kbs_biomass_live2)

anova(mod1_k,mod2_k) # mod 2
anova(mod2_k,mod3_k) # mod 2
anova(mod2_k,mod4_k) # mod 4
anova(mod4_k,mod5_k) # mod 4
anova(mod4_k,mod6_k) # mod 4
anova(mod4_k,mod7_k) # mod 4
anova(mod4_k,mod8_k) # mod 4
AICctab(mod1_k,mod2_k,mod3_k,mod4_k,mod5_k,mod6_k,mod7_k,mod8_k,weights=T)
# based on these comparisons, mod2 and mod4 both seem good. will look at model w/ insecticide to see if theres a treatment effect

# mod2
plot_model(mod2_k, show.values=TRUE, show.p=TRUE)
tab_model(mod2_k)
summary(mod2_k)
anova(mod2_k)
emmip(mod2_k, state ~ species) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_minimal()
# code below pulls out state-species comparisons
mod2k.emm <- emmeans(mod2_k, ~ state * species)
contrast(mod2k.emm, "consec", simple = "each", combine = F, adjust = "mvt")

#mod4
plot_model(mod4_k, show.values=TRUE, show.p=TRUE)
tab_model(mod4_k)
summary(mod4_k)
anova(mod4_k)
emmip(mod4_k, state ~ species) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_minimal()
# code below pulls out state-species comparisons
mod4k.emm <- emmeans(mod4_k, ~ state * species)
contrast(mod4k.emm, "consec", simple = "each", combine = F, adjust = "mvt")


