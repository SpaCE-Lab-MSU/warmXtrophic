# TITLE:          warmXtrophic biomass analyses
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
library(stats)
library(knitr)

# Set working directory 
L1_dir <- Sys.getenv("L1DIR")
list.files(L1_dir)

# load data
# note: we also have 2020 ancillary biomass (no warming treatment), so only analyzing warming effect with 2021 harvested biomass
kbs_biomass_21 <- read.csv(file.path(L1_dir, "ANPP/kbs_biomass_2021_L1.csv"))
umbs_biomass_21 <- read.csv(file.path(L1_dir, "ANPP/umbs_biomass_2021_L1.csv"))

# making separate dataframe for biomass - easier in analyses
kbs_biomass_only <- kbs_biomass_21 %>%
        dplyr::select(-cover) %>%
        drop_na(weight_g)
umbs_biomass_only <- umbs_biomass_21 %>%
        dplyr::select(-cover) %>%
        drop_na(weight_g)

# removing uninformative & unwanted species
# Umsp & Lisp = moss and lichen, removing because sample collection also contains dirt/sand
kbs_biomass_live <- kbs_biomass_only[!grepl("Litter", kbs_biomass_only$species),]
kbs_biomass_live <- kbs_biomass_live[!grepl("Umsp", kbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_only[!grepl("Litter", umbs_biomass_only$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Standing_Dead", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Surface_Litter", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Umsp", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Lisp", umbs_biomass_live$species),]

# summarizing data to plot-level
kbs_plot_biomass <- kbs_biomass_live %>%
        group_by(plot, state, insecticide) %>%
        summarize(plot_sum_g = sum(weight_g, na.rm = TRUE))
umbs_plot_biomass <- umbs_biomass_live %>%
        group_by(plot, state, insecticide) %>%
        summarize(plot_sum_g = sum(weight_g, na.rm = TRUE))

# making a dataframe for regression between cover and biomass - removing uninformative species first
# note: this regression not used in manuscript
kbs_biomass_reg <- kbs_biomass_21[!grepl("Litter", kbs_biomass_21$species),]
kbs_biomass_reg <- kbs_biomass_reg[!grepl("Total Live", kbs_biomass_reg$species),]
kbs_biomass_reg <- kbs_biomass_reg[!grepl("Unknown", kbs_biomass_reg$species),]
kbs_biomass_reg <- kbs_biomass_reg[!grepl("Unsorted", kbs_biomass_reg$species),]

umbs_biomass_reg <- umbs_biomass_21[!grepl("Bare_Ground", umbs_biomass_21$species),]
umbs_biomass_reg <- umbs_biomass_reg[!grepl("Groundhog", umbs_biomass_reg$species),]
umbs_biomass_reg <- umbs_biomass_reg[!grepl("Litter", umbs_biomass_reg$species),]
umbs_biomass_reg <- umbs_biomass_reg[!grepl("Standing_Dead", umbs_biomass_reg$species),]
umbs_biomass_reg <- umbs_biomass_reg[!grepl("Surface_Litter", umbs_biomass_reg$species),]

# setting NA to 0 for cover or biomass for the regression
# the 0 shows either a species wasn't seen in cover but weighed, or it was seen in cover but not weighed
kbs_biomass_reg$cover[is.na(kbs_biomass_reg$cover)] <- 0
kbs_biomass_reg$weight_g[is.na(kbs_biomass_reg$weight_g)] <- 0
umbs_biomass_reg$cover[is.na(umbs_biomass_reg$cover)] <- 0
umbs_biomass_reg$weight_g[is.na(umbs_biomass_reg$weight_g)] <- 0

# fixing values listed as <1 for umbs
umbs_biomass_reg[umbs_biomass_reg=="<1"] <- 0
str(umbs_biomass_reg)
str(kbs_biomass_reg)

# making cover column numeric for umbs
umbs_biomass_reg$cover <- as.numeric(umbs_biomass_reg$cover)
str(umbs_biomass_reg)

# keeping species that are found in W and A plots, not just one or the other
# tried all models with live2 dataframe and results seemed the same as with the live dataframe
xtabs(weight_g ~ state + species, data=kbs_biomass_live) # some species have 0 biomass in one treatment
kbs_biomass_live2 <- kbs_biomass_live %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
xtabs(weight_g ~ state + species, data=kbs_biomass_live2)

xtabs(weight_g ~ state + species, data=umbs_biomass_live) # some species have 0 biomass in one treatment
umbs_biomass_live2 <- umbs_biomass_live %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
xtabs(weight_g ~ state + species, data=umbs_biomass_live2)



#########################################
# KBS

#########################################

### Data exploration ###
hist(kbs_biomass_only$weight_g)
qqnorm(kbs_biomass_only$weight_g)
shapiro.test(kbs_biomass_only$weight_g)
# very right skewed

# Data exploration - cleaned up dataframe
hist(kbs_biomass_live$weight_g)
qqnorm(kbs_biomass_live$weight_g)
shapiro.test(kbs_biomass_live$weight_g)
# very right skewed, looks about the same as above

# histograms for each treatment separately - plot level
hist(kbs_biomass_live$weight_g[kbs_biomass_only$state == "ambient"])
hist(kbs_biomass_live$weight_g[kbs_biomass_only$state == "warmed"])

# checking individual species
hist(kbs_biomass_live$weight_g[kbs_biomass_live2$species == "Elre"])
hist(kbs_biomass_live$weight_g[kbs_biomass_live2$species == "Soca"])
hist(kbs_biomass_live$weight_g[kbs_biomass_live2$species == "Popr"])
# still kinda skewed for some of these

# histograms for each species
ggplot(data = kbs_biomass_live, aes(x = weight_g, fill=state)) + 
        geom_histogram(alpha=0.5, binwidth=10) + 
        scale_fill_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        facet_wrap(~species)

# leverage plots
fit_k <- lm(weight_g ~ state, data = kbs_biomass_live)
outlierTest(fit_k) # three outliers, all Soca
qqPlot(fit_k, main="QQ Plot") 
hist(fit_k$residuals)
leveragePlots(fit_k)

fit1_k <- lm(weight_g ~ state + species, data = kbs_biomass_live)
outlierTest(fit1_k) # three outliers, all Soca
hist(fit1_k$residuals)
qqPlot(fit1_k, main="QQ Plot") 
leveragePlots(fit1_k)

fit2_k <- lm(log(weight_g) ~ state + species, data = kbs_biomass_live)
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
plot(fit2_k)
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(fit2_k) ~ kbs_biomass_live$state)
leveneTest(residuals(fit2_k) ~ kbs_biomass_live$species)
# Assumption not met for species - ignoring for now
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals - did this above


### Data analyses ###
# plot-level analyses
mod2_kp <- lm(plot_sum_g ~ state*insecticide, data=kbs_plot_biomass)

outlierTest(mod2_kp) # no outliers
hist(mod2_kp$residuals)
qqPlot(mod2_kp, main="QQ Plot") 
leveragePlots(mod2_kp)
leveneTest(residuals(mod2_kp) ~ kbs_plot_biomass$state)
leveneTest(residuals(mod2_kp) ~ kbs_plot_biomass$insecticide)
shapiro.test(resid(mod2_kp))
# looks good
anova(mod2_kp) ### used in manuscript ###
summary(mod2_kp)
kable(anova(mod2_kp)) %>% kableExtra::kable_styling()

# comparisons
emmeans(mod2_kp, list(pairwise ~ state*insecticide), adjust = "tukey")
mod2kp.emm <- emmeans(mod2_kp, ~ state*insecticide)
contrast(mod2kp.emm, "pairwise", simple = "each", combine = F, adjust = "mvt")

# for supp
modk_supp <- lm(log(weight_g) ~ state * insecticide + species, kbs_biomass_live)
anova(modk_supp)
kable(anova(modk_supp)) %>% kableExtra::kable_styling()

# regression #
### not used in manuscript ###
cor.test(kbs_biomass_reg$cover, kbs_biomass_reg$weight_g, method="pearson")
lm1 <- lm(cover ~ weight_g, data = kbs_biomass_reg)
plot(cover ~ weight_g, data = kbs_biomass_reg)
abline(lm1)
summary(lm1)


#########################################
# UMBS
#########################################

### Data exploration ###
hist(umbs_biomass_only$weight_g)
qqnorm(umbs_biomass_only$weight_g)
shapiro.test(umbs_biomass_only$weight_g)
# very right skewed

# Data exploration - cleaned up dataframe
hist(umbs_biomass_live$weight_g)
qqnorm(umbs_biomass_live$weight_g)
shapiro.test(umbs_biomass_live$weight_g)
# very right skewed, looks about the same as above

# histograms for each treatment separately - plot level
hist(umbs_biomass_live$weight_g[umbs_biomass_only$state == "ambient"])
hist(umbs_biomass_live$weight_g[umbs_biomass_only$state == "warmed"])

# histograms for each species
ggplot(data = umbs_biomass_live, aes(x = weight_g, fill=state)) + 
        geom_histogram(alpha=0.5, binwidth=10) + 
        scale_fill_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        facet_wrap(~species)

# leverage plots
fit_u <- lm(weight_g ~ state, data = umbs_biomass_live)
outlierTest(fit_u) # one outlier
qqPlot(fit_u, main="QQ Plot") 
hist(fit_u$residuals)
leveragePlots(fit_u)

fit1_u <- lm(weight_g ~ state + species, data = umbs_biomass_live)
outlierTest(fit1_u) # no outliers
hist(fit1_u$residuals)
qqPlot(fit1_u, main="QQ Plot") # not bad
leveragePlots(fit1_u)
shapiro.test(resid(fit1_u))

fit2_u <- lm(log(weight_g) ~ state + species, data = umbs_biomass_live)
outlierTest(fit2_u) # no outliers
hist(fit2_u$residuals)
qqPlot(fit2_u, main="QQ Plot") 
leveragePlots(fit2_u)
shapiro.test(resid(fit2_u))
# log transformation looks good

# Assumption checking - log transformation
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(fit2_u)
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(fit2_u) ~ umbs_biomass_live$state)
leveneTest(residuals(fit2_u) ~ umbs_biomass_live$species)
# Assumption not met for species and state - ignoring for now
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals - did this above



### Data analyses ### 
# plot-level analyses #
mod2_up <- lm(plot_sum_g ~ state * insecticide, data=umbs_plot_biomass)

outlierTest(mod2_up) # no outliers
hist(mod2_up$residuals)
qqPlot(mod2_up, main="QQ Plot") 
leveragePlots(mod2_up)
leveneTest(residuals(mod2_up) ~ umbs_plot_biomass$state)
leveneTest(residuals(mod2_up) ~ umbs_plot_biomass$insecticide)
shapiro.test(resid(mod2_up))
# looks good
anova(mod2_up) ### used in manuscript ###
summary(mod2_up)
kable(anova(mod2_up)) %>% kableExtra::kable_styling()

# comparisons
emmeans(mod2_up, list(pairwise ~ state*insecticide), adjust = "tukey")
mod2up.emm <- emmeans(mod2_up, ~ state*insecticide)
contrast(mod2up.emm, "pairwise", simple = "each", combine = F, adjust = "mvt")

# for supp
modu_supp <- lm(log(weight_g) ~ state * insecticide + species, umbs_biomass_live)
anova(modu_supp)
kable(anova(modu_supp)) %>% kableExtra::kable_styling()

# regression #
### not used in manuscript ###
cor.test(umbs_biomass_reg$cover, umbs_biomass_reg$weight_g, method="pearson")
lm1 <- lm(cover ~ weight_g, data = umbs_biomass_reg)
plot(cover ~ weight_g, data = umbs_biomass_reg)
abline(lm1)
summary(lm1)







######## old code - not used in manuscript ########
# KBS
# models with random effect - species included here #
### not used in manuscript, except to see species effect ###
mod1_k <- lmer(log(weight_g) ~ state + species + insecticide + (1|plot), kbs_biomass_live, REML=FALSE)
mod2_k <- lmer(log(weight_g) ~ state * species + insecticide + (1|plot), kbs_biomass_live, REML=FALSE)
mod3_k <- lmer(log(weight_g) ~ state + species + (1|plot), kbs_biomass_live, REML=FALSE)
mod4_k <- lmer(log(weight_g) ~ state * species + (1|plot), kbs_biomass_live, REML=FALSE)
mod5_k <- lmer(log(weight_g) ~ state + (1|plot/species), kbs_biomass_live, REML=FALSE)
mod6_k <- lmer(log(weight_g) ~ state + insecticide + species + (1|plot), kbs_biomass_live, REML=FALSE)
mod7_k <- lmer(log(weight_g) ~ state * insecticide + species + (1|plot), kbs_biomass_live, REML=FALSE)
mod8_k <- lmer(log(weight_g) ~ state + (1|plot) + (1|species), kbs_biomass_live, REML=FALSE)
mod9_k <- lmer(log(weight_g) ~ state + (1|species), kbs_biomass_live, REML=FALSE)

# comparing models from above
anova(mod1_k,mod2_k) # mod 2
anova(mod2_k,mod3_k) # mod 2
anova(mod2_k,mod4_k) # mod 4
anova(mod4_k,mod5_k) # mod 4
anova(mod4_k,mod6_k) # mod 4
anova(mod4_k,mod7_k) # mod 4
anova(mod4_k,mod8_k) # mod 4
anova(mod4_k,mod9_k) # mod 4
AICctab(mod1_k,mod2_k,mod3_k,mod4_k,mod5_k,mod6_k,mod7_k,mod8_k,mod9_k,weights=T)
# based on these comparisons, mod2 and mod4 both seem good (mod4 better). will look at model w/ insecticide to see if theres a treatment effect

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

# UMBS
### Data analyses ###
# data seems normal after accounting for variation in species
mod1_u <- lmer(log(weight_g) ~ state + species + insecticide + (1|plot), umbs_biomass_live, REML=FALSE)
mod2_u <- lmer(log(weight_g) ~ state * species + insecticide + (1|plot), umbs_biomass_live, REML=FALSE)
mod3_u <- lmer(log(weight_g) ~ state + species + (1|plot), umbs_biomass_live, REML=FALSE)
mod4_u <- lmer(log(weight_g) ~ state * species + (1|plot), umbs_biomass_live, REML=FALSE)
mod5_u <- lmer(log(weight_g) ~ state + (1|plot/species), umbs_biomass_live, REML=FALSE)
mod6_u <- lmer(log(weight_g) ~ state + insecticide + species + (1|plot), umbs_biomass_live, REML=FALSE)
mod7_u <- lmer(log(weight_g) ~ state * insecticide + species + (1|plot), umbs_biomass_live, REML=FALSE)
mod8_u <- lmer(log(weight_g) ~ state + (1|plot) + (1|species), umbs_biomass_live, REML=FALSE)

# comparing models from above
anova(mod1_u,mod2_u) # mod 2
anova(mod2_u,mod3_u) # mod 2
anova(mod2_u,mod4_u) # mod 4
anova(mod3_u,mod4_u) # mod 4
anova(mod4_u,mod6_u) # mod 4
anova(mod4_u,mod7_u) # mod 4
anova(mod4_u,mod8_u) # mod 4
AICctab(mod1_u,mod2_u,mod3_u,mod4_u,mod6_u,mod7_u,mod8_u,weights=T)
# mod 4 or mod 3?

# need to check this - doesn't look right
# mod3
plot_model(mod3_u, show.values=TRUE, show.p=TRUE)
tab_model(mod3_u)
summary(mod3_u)
anova(mod3_u)
emmip(mod3_u, state ~ species) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_minimal()
# code below pulls out state-species comparisons
mod3u.emm <- emmeans(mod3_u, ~ state + species)
contrast(mod3u.emm, "consec", simple = "each", combine = F, adjust = "mvt")

#mod4
plot_model(mod4_u, show.values=TRUE, show.p=TRUE)
tab_model(mod4_u)
summary(mod4_u)
anova(mod4_u)
emmip(mod4_u, state ~ species) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_minimal()
# code below pulls out state-species comparisons
mod4u.emm <- emmeans(mod4_u, ~ state * species)
contrast(mod4u.emm, "consec", simple = "each", combine = F, adjust = "mvt")

################################################################################

# 2025 data analysis
kbs_biomass <- read.csv(file.path(L1_dir, "ANPP/kbs_biomass_L1.csv"))
umbs_biomass <- read.csv(file.path(L1_dir, "ANPP/umbs_biomass_L1.csv"))

# making separate dataframe for biomass - easier in analyses
kbs_biomass <- kbs_biomass %>%
        dplyr::select(-cover) %>%
        drop_na(weight_g)
umbs_biomass <- umbs_biomass %>%
        dplyr::select(-cover) %>%
        drop_na(weight_g)

kbs_biomass_25 <- kbs_biomass %>% dplyr::filter(year == "2025")
umbs_biomass_25 <- umbs_biomass %>% dplyr::filter(year == "2025")

# remove uninformative species
# kbs
kbs_biomass_live <- kbs_biomass[!grepl("Litter", kbs_biomass$species),]
kbs_biomass_live <- kbs_biomass_live[!grepl("Umsp", kbs_biomass_live$species),]
kbs_biomass_live <- kbs_biomass_live[!grepl("Surfl", kbs_biomass_live$species),]
kbs_biomass_live <- kbs_biomass_live[!grepl("Vert_litter", kbs_biomass_live$species),]
# umbs
umbs_biomass_live <- umbs_biomass[!grepl("Litter", umbs_biomass$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Standing_Dead", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Surface_Litter", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Lisp", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Umsp", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Vert_litter", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Surfl", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Moss_lichen", umbs_biomass_live$species),]

# kbs

descdist(kbs_biomass_live$weight_g, discrete = FALSE)
# Shapiro-Wilk test for normality
shapiro.test(kbs_biomass_live$weight_g) # not normal
# Visualization
hist(kbs_biomass_live$weight_g, main = "Histogram of Biomass")
qqnorm(kbs_biomass_live$weight_g)
qqline(kbs_biomass_live$weight_g, col = "blue")

# Log transform
kbs_biomass_live$weight_g_log <- log(kbs_biomass_live$weight_g)
hist(kbs_biomass_live$weight_g_log, main = "Histogram of Biomass")
qqnorm(kbs_biomass_live$weight_g_log)
qqline(kbs_biomass_live$weight_g_log, col = "blue")
shapiro.test(kbs_biomass_live$weight_g_log)
# log looks better

# sqrt transform
kbs_biomass_live$weight_g_sqrt <- sqrt(kbs_biomass_live$weight_g)
hist(kbs_biomass_live$weight_g_sqrt, main = "Histogram of Biomass")
qqnorm(kbs_biomass_live$weight_g_sqrt)
qqline(kbs_biomass_live$weight_g_sqrt, col = "blue")
shapiro.test(kbs_biomass_live$weight_g_sqrt)
# log looks the best

m1_kb <- lmer(log(weight_g) ~ state + (1|plot), data = kbs_biomass_live)

# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1_kb, main = "Biomass")
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1_kb) ~ kbs_biomass_live$state) # met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1_kb), main = "Biomass")
hist(residuals(m1_kb), main = "Biomass")
shapiro.test(resid(m1_kb))
outlierTest(m1_kb) # checking for outliers - none

anova(m1_kb)
#Type III Analysis of Variance Table with Satterthwaite's method
#        Sum Sq  Mean Sq NumDF  DenDF F value Pr(>F)
#state 0.021314 0.021314     1 18.779  0.0041 0.9498

# umbs

descdist(umbs_biomass_live$weight_g, discrete = FALSE)
# Shapiro-Wilk test for normality
shapiro.test(umbs_biomass_live$weight_g) # not normal
# Visualization
hist(umbs_biomass_live$weight_g, main = "Histogram of Biomass")
qqnorm(umbs_biomass_live$weight_g)
qqline(umbs_biomass_live$weight_g, col = "blue")

# Log transform
umbs_biomass_live$weight_g_log <- log(umbs_biomass_live$weight_g)
hist(umbs_biomass_live$weight_g_log, main = "Histogram of Biomass")
qqnorm(umbs_biomass_live$weight_g_log)
qqline(umbs_biomass_live$weight_g_log, col = "blue")
shapiro.test(umbs_biomass_live$weight_g_log)
# log looks better

# sqrt transform
umbs_biomass_live$weight_g_sqrt <- sqrt(umbs_biomass_live$weight_g)
hist(umbs_biomass_live$weight_g_sqrt, main = "Histogram of Biomass")
qqnorm(umbs_biomass_live$weight_g_sqrt)
qqline(umbs_biomass_live$weight_g_sqrt, col = "blue")
shapiro.test(umbs_biomass_live$weight_g_sqrt)
# log looks the best

m1_ub <- lmer(log(weight_g) ~ state + (1|plot), data = umbs_biomass_live)

# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1_ub, main = "Biomass")
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1_ub) ~ umbs_biomass_live$state) # met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1_ub), main = "Biomass")
hist(residuals(m1_ub), main = "Biomass")
shapiro.test(resid(m1_ub))
outlierTest(m1_ub) # checking for outliers - none

anova(m1_ub)
#Type III Analysis of Variance Table with Satterthwaite's method
#      Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
#state 5.5556  5.5556     1   293  1.4109 0.2359

# litter
kbs_litter <- kbs_biomass_25 %>%
        filter(species %in% c("Vert_litter", "Surfl"))
umbs_litter <- umbs_biomass_25 %>%
        filter(species %in% c("Vert_litter", "Surfl"))

# kbs

descdist(kbs_litter$weight_g, discrete = FALSE)
# Shapiro-Wilk test for normality
shapiro.test(kbs_litter$weight_g) # not normal
# Visualization
hist(kbs_litter$weight_g, main = "Histogram of Biomass")
qqnorm(kbs_litter$weight_g)
qqline(kbs_litter$weight_g, col = "blue")
shapiro.test(kbs_litter$weight_g) # not normal

# Log transform
kbs_litter$weight_g_log <- log(kbs_litter$weight_g)
hist(kbs_litter$weight_g_log, main = "Histogram of Biomass")
qqnorm(kbs_litter$weight_g_log)
qqline(kbs_litter$weight_g_log, col = "blue")
shapiro.test(kbs_litter$weight_g_log) # not normal
# not transformation looks better

# sqrt transform
kbs_litter$weight_g_sqrt <- sqrt(kbs_litter$weight_g)
hist(kbs_litter$weight_g_sqrt, main = "Histogram of Biomass")
qqnorm(kbs_litter$weight_g_sqrt)
qqline(kbs_litter$weight_g_sqrt, col = "blue")
shapiro.test(kbs_litter$weight_g_sqrt)
# sqrt looks the best

m1_kl <- lmer(sqrt(weight_g) ~ state + (1|plot), data = kbs_litter)

# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1_kl, main = "Biomass")
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1_kl) ~ kbs_litter$state) # met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1_kl), main = "Biomass")
hist(residuals(m1_kl), main = "Biomass")
shapiro.test(resid(m1_kl))
outlierTest(m1_kl) # checking for outliers - none

anova(m1_kl)
#Type III Analysis of Variance Table with Satterthwaite's method
#      Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
#state 23.027  23.027     1    37  1.9599 0.1698

# umbs

descdist(umbs_litter$weight_g, discrete = FALSE)
# Shapiro-Wilk test for normality
shapiro.test(umbs_litter$weight_g) # not normal
# Visualization
hist(umbs_litter$weight_g, main = "Histogram of Biomass")
qqnorm(umbs_litter$weight_g)
qqline(umbs_litter$weight_g, col = "blue")

# Log transform
umbs_litter$weight_g_log <- log(umbs_litter$weight_g)
hist(umbs_litter$weight_g_log, main = "Histogram of Biomass")
qqnorm(umbs_litter$weight_g_log)
qqline(umbs_litter$weight_g_log, col = "blue")
shapiro.test(umbs_litter$weight_g_log) # not normal
# not transformation looks better

# sqrt transform
umbs_litter$weight_g_sqrt <- sqrt(umbs_litter$weight_g)
hist(umbs_litter$weight_g_sqrt, main = "Histogram of Biomass")
qqnorm(umbs_litter$weight_g_sqrt)
qqline(umbs_litter$weight_g_sqrt, col = "blue")
shapiro.test(umbs_litter$weight_g_sqrt)
# sqrt looks the best

m1_ul <- lmer(sqrt(weight_g) ~ state + (1|plot), data = umbs_litter)

# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
plot(m1_ul, main = "Biomass")
# Homogeneity of variance looks a bit off (increasing variance in resids does increase with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1_ul) ~ umbs_litter$state) # met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1_ul), main = "Biomass")
hist(residuals(m1_ul), main = "Biomass")
shapiro.test(resid(m1_ul))
outlierTest(m1_ul) # checking for outliers - none

anova(m1_ul)
#Type III Analysis of Variance Table with Satterthwaite's method
#      Sum Sq Mean Sq NumDF DenDF F value Pr(>F)
#state  3.896   3.896     1    31  0.5287 0.4726
