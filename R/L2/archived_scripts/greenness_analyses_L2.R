# TITLE:          warmXtrohpic - Greenness
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Moriah Young, Kristin Wolford, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    Analyses results
# PROJECT:        warmXtrophic
# DATE:           July, 2021


######## not used in manuscript #######

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(bbmle)
library(lmerTest)
library(fitdistrplus)
library(sjPlot)
library(car)

# Set working directory
L1_dir<-Sys.getenv("L1DIR")

# Read in data
green <- read.csv(file.path(L1_dir, "Greenness/greenness_L1.csv"))

# Take individual plant average
green2 <- green %>%
        group_by(plot, plant_number, state) %>%
        summarize(greenness = mean(greenness, na.rm = TRUE))

# Data exploration
descdist(green2$greenness, discrete = FALSE)
hist(green2$greenness)
qqnorm(green2$greenness)
shapiro.test(green2$greenness)
# Normal distribution

# Assumption check?
m1 <- lmer(greenness ~ state + (1|plot), data = green2, REML=FALSE)
# Check Assumptions:
# (1) Linearity: if covariates are not categorical
# (2) Homogeneity: Need to Check by plotting residuals vs predicted values.
par(mfrow=c(1,2))
plot(m1, main = "Greenness")
# Homogeneity of variance is ok here (increasing variance in resids is not increasing with fitted values)
# Check for homogeneity of variances (true if p>0.05). If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(m1) ~ green2$state)
# Assumption met
leveneTest(residuals(m1) ~ green2$plot)
# Assumption met
# (3) Normality of error term: need to check by histogram, QQplot of residuals, could do Kolmogorov-Smirnov test.
# Check for normal residuals
qqPlot(resid(m1), main = "Greenness")
hist(residuals(m1), main = "Greenness")
shapiro.test(resid(m1)) # Normal

# Model comparisons
m2 <- lm(greenness ~ state, data=green2)
AIC(m1, m2)
# need plot as random effect
summary(m1)
plot_model(m1, type = "pred", terms = c("state"))

