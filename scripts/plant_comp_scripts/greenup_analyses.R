# TITLE:          Greenup data analysis
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           Jan 2021




######### very much in progress - looks bad rn ############
# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(olsrr)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data
greenup <- read.csv("L1/greenup/final_greenup_L1.csv")
str(plant_comp)

# create dataframes for kbs and umbs
final_kbs <- subset(greenup, site == "kbs")
final_umbs <- subset(greenup, site == "umbs")



###### statistical analysis #########
# first, checking that residuals are normal
fit <- lm(abs(final_kbs$half_cover_date - mean(final_kbs$half_cover_date))~state*year + insecticide, data = final_kbs)
residual <- fit$residuals
predicted <- fit$fitted.values
shapiro.test(residual)
ols_plot_resid_hist(fit)
ols_plot_resid_qq(fit)
ols_test_normality(fit)
ols_plot_resid_fit(fit)
plot(predicted, residual, main = "Residuals vs. predicted values", las = 1, xlab = "Predicted values", ylab = "Residuals")
hist(residual)
anova(fit)
emmeans(fit, specs = pairwise ~ state*year, type = "response", adjust = "tukey")

idk <- cbind(final_kbs, Residual = resid(fit))
ggplot(idk, aes(x = Residual))+
  geom_histogram(bins = 10) + labs(title = "Residuals")
plot_grid(p1, p2, ncol = 2, align = 'hv', axis = 'lrtb')

transformed <- abs(final_kbs$half_cover_date - mean(final_kbs$half_cover_date))
shapiro.test(transformed)
hist(transformed)

library(ggpubr)
ggqqplot(final_kbs$half_cover_date)
shapiro.test(final_kbs$half_cover_date)

library(moments)
skewness(final_kbs$half_cover_date, na.rm = TRUE)
ggdensity(final_kbs, x = "half_cover_date", fill = "lightgray", title = "CONT") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

## partially taken from kileighs old models ##
# do we need plot as a random effect?
moda <- lmer(half_cover_date ~ state*year + (1|species) + (1|plot), final_kbs)
modb <- lmer(half_cover_date ~ state + insecticide + (1|species), final_kbs)
anova(modb, moda) #yes
summary(moda)
anova(moda)
emmeans(moda, specs = pairwise ~ state + year, type = "response", adjust = "tukey")

#year?
modb2 <- lmer(half_cover_date ~ treatment_key + (1|species) + (1|year), final_kbs)
anova(modb, modb2) # yes
summary(modb2)
anova(modb2)
confint(modb2, method="boot", nsim=999)
difflsmeans(modb2, test.effs=NULL, ddf="Satterthwaite")

# do we need insects?
#modc <- lmer(half_cover_date ~ state + (1|species) + (1|plot), final_kbs, REML=FALSE)
#anova(moda, modc) #yes


##### attempting different models #######
### comparing linear vs mixed effects ###
# linear model
lm1 <- lm(half_cover_date~state*insecticide, data = final_kbs)
summary(lm1)

lm2 <- lm(half_cover_date~state*insecticide, data = final_umbs)
summary(lm2)

# mixed effects model -> after running lme with plot as random effect, may not be needed (plot doesn't affect intercepts in coef)
# below does not give p values from lme4, so I switched to nlme instead
#lme1 <- lmer(julian~state*insecticide + (1 | plot), data=filter_comp)
#summary(lme1)
lme2 <- lme(half_cover_date~state*insecticide, random=~1|year, data = final_kbs)
summary(lme2)
coef(lme2)

lme3 <- lme(half_cover_date~state*insecticide, random=~1|plot, data = final_umbs)
summary(lme3)
coef(lme3)

# note to self: include species as random effect?
# no sig difference between warmed and ambient with or without insecticide
# could be due to the chambers not influencing warming in early spring as much as in summer?




### testing 3-way mixed anova
library(rstatix)
library(ggpubr)

# generate summary stats
final_kbs %>%
  group_by(state, insecticide, year) %>%
  get_summary_stats(half_cover_date, type = "mean_sd")

# boxplot of data
ggboxplot(
  final_kbs, x = "state", y = "half_cover_date",
  color = "year", palette = "jco",
  facet.by = "insecticide", short.panel.labs = FALSE
)

# any extreme outliers? no
outlier <- final_kbs %>%
  group_by(state, insecticide, year) %>%
  identify_outliers(half_cover_date)

normal <- final_kbs %>%
  group_by(state, insecticide, year) %>%
  shapiro_test(half_cover_date)

ggqqplot(final_kbs, "half_cover_date", ggtheme = theme_bw()) +
  facet_grid(state + insecticide ~ year, labeller = "label_both")
