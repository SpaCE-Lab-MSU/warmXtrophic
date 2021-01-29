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
plant_comp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
meta <- read.csv("L0/plot.csv")
str(plant_comp) # for some reason, date column converted back to character

# Fix date column
plant_comp$date <- as.Date(plant_comp$date,format="%Y-%m-%d")
str(plant_comp)

# split the plant_comp dataframe
dataus <- split(x = plant_comp, f = plant_comp[, c("plot", "species","site", "year")])

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){
  x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]
}))

# make into a dataframe
half_cover_dates_df <- data.frame("plot.species.site.year" = names(half_cover_dates),
                                  "half_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

# fix species and plot column
half_cover_dates_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dates_df[["species"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_dates_df[["site"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_dates_df[["year"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
half_cover_dates_df$plot.species.site.year <- NULL

# determine first date of emergence for correlation with 'green-up' index
min_date <- aggregate(plant_comp$julian,by=plant_comp[,c("plot","species")],FUN=min)
colnames(min_date) <- c("plot", "species", "min_emerg_date")

# merge min date dateframe with "green-up index" df
combined <- merge(half_cover_dates_df, min_date, by=c("plot", "species"))

# calculate correlation
cor.test(combined$min_emerg_date, combined$half_cover_date)

# re-merge data with meta data info
final <- left_join(meta, combined, by = "plot")

# remove uneeded columns
final$date <- NULL
final$species.y <- NULL
final$cover <- NULL
final$julian <- NULL
final$X <- NULL
final$site.y <- NULL
final$year.y <- NULL

# fix column names
colnames(final)[which(names(final) == "species.x")] <- "species"
colnames(final)[which(names(final) == "site.x")] <- "site"
colnames(final)[which(names(final) == "year.x")] <- "year"

# create dataframes for kbs and umbs
final_kbs <- subset(final, site == "kbs")
final_umbs <- subset(final, site == "umbs")


###### statistical analysis #########
# first, checking that residuals are normal
fit <- lm(half_cover_date~state*insecticide, data = final_kbs)
residual <- fit$residuals
predicted <- fit$fitted.values
shapiro.test(residual)
ols_plot_resid_hist(fit)
ols_plot_resid_qq(fit)
ols_test_normality(fit)
ols_plot_resid_fit(fit)
plot(predicted, residual, main = "Residuals vs. predicted values", las = 1, xlab = "Predicted values", ylab = "Residuals")
hist(residual) # i think they are? looks pretty good to me


## partially taken from kileighs old models ##
# do we need plot as a random effect?
moda <- lmer(half_cover_date ~ state + insecticide + (1|species) + (1|plot), final_kbs)
modb <- lmer(half_cover_date ~ state + insecticide + (1|species), final_kbs)
anova(modb, moda) #yes
summary(modb)
anova(modb)

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
