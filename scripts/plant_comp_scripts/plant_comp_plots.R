# TITLE:          Plant comp plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    Plots for plant comp data for each species and per site
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(plotrix)
library(ggpubr)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in plant comp data and meta-data
comp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
meta <- read.csv("L0/plot.csv")
str(comp)

# remove uneeded X column and species
comp$X <- NULL
comp$species[comp$species == "Bare_Ground"] <- NA
comp$species[comp$species == "Brown"] <- NA
comp <- na.omit(comp)

# getting relative % cover for comparisions between native & exotic #
# average sub-quadrats for plots
comp_org <- subset(comp, origin == "Exotic" | origin == "Native")
quad.mn <- aggregate(cover ~ plot*origin*species*year*site, data=comp_org, FUN=mean, na.rm=T)
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"

head(quad.mn)

# convert cover to relative abundance 
# first get summed cover for all plants per plot
cov.sum = aggregate(quad.mn ~ plot*origin*year*site, data=quad.mn, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="quad.mn"]<-"cov.sum"
head(cov.sum)
comp2 <- merge(quad.mn,cov.sum, by=c("plot","origin","year","site"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
comp2$relab <- comp2$quad.mn/comp2$cov.sum
summary(comp2)

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"

# Merge meta-data with plant comp data
comp_rel <- left_join(meta, comp2, by = "plot")




#### plots for percent cover #####
# subset out a date in August (one in late July) for each site to look at biomass w/o repeating measurements
comp_peak <- subset(comp, date == "2015-08-25" | date == "2015-07-28" | # first date is kbs, second is umbs
                      date == "2016-08-19" | date == "2016-08-13" |
                      date == "2017-08-02" | date == "2017-08-10" |
                      date == "2018-08-23" | date == "2018-08-09" |
                      date == "2019-08-12" | date == "2019-08-14" |
                      date == "2020-08-15" | date == "2020-08-04")

# filter data to contain the averages and std error for each site
comp_peak$species[comp_peak$species == "Litter"] <- NA
comp_peak$species[comp_peak$species == "Vert_Litter"] <- NA
sum_comp_site <- comp_peak %>%
  group_by(site, state, year) %>%
  summarize(avg_cover = mean(cover, na.rm = TRUE),
            se = std.error(cover, na.rm = TRUE))

# Plot for all species between warmed and ambient
comp_plot_all <- function(loc) { 
  comp_spp <- subset(sum_comp_site, site == loc)
  return(ggplot(comp_spp, aes(x = state, y = avg_cover, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_cover - se, ymax = avg_cover + se), width = 0.2,
                         position = "identity") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
comp_u <- comp_plot_all("umbs")
comp_k <- comp_plot_all("kbs")

final_comp <- ggarrange(comp_k, comp_u, nrow = 2, legend = "none")
annotate_figure(final_comp,
                left = text_grob("Percent Cover", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))


# by plant origin (native/exotic) - no grouping by year
sum_comp_org <- comp_rel %>%
  group_by(site, state, origin) %>%
  summarize(avg_comp = mean(relab, na.rm = TRUE),
            se = std.error(relab, na.rm = TRUE))
sum_comp_org <- subset(sum_comp_org, origin == "Exotic" | origin == "Native")

comp_plot_org <- function(loc) { 
  comp_spp <- subset(sum_comp_org, site == loc)
  return(ggplot(comp_spp, aes(x = origin, y = avg_comp, fill = state)) +
           #facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_comp - se, ymax = avg_comp + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
org_u <- comp_plot_org("umbs")
org_k <- comp_plot_org("kbs")

final_org <- ggarrange(org_k, org_u, ncol = 2, legend = "none")
annotate_figure(final_org,
                left = text_grob("Relative Percent Cover", color = "black", rot = 90),
                bottom = text_grob("Origin", color = "black"))


# by plant origin (native/exotic) - grouped by year
sum_compyear_org <- comp_rel %>%
  group_by(site, state, origin, year) %>%
  summarize(avg_comp = mean(relab, na.rm = TRUE),
            se = std.error(relab, na.rm = TRUE))
sum_compyear_org <- subset(sum_compyear_org, origin == "Exotic" | origin == "Native")

compyear_plot_org <- function(loc) { 
  comp_spp <- subset(sum_compyear_org, site == loc)
  return(ggplot(comp_spp, aes(x = origin, y = avg_comp, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_comp - se, ymax = avg_comp + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = "State", y = "Percent Cover", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("Exotic" = "E", "Native" = "N")) +
           theme_classic())
}
compyear_plot_org("umbs")
compyear_plot_org("kbs")


# by plant growth habit (forb, graminoid, shrub)
sum_comp_habit <- comp %>%
  group_by(site, growth_habit, state) %>%
  summarize(avg_cover = mean(cover, na.rm = TRUE),
            se = std.error(cover, na.rm = TRUE))
sum_comp_habit <- subset(sum_comp_habit, growth_habit == "Forb" | growth_habit == "Graminoid")

comp_plot_habit <- function(loc) { 
  comp_spp <- subset(sum_comp_habit, site == loc)
  return(ggplot(comp_spp, aes(x = growth_habit, y = avg_cover, fill = state)) +
           #facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_cover - se, ymax = avg_cover + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = "State", y = "Percent Cover", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           theme_classic())
}
comp_plot_habit("kbs")
comp_plot_habit("umbs")  



##### plots for relative percent cover #####
# filter data to contain the averages and std error for each site
sum_relcomp_site <- rel_comp %>%
  group_by(site, state, year) %>%
  summarize(avg_cover = mean(relab, na.rm = TRUE),
            se = std.error(relab, na.rm = TRUE))

# Plot for all species between warmed and ambient
relcomp_plot_all <- function(loc) { 
  comp_spp <- subset(sum_relcomp_site, site == loc)
  return(ggplot(comp_spp, aes(x = state, y = avg_cover, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_cover - se, ymax = avg_cover + se), width = 0.2,
                         position = "identity") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
relcomp_u <- relcomp_plot_all("umbs")
relcomp_k <- relcomp_plot_all("kbs")

final_relcomp <- ggarrange(relcomp_k, relcomp_u, nrow = 2, legend = "none")
annotate_figure(final_relcomp,
                left = text_grob("Relative Percent Cover", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))
