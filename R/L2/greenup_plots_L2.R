# TITLE:          Greenup plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    Plots for greenup data for each species and per site
# PROJECT:        warmXtrophic
# DATE:           May 2021

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(plotrix)

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)
greenup <- read.csv(file.path(L1_dir, "greenup/final_greenup_L1.csv"))
str(greenup)

# remove uneeded X column
greenup$X <- NULL


# filter data to contain the averages and std error for each site & species
sum_green_spp <- greenup %>%
  group_by(site, state, species, year) %>%
  summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
            se = std.error(spp_half_cover_date, na.rm = TRUE))

# Function to make a plot for any species
greenup_plot <- function(spp, loc) { 
  greenup_spp <- subset(sum_green_spp, species == spp & site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Julian Day of Greenup", title = spp) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           #coord_cartesian(ylim = c(100, 250)) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
greenup_plot("Popr", "umbs")
greenup_plot("Eugr", "kbs")
greenup_plot("Soca", "kbs")



# filter data to contain the averages and std error for each site (species half cover)
sum_green_site <- greenup %>%
  group_by(site, state, year) %>%
  summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
            se = std.error(spp_half_cover_date, na.rm = TRUE))

# Plot for all species between warmed and ambient
greenup_plot_all <- function(loc) { 
  greenup_spp <- subset(sum_green_site, site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = "identity") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(100, 185)) +
           theme(legend.position = "none") +
           theme_classic())
}
all_u <- greenup_plot_all("umbs")
all_k <- greenup_plot_all("kbs")

final_all <- ggarrange(all_k, all_u, nrow = 2, common.legend = T, legend = "right")
annotate_figure(final_all,
                left = text_grob("Julian Day of Greenup", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))

# boxplot
greenup_plot_box <- function(loc) { 
  greenup_spp <- subset(greenup, site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = half_cover_date, fill = state)) +
           #facet_grid(.~year) +
           geom_boxplot(color = "black") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(0, 300)) +
           theme(legend.position = "none") +
           geom_jitter(shape=16, position=position_jitter(0.2)) +
           theme_classic())
}
all_u <- greenup_plot_box("umbs")
all_k <- greenup_plot_box("kbs")



# filter data to contain the averages and std error for each site (plot half cover)
sum_green_plot <- greenup %>%
  group_by(site, state, year) %>%
  summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
            se = std.error(plot_half_cover_date, na.rm = TRUE))

# Plot for all species between warmed and ambient
greenup_plots <- function(loc) { 
  greenup_spp <- subset(sum_green_plot, site == loc)
  return(ggplot(greenup_spp, aes(x = state, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = "identity") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(100, 200)) +
           theme(legend.position = "none") +
           theme_classic())
}
plot_u <- greenup_plots("umbs")
plot_k <- greenup_plots("kbs")

final_plot <- ggarrange(plot_k, plot_u, nrow = 2, legend = "none")
annotate_figure(final_plot,
                left = text_grob("Julian Day of Greenup", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))



# by plant origin (native/exotic)
sum_green_org <- greenup %>%
  group_by(site, origin, state, year) %>%
  summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
            se = std.error(spp_half_cover_date, na.rm = TRUE))
sum_green_org <- subset(sum_green_org, origin == "Exotic" | origin == "Native")

greenup_plot_org <- function(loc) { 
  greenup_spp <- subset(sum_green_org, site == loc)
  return(ggplot(greenup_spp, aes(x = origin, y = avg_julian, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
          # coord_cartesian(ylim = c(100, 200)) +
           theme_classic())
}
org_u <- greenup_plot_org("umbs")
org_k <- greenup_plot_org("kbs")

final_org <- ggarrange(org_k, org_u, nrow = 2, legend = "none")
annotate_figure(final_org,
                left = text_grob("Julian Day of Greenup", color = "black", rot = 90),
                bottom = text_grob("Origin", color = "black"))

# boxplot
greenup_origin <- subset(greenup, origin == "Exotic" | origin == "Native")
greenup_plot_orgbox <- function(loc) { 
  greenup_spp <- subset(greenup_origin, site == loc)
  return(ggplot(greenup_spp, aes(x = origin, y = half_cover_date, fill = state)) +
           #facet_grid(.~year) +
           geom_boxplot(color = "black") +
           #geom_jitter(shape=16, alpha = 0.2, position=position_jitter(0.4)) +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
org_box_u <- greenup_plot_orgbox("umbs")
org_box_k <- greenup_plot_orgbox("kbs")

final_org_box <- ggarrange(org_box_k, org_box_u, ncol = 2, legend = "none")
annotate_figure(final_org_box,
                left = text_grob("Julian Day of Greenup", color = "black", rot = 90),
                bottom = text_grob("Origin", color = "black"))


# by plant growth type (forb, graminoid, shrub)
sum_green_habit <- greenup %>%
  group_by(site, growth_habit, state) %>%
  summarize(avg_julian = mean(half_cover_date, na.rm = TRUE),
            se = std.error(half_cover_date, na.rm = TRUE))
sum_green_habit <- subset(sum_green_habit, growth_habit == "Forb" | growth_habit == "Graminoid" | growth_habit == "Shrub")

greenup_plot_habit <- function(loc) { 
  greenup_spp <- subset(sum_green_habit, site == loc)
  return(ggplot(greenup_spp, aes(x = growth_habit, y = avg_julian, fill = state)) +
           #facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", color = "black") +
           geom_errorbar(aes(ymin = avg_julian - se, ymax = avg_julian + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = "State", y = "Julian Day of Greenup", title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           coord_cartesian(ylim = c(100, 200)) +
           theme_classic())
}
greenup_plot_habit("umbs")
greenup_plot_habit("kbs")  


# Trying something different - can I compare yearly avg temp to date of greenup?
# Getting yearly average temps for the growing season
KBS_temp <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.csv"))
KBS_temp$Date_Time <- as.Date(KBS_temp$Date_Time)
KBS_temp$month <- format(KBS_temp$Date_Time,format="%m")
KBS_temp$year <- format(KBS_temp$Date_Time,format="%Y")
KBS_temp <- KBS_temp %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        group_by(year) %>%
        summarize(avg_temp = mean(Temp_F_XP_air_1m, na.rm=T))

# Plot of yearly avg temps        
ggplot(KBS_temp, aes(x=year, y=avg_temp)) +
        geom_line() +
        geom_point() +
        theme_classic()

# Plotting KBS yearly avg greenup
greenup_plot_kbs <- subset(greenup, site == "kbs")
greenup_plot_kbs <- greenup_plot_kbs %>%
        group_by(state, year) %>%
        summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
                  se = std.error(plot_half_cover_date, na.rm = TRUE))
ggplot(greenup_plot_kbs, aes(x = year, y = avg_julian, col = state)) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a")) +
        theme_classic()

# UMBS yearly avg greenup
greenup_plot_umbs <- subset(greenup, site == "umbs")
greenup_plot_umbs <- greenup_plot_umbs %>%
        group_by(state, year) %>%
        summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
                  se = std.error(plot_half_cover_date, na.rm = TRUE))
ggplot(greenup_plot_umbs, aes(x = year, y = avg_julian, col = state)) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a")) +
        theme_classic()

