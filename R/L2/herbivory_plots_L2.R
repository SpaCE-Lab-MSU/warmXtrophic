# TITLE:          Herbivory plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 herbivory folder
# DATA OUTPUT:    Plots for herbivory for each species and per site
# PROJECT:        warmXtrophic
# DATE:           May 2021

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)
library(plotrix)
library(ggpubr)

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)
herb <- read.csv(file.path(L1_dir, "herbivory/final_herbivory_L1.csv"))
str(herb) # for some reason, date column converted back to character

# Fix date column & add column for the year and julian day
herb$date <- as.Date(herb$date,format="%Y-%m-%d")
str(herb)



#### Total herb by site and species w/o insecticide treatment####
sum_herb_in <- herb %>%
  group_by(site, state, insecticide, year) %>%
  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
            se = std.error(p_eaten, na.rm = TRUE))
sum_herb_in <- subset(sum_herb_in, insecticide == "insects")

herb_plot_in <- function(loc) { 
  herb_spp <- subset(sum_herb_in, site == loc)
  return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", col = "black") +
           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                         position = "identity") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
herb_u <- herb_plot_in("umbs")
herb_k <- herb_plot_in("kbs")

final_herb <- ggarrange(herb_k, herb_u, nrow = 2, common.legend = T, legend = "right")
annotate_figure(final_herb,
                left = text_grob("Average Percent of Leaf Eaten", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))


### Overall averages btwn treatments - boxplot
herb_overall <- function(loc) { 
        herb_plot <- subset(herb, site == loc)
        return(ggplot(herb_plot, aes(x = state, y = p_eaten, fill=state)) +
                       facet_wrap(~insecticide,nrow=1) +
                       geom_boxplot(color = "black") +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = NULL, "warmed" = NULL)) +
                       ylim(0,10) +
                       theme_classic())
}
herb_overall("kbs")
herb_overall("umbs")


### Overall averages - violin plot
herb_violin <- function(loc) { 
        herb_plot <- subset(herb, site == loc)
        return(ggplot(herb_plot, aes(x = state, y = p_eaten, fill=state)) +
                       facet_wrap(~insecticide,ncol=1) +
                       geom_violin(width=2, size=0.1) +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = NULL, "warmed" = NULL)) +
                       theme_classic() +
                       coord_flip())
}
herb_violin_kbs <- herb_violin("kbs")
herb_violin_umbs <- herb_violin("umbs")
herb_violin <- ggarrange(herb_violin_kbs, herb_violin_umbs,
                         ncol = 2, common.legend = T, legend="right")
#png("herbivory_plots_L2_violin.png", units="in", width=8, height=8, res=300)
annotate_figure(herb_violin,
                left = text_grob("Treatment", color = "black", rot = 90),
                bottom = text_grob("Percent of leaf eaten", color = "black"))
#dev.off()


### Overall average ###
sum_herb_overall <- herb %>%
        group_by(site, state, insecticide) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
sum_herb_overall <- subset(sum_herb_overall, insecticide == "insects")
#png("herbivory_plots_L2_boxplot.png", units="in", width=8, height=8, res=300)
ggplot(sum_herb_overall, aes(x = state, y = avg_eaten, fill = state)) +
        facet_grid(.~site) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = "State", y = "Average Percent of Leaf Eaten") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
        theme_classic()
#dev.off()

### Total herb by origin (native/exotic)
sum_herb_org <- herb %>%
  group_by(site, state, origin, insecticide, year) %>%
  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
            se = std.error(p_eaten, na.rm = TRUE))
sum_herb_org <- subset(sum_herb_org, insecticide == "insects")
sum_herb_org <- subset(sum_herb_org, origin == "Exotic" | origin == "Native")

sum_plot_org <- function(loc) { 
  org_spp <- subset(sum_herb_org, site == loc)
  return(ggplot(org_spp, aes(x = origin, y = avg_eaten, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", col = "black") +
           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
herb_org_u <- sum_plot_org("umbs")
herb_org_k <- sum_plot_org("kbs")
final_herb_org <- ggarrange(herb_org_k, herb_org_u, nrow = 2, legend = "none")
#png("herbivory_plots_L2_origin.png", units="in", width=8, height=8, res=300)
annotate_figure(final_herb_org,
                left = text_grob("Average Percent of Leaf Eaten", color = "black", rot = 90),
                bottom = text_grob("Origin", color = "black"))
#dev.off()


#### Total damage by site and species w/o insecticide treatment####
sum_dam_in <- herb %>%
  group_by(site, state, insecticide, year) %>%
  summarize(avg_dam = mean(p_damage, na.rm = TRUE),
            se = std.error(p_damage, na.rm = TRUE))
sum_dam_in <- subset(sum_dam_in, insecticide == "insects")

dam_plot_in <- function(loc) { 
  dam_spp <- subset(sum_dam_in, site == loc)
  return(ggplot(dam_spp, aes(x = state, y = avg_dam, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity", col = "black") +
           geom_errorbar(aes(ymin = avg_dam - se, ymax = avg_dam + se), width = 0.2,
                         position = "identity") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
dam_u <- dam_plot_in("umbs")
dam_k <- dam_plot_in("kbs")
final_dam <- ggarrange(dam_k, dam_u, nrow = 2, legend = "none")
annotate_figure(final_dam,
                left = text_grob("Average Percent of Leaf Damage", color = "black", rot = 90),
                bottom = text_grob("State", color = "black"))


### Total damage by origin (native/exotic)
sum_dam_org <- herb %>%
  group_by(site, state, origin, insecticide, year) %>%
  summarize(avg_dam = mean(p_damage, na.rm = TRUE),
            se = std.error(p_damage, na.rm = TRUE))
sum_dam_org <- subset(sum_dam_org, insecticide == "insects")
sum_dam_org <- subset(sum_dam_org, origin == "Exotic" | origin == "Native")

dam_plot_org <- function(loc) { 
  org_dam_spp <- subset(sum_dam_org, site == loc)
  return(ggplot(org_dam_spp, aes(x = origin, y = avg_dam, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "dodge", stat = "identity", col = "black") +
           geom_errorbar(aes(ymin = avg_dam - se, ymax = avg_dam + se), width = 0.2,
                         position = position_dodge(0.9)) +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
dam_org_u <- dam_plot_org("umbs")
dam_org_k <- dam_plot_org("kbs")
final_dam_org <- ggarrange(dam_org_k, dam_org_u, nrow = 2, legend = "none")
annotate_figure(final_dam_org,
                left = text_grob("Average Percent of Leaf Damage", color = "black", rot = 90),
                bottom = text_grob("Origin", color = "black"))

# boxplot for the same data
dam_origin <- subset(herb, origin == "Exotic" | origin == "Native")
#dam_origin <- subset(dam_origin, p_damage <= 20)
dam_plot_box <- function(loc) { 
  dam_spp <- subset(dam_origin, site == loc)
  return(ggplot(dam_spp, aes(x = origin, y = p_damage, fill = state)) +
           #facet_grid(.~year) +
           geom_boxplot(color = "black") +
           labs(x = NULL, y = NULL, title = loc) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme(legend.position = "none") +
           #geom_jitter(shape=16, position=position_jitter(0.2)) +
           theme_classic())
}
dam_box_u <- dam_plot_box("umbs")
dam_box_k <- dam_plot_box("kbs")



##### Total herb by site and species #####
sum_herb_spp <- herb %>%
  group_by(site, state, insecticide, species, year) %>%
  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
            se = std.error(p_eaten, na.rm = TRUE))
sum_herb_spp <- subset(sum_herb_spp, insecticide == "insects")

# Function to make a plot for any species
herb_plot_spp <- function(spp, loc) { 
  herb_spp <- subset(sum_herb_spp, species == spp & site == loc)
  return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = state)) +
           facet_grid(.~year) +
           geom_bar(position = "identity", stat = "identity") +
           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                         position = "identity") +
           labs(x = "State", y = "Average Percent of Leaf Eaten", title = spp) +
           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
           theme_classic())
}
herb_plot_spp("Popr", "umbs")
herb_plot_spp("Eugr", "kbs")
herb_plot_spp("Soca", "kbs")

### dont need this one by treatment key
#### Total herb by site and species with separated insecticide and no insecticide####
#sum_herb_in <- herb %>%
#  group_by(site, treatment_key, year) %>%
#  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
#            se = std.error(p_eaten, na.rm = TRUE))
#
#herb_plot_in <- function(loc, key, key2) { 
#  herb_spp <- subset(sum_herb_in, site == loc & treatment_key == key | site== loc & treatment_key == key2)
#  return(ggplot(herb_spp, aes(x = treatment_key, y = avg_eaten, fill = treatment_key)) +
#           facet_grid(.~year) +
#           geom_bar(position = "identity", stat = "identity") +
#           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
#                         position = "identity") +
#           labs(x = "Treatment", y = "Average Percent of Leaf Eaten", title = loc) +
#           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
#           theme_classic())
#}
#herb_plot_in("umbs", "A0", "W0")
#herb_plot_in("kbs", "A0", "W0")
#herb_plot_in("umbs", "AI", "WI")
#herb_plot_in("kbs", "AI", "WI")

### didnt account for insecticide
#### Total herb by site ####
#sum_herb_all <- herb %>%
#  group_by(site, state, year) %>%
#  summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
#            se = std.error(p_eaten, na.rm = TRUE))
#
# Plot for all species between warmed and ambient - doesn't account for insecticide treatment
#herb_plot_all <- function(loc) { 
#  herb_spp <- subset(sum_herb_all, site == loc)
#  return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = state)) +
#           facet_grid(.~year) +
#           geom_bar(position = "identity", stat = "identity") +
#           geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
#                         position = "identity") +
#           labs(x = "State", y = "Average Percent of Leaf Eaten", title = loc) +
#           scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
#           scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
#           theme_classic())
#}
#herb_plot_all("umbs")
#herb_plot_all("kbs")
