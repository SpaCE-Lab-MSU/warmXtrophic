# TITLE:          Phenology plots
# AUTHORS:        Kara Dobson, Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared Google drive L1 phenology folders
# DATA OUTPUT:    Plots for phenology for each site
# PROJECT:        warmXtrophic
# DATE:           Clean version created April 2023


# Clear all existing data
rm(list=ls())

# Load packages
library(plotrix)
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(ggpattern)

# Set working directory
#Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
L2_dir<-Sys.getenv("L2DIR")
#list.files(L1_dir)

# Read in data
gr_plot <- read.csv(file.path(L2_dir,"greenup/final_greenup_plot_L2.csv"))
# plot-level green-up V2 (below) takes the median half cover date of all species per plot
# whereas the first plot-level file finds the overall half cover date for the entire plot, regardless of species
# using v2 for plots
gr_plot_v2 <- read.csv(file.path(L2_dir,"greenup/final_greenup_plot_V2_L2.csv"))
gr_species <- read.csv(file.path(L2_dir,"greenup/final_greenup_species_L2.csv"))
gr_growth <- read.csv(file.path(L2_dir,"greenup/final_greenup_growthhabit_L2.csv"))
gr_origin <- read.csv(file.path(L2_dir,"greenup/final_greenup_origin_L2.csv"))

flwr_spp <- read.csv(file.path(L2_dir,"phenology/final_flwr_species_L2.csv"))
flwr_plot <- read.csv(file.path(L2_dir,"phenology/final_flwr_plot_L2.csv"))
sd_spp <- read.csv(file.path(L2_dir, "phenology/final_sd_species_L2.csv"))
sd_plot <- read.csv(file.path(L2_dir, "phenology/final_sd_plot_L2.csv"))

# making site names capital for cleaner plots
change_site <- function(df){
        df$site[df$site == "umbs"] <- "UMBS"
        df$site[df$site == "kbs"] <- "KBS"
        return(df)
}
gr_plot <- change_site(gr_plot)
gr_plot_v2 <- change_site(gr_plot_v2)
sd_plot <- change_site(sd_plot)
flwr_plot <- change_site(flwr_plot)
gr_species <- change_site(gr_species)

# creating a vector with cleaned up insecticide labels for plotting
insect_labels <- c("insects" = "Herbivory", "no_insects" = "Reduced Herbivory")

# note: figures for GDD and temp comparisons are at the end of the script


# filter data to contain the averages and std error for each site & species
sum_green_spp <- gr_species %>%
        group_by(site, state, species) %>%
        summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
                  se = std.error(spp_half_cover_date, na.rm = TRUE))
sum_green_spp_i <- gr_species %>%
        group_by(site, state, insecticide,species) %>%
        summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
                  se = std.error(spp_half_cover_date, na.rm = TRUE))

# averages + std error for plot level data
sum_green_plot <- gr_plot %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
                  se = std.error(plot_half_cover_date, na.rm = TRUE))
sum_green_plot_v2 <- gr_plot_v2 %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(med_half_cover_date, na.rm = TRUE),
                  se = std.error(med_half_cover_date, na.rm = TRUE))
sum_flwr_plot <- flwr_plot %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(julian_median, na.rm = TRUE),
                  se = std.error(julian_median, na.rm = TRUE))
sum_sd_plot <- sd_plot %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(julian_min, na.rm = TRUE),
                  se = std.error(julian_min, na.rm = TRUE))

# averages + std error for plot level data + insecticide treatment
sum_green_plot_i <- gr_plot %>%
        group_by(site, state, insecticide, year) %>%
        summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
                  se = std.error(plot_half_cover_date, na.rm = TRUE))
sum_green_plot_i_v2 <- gr_plot_v2 %>%
        group_by(site, state, insecticide, year) %>%
        summarize(avg_julian = mean(med_half_cover_date, na.rm = TRUE),
                  se = std.error(med_half_cover_date, na.rm = TRUE))
sum_flwr_plot_i <- flwr_plot %>%
        group_by(site, state, insecticide, year) %>%
        summarize(avg_julian = mean(julian_median, na.rm = TRUE),
                  se = std.error(julian_median, na.rm = TRUE))
sum_flwr_plot_i2 <- flwr_plot %>%
        group_by(site, state, insecticide) %>%
        summarize(avg_julian = mean(julian_median, na.rm = TRUE),
                  se = std.error(julian_median, na.rm = TRUE))
sum_sd_plot_i <- sd_plot %>%
        group_by(site, state, insecticide, year) %>%
        summarize(avg_julian = mean(julian_min, na.rm = TRUE),
                  se = std.error(julian_min, na.rm = TRUE))
sum_sd_plot_i2 <- sd_plot %>%
        group_by(site, state, insecticide) %>%
        summarize(avg_julian = mean(julian_min, na.rm = TRUE),
                  se = std.error(julian_min, na.rm = TRUE))

# filter data to contain the averages and std error for each site - half cover
sum_green_site <- gr_species %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
                  se = std.error(spp_half_cover_date, na.rm = TRUE))

# filter data to contain the averages and std error for each site - minimum greenup date
sum_green_site_min <- gr_species %>%
        group_by(site, state, year) %>%
        summarize(avg_julian = mean(min_green_date, na.rm = TRUE),
                  se = std.error(min_green_date, na.rm = TRUE))

# by plant origin (native/exotic)
sum_green_org <- gr_species %>%
        group_by(site, origin, state) %>%
        summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
                  se = std.error(spp_half_cover_date, na.rm = TRUE))
sum_green_org <- subset(sum_green_org, origin == "Exotic" | origin == "Native")

# by plant growth type (forb, graminoid, shrub)
sum_green_habit <- gr_species %>%
        group_by(site, growth_habit, state) %>%
        summarize(avg_julian = mean(spp_half_cover_date, na.rm = TRUE),
                  se = std.error(spp_half_cover_date, na.rm = TRUE))
sum_green_habit <- subset(sum_green_habit, growth_habit == "Forb" | growth_habit == "Graminoid" | growth_habit == "Shrub")

# add empty values for 2015 to kbs greenup so the x-axis matches the other plots
gr_plot2 <- gr_plot
de<-data.frame(NA,"KBS",NA,"2015",NA,"ambient","insects",NA,NA,NA,NA,NA,NA)
names(de)<-c("X","site","plot","year","treatment_key","state","insecticide","plot_half_cover_date","min_green_date","mean_temp","median_temp","max_temp","GDD_cumulative")
gr_plot3 <- rbind(gr_plot2, de)
sum_green_plot2 <- gr_plot3 %>%
        group_by(site, year, state) %>%
        summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
                  se = std.error(plot_half_cover_date, na.rm = TRUE))
sum_green_plot_i2 <- gr_plot3 %>%
        group_by(site, state, insecticide, year) %>%
        summarize(avg_julian = mean(plot_half_cover_date, na.rm = TRUE),
                  se = std.error(plot_half_cover_date, na.rm = TRUE))

gr_plot_v2.2 <- gr_plot_v2
de2<-data.frame(NA,"KBS",NA,"2015","ambient","insects",NA,NA,NA,NA,NA,NA)
names(de2)<-c("X","site","plot","year","state","insecticide","med_half_cover_date","min_green_date","mean_temp","median_temp","max_temp","GDD_cumulative")
gr_plot_v2.3 <- rbind(gr_plot_v2.2, de2)
sum_green_plot2_v2 <- gr_plot_v2.3 %>%
        group_by(site, year, state) %>%
        summarize(avg_julian = mean(med_half_cover_date, na.rm = TRUE),
                  se = std.error(med_half_cover_date, na.rm = TRUE))
sum_green_plot_i2_v2 <- gr_plot_v2.3 %>%
        group_by(site, state, insecticide, year) %>%
        summarize(avg_julian = mean(med_half_cover_date, na.rm = TRUE),
                  se = std.error(med_half_cover_date, na.rm = TRUE))
sum_green_plot_i3_v2 <- gr_plot_v2.3 %>%
        group_by(site, state, insecticide) %>%
        summarize(avg_julian = mean(med_half_cover_date, na.rm = TRUE),
                  se = std.error(med_half_cover_date, na.rm = TRUE))



### line plot with insecticide ###
# greenup
sum_green_plot_i2_v2$year <- as.factor(sum_green_plot_i2_v2$year)
sum_green_plot_i2_v2$full_treat <- paste(sum_green_plot_i2_v2$state, sum_green_plot_i2_v2$insecticide, sep="_")
gr_line_i <- function(loc) { 
        gr_plot <- subset(sum_green_plot_i2_v2, site == loc)
        return(ggplot(gr_plot, aes(x = year, y = avg_julian, group=full_treat, linetype=full_treat, color = full_treat)) +
                       geom_errorbar(aes(ymin=avg_julian-se, ymax=avg_julian+se), color="black",linetype="solid", position=position_dodge(0.1), width=.3) +
                       geom_line(size = 1) +
                       geom_point(size = 2) +
                       scale_color_manual(name="Treatment",
                                          values = c("#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"),
                                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
                       scale_linetype_manual(name="Treatment",
                                             values = c("solid", "dotdash", "solid", "dotdash"),
                                             labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
                       labs(x = NULL, y = "Green-up", title = loc) +
                       #ylim(100,165) +
                       theme_bw(14))
}
gr_line_i_kbs <- gr_line_i("KBS")
gr_line_i_kbs <- gr_line_i_kbs + theme(axis.text.x=element_blank())
gr_line_i_umbs <- gr_line_i("UMBS")
gr_line_i_umbs <- gr_line_i_umbs + labs(y=NULL) + theme(axis.text.x=element_blank(),
                                                        axis.title.y=element_blank())

#flower
sum_flwr_plot_i$year <- as.factor(sum_flwr_plot_i$year)
sum_flwr_plot_i$full_treat <- paste(sum_flwr_plot_i$state, sum_flwr_plot_i$insecticide, sep="_")
flwr_line_i <- function(loc) { 
        flwr_plot <- subset(sum_flwr_plot_i, site == loc)
        return(ggplot(flwr_plot, aes(x = year, y = avg_julian, group=full_treat, linetype=full_treat, color = full_treat)) +
                       geom_errorbar(aes(ymin=avg_julian-se, ymax=avg_julian+se), color="black",linetype="solid",position=position_dodge(0.1), width=.3) +
                       geom_line(size = 1) +
                       geom_point(size = 2) +
                       scale_color_manual(name="Treatment",
                                          values = c("#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"),
                                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
                       scale_linetype_manual(name="Treatment",
                                             values = c("solid", "dotdash", "solid", "dotdash"),
                                             labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
                       labs(x = NULL, y = "Flowering", title = loc) +
                       #ylim(160,215) +
                       theme_bw(14))
}
flwr_line_i_kbs <- flwr_line_i("KBS")
flwr_line_i_kbs <- flwr_line_i_kbs + theme(axis.text.x=element_blank()) + labs(title=NULL)
flwr_line_i_umbs <- flwr_line_i("UMBS")
flwr_line_i_umbs <- flwr_line_i_umbs + labs(y=NULL, title=NULL) + theme(axis.text.x=element_blank(),
                                                                        axis.title.y=element_blank())

#seed
sum_sd_plot_i$year <- as.factor(sum_sd_plot_i$year)
sum_sd_plot_i$full_treat <- paste(sum_sd_plot_i$state, sum_sd_plot_i$insecticide, sep="_")
sd_line_i <- function(loc) { 
        sd_plot <- subset(sum_sd_plot_i, site == loc)
        return(ggplot(sd_plot, aes(x = year, y = avg_julian, group=full_treat, linetype=full_treat, color = full_treat)) +
                       geom_errorbar(aes(ymin=avg_julian-se, ymax=avg_julian+se), color="black",linetype="solid",position=position_dodge(0.1), width=.3) +
                       geom_line(size = 1) +
                       geom_point(size = 2) +
                       scale_color_manual(name="Treatment",
                                          values = c("#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"),
                                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
                       scale_linetype_manual(name="Treatment",
                                             values = c("solid", "dotdash", "solid", "dotdash"),
                                             labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
                       labs(x = NULL, y = "Seed set", title = loc) +
                       #ylim(175,255) +
                       theme_bw(14))
}
sd_line_i_kbs <- sd_line_i("KBS")
sd_line_i_kbs <- sd_line_i_kbs + labs(title=NULL)
sd_line_i_umbs <- sd_line_i("UMBS")
sd_line_i_umbs <- sd_line_i_umbs + labs(y=NULL, title=NULL)+ theme(axis.title.y=element_blank())

phen_line_i <- ggpubr::ggarrange(gr_line_i_kbs, gr_line_i_umbs,
                                 flwr_line_i_kbs, flwr_line_i_umbs,
                                 sd_line_i_kbs, sd_line_i_umbs,
                                 nrow = 3, ncol = 2, common.legend = T, legend="right")
png("greenup_plots_L2_all_phenology_line_insect.png", units="in", width=10, height=8, res=300)
annotate_figure(phen_line_i,
                left = text_grob("Phenological event julian date", color = "black", rot = 90, size=15),
                bottom = text_grob("Year", color = "black", size=15))
dev.off()



#### line plot no insecticide ####
# greenup
sum_green_plot2_v2$year <- as.factor(sum_green_plot2_v2$year)
gr_line <- function(loc) { 
        gr_plot <- subset(sum_green_plot2_v2, site == loc)
        return(ggplot(gr_plot, aes(x = year, y = avg_julian, group = state)) +
                       geom_errorbar(aes(ymin=avg_julian-se, ymax=avg_julian+se), position=position_dodge(0.1), width=.25) +
                       geom_line(aes(color=state), size = 1) +
                       geom_point(aes(color=state), size = 2) +
                       scale_color_manual(values = c("#a6bddb", "#fb6a4a"), labels=c("Ambient","Warmed")) +
                       labs(x = NULL, y = "Green-up", title = loc, color="Treatment") +
                       ylim(100,170) +
                       theme_bw(14))
}
gr_line_kbs <- gr_line("KBS")
gr_line_kbs <- gr_line_kbs + theme(axis.text.x=element_blank())
gr_line_umbs <- gr_line("UMBS")
gr_line_umbs <- gr_line_umbs + labs(y=NULL) + theme(axis.text.x=element_blank(),
                                                    axis.title.y=element_blank(),
                                                    axis.text.y=element_blank())

#flower
sum_flwr_plot <- sum_flwr_plot[!(sum_flwr_plot$site == "UMBS" & sum_flwr_plot$year == "2021" ),] 

sum_flwr_plot$year <- as.factor(sum_flwr_plot$year)
flwr_line <- function(loc) { 
        flwr_plot <- subset(sum_flwr_plot, site == loc)
        return(ggplot(flwr_plot, aes(x = year, y = avg_julian, group = state)) +
                       geom_errorbar(aes(ymin=avg_julian-se, ymax=avg_julian+se), position=position_dodge(0.1), width=.25) +
                       geom_line(aes(color=state), size = 1) +
                       geom_point(aes(color=state), size = 2) +
                       scale_color_manual(values = c("#a6bddb", "#fb6a4a"), labels=c("Ambient","Warmed")) +
                       labs(x = NULL, y = "Flowering", title=loc, color="Treatment") +
                       ylim(165,220) +
                       theme_bw(14))
}
flwr_line_kbs <- flwr_line("KBS")
flwr_line_kbs <- flwr_line_kbs + theme(axis.text.x=element_blank()) + labs(title=NULL)
flwr_line_umbs <- flwr_line("UMBS")
flwr_line_umbs <- flwr_line_umbs + labs(y=NULL, title=NULL) + theme(axis.text.x=element_blank(),
                                                                    axis.title.y=element_blank(),
                                                                    axis.text.y=element_blank())

#seed
sum_sd_plot <- sum_sd_plot[!(sum_sd_plot$site == "UMBS" & sum_sd_plot$year == "2021" ),] 

sum_sd_plot$year <- as.factor(sum_sd_plot$year)
sd_line <- function(loc) { 
        sd_plot <- subset(sum_sd_plot, site == loc)
        return(ggplot(sd_plot, aes(x = year, y = avg_julian, group = state)) +
                       geom_errorbar(aes(ymin=avg_julian-se, ymax=avg_julian+se), position=position_dodge(0.1), width=.25) +
                       geom_line(aes(color=state), size = 1) +
                       geom_point(aes(color=state), size = 2) +
                       scale_color_manual(values = c("#a6bddb", "#fb6a4a"),labels=c("Ambient","Warmed")) +
                       labs(x = NULL, y = "Seed set", title=loc, color="Treatment") +
                       ylim(175,250) +
                       theme_bw(14))
}
sd_line_kbs <- sd_line("KBS")
sd_line_kbs <- sd_line_kbs + labs(title=NULL)
sd_line_umbs <- sd_line("UMBS")
sd_line_umbs <- sd_line_umbs + labs(y=NULL, title=NULL) + theme(axis.title.y=element_blank(),
                                                                axis.text.y=element_blank())

phen_line <- ggpubr::ggarrange(gr_line_kbs, gr_line_umbs,
                               flwr_line_kbs, flwr_line_umbs,
                               sd_line_kbs, sd_line_umbs,
                               nrow = 3, ncol = 2, common.legend = T, legend="bottom")
png("greenup_plots_L2_all_phenology_line.png", units="in", width=9, height=8, res=300)
annotate_figure(phen_line,
                left = text_grob("Phenological event julian date", color = "black", rot = 90, size=15),
                bottom = text_grob("Year", color = "black", size=15))
dev.off()



#### dot plot with insecticide ####
# greenup
sum_green_plot_i3_v2$full_treat <- paste(sum_green_plot_i3_v2$state, sum_green_plot_i3_v2$insecticide, sep="_")
gr_dot_i <- function(loc) { 
        gr_plot <- subset(sum_green_plot_i3_v2, site == loc)
        return(ggplot(gr_plot, aes(x = state, y = avg_julian, fill=insecticide)) +
                       #geom_point(stat = "identity",position=position_dodge(0.2),size=5) +
                       geom_pointrange(aes(ymin=avg_julian-se, ymax=avg_julian+se), ,pch=21,size=1,position=position_dodge(0.3)) +
                       #scale_shape_manual(name="Treatment",
                       #                   values = c(1, 19),
                       #                   labels=c("Herbivory","Reduced Herbivory")) +
                       scale_fill_manual(name="Treatment",
                                         values = c("#FFB451", "#0b0055"),
                                         labels=c("Herbivory","Reduced Herbivory")) +
                       labs(x = NULL, y = "Green-up", title = loc) +
                       scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
                       coord_cartesian(ylim=c(110,145)) +
                       theme_bw(14))
}
gr_dot_i_kbs <- gr_dot_i("KBS")
gr_dot_i_kbs <- gr_dot_i_kbs + theme(axis.text.x = element_blank(),
                                     plot.title = element_text(size=20),
                                     axis.text.y = element_text(size=17),
                                     axis.title.y = element_text(size=17),
                                     legend.text = element_text(size=17),
                                     legend.title = element_text(size=17))
gr_dot_i_umbs <- gr_dot_i("UMBS")
gr_dot_i_umbs <- gr_dot_i_umbs + labs(y=NULL) + theme(axis.title.y=element_blank(),
                                                      plot.title = element_text(size=20),
                                                      axis.text.y = element_blank(),
                                                      axis.text.x = element_blank(),
                                                      legend.text = element_text(size=17),
                                                      legend.title = element_text(size=17))
#flower
sum_flwr_plot_i2$full_treat <- paste(sum_flwr_plot_i2$state, sum_flwr_plot_i2$insecticide, sep="_")
flwr_dot_i <- function(loc) { 
        flwr_plot <- subset(sum_flwr_plot_i2, site == loc)
        return(ggplot(flwr_plot, aes(x = state, y = avg_julian, fill = insecticide)) +
                       #geom_point(stat = "identity",position=position_dodge(0.2),size=5) +
                       geom_pointrange(aes(ymin=avg_julian-se, ymax=avg_julian+se), pch=21,size=1,position=position_dodge(0.3)) +
                       #scale_shape_manual(name="Treatment",
                       #                   values = c(1, 19),
                       #                   labels=c("Herbivory","Reduced Herbivory")) +
                       scale_fill_manual(name="Treatment",
                                         values = c("#FFB451", "#0b0055"),
                                         labels=c("Herbivory","Reduced Herbivory")) +
                       labs(x = NULL, y = "Flowering", title = loc) +
                       scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
                       coord_cartesian(ylim=c(175,195)) +
                       theme_bw(14))
}
flwr_dot_i_kbs <- flwr_dot_i("KBS")
flwr_dot_i_kbs <- flwr_dot_i_kbs + labs(title=NULL) + theme(axis.text.x = element_blank(),
                                                            axis.text.y = element_text(size=17),
                                                            axis.title.y = element_text(size=17),
                                                            legend.text = element_text(size=17),
                                                            legend.title = element_text(size=17))
flwr_dot_i_umbs <- flwr_dot_i("UMBS")
flwr_dot_i_umbs <- flwr_dot_i_umbs + labs(y=NULL, title=NULL) + theme(axis.title.y=element_blank(),
                                                                      axis.text.y = element_blank(),
                                                                      axis.text.x = element_blank(),
                                                                      legend.text = element_text(size=17),
                                                                      legend.title = element_text(size=17))

#seed
sum_sd_plot_i2$full_treat <- paste(sum_sd_plot_i2$state, sum_sd_plot_i2$insecticide, sep="_")
sd_dot_i <- function(loc) { 
        sd_plot <- subset(sum_sd_plot_i2, site == loc)
        return(ggplot(sd_plot, aes(x = state, y = avg_julian, fill = insecticide)) +
                       #geom_point(stat = "identity",position=position_dodge(0.2),size=5) +
                       geom_pointrange(aes(ymin=avg_julian-se, ymax=avg_julian+se), pch=21,size=1,position=position_dodge(0.3)) +
                       #scale_shape_manual(name="Treatment",
                       #                   values = c(1, 19),
                       #                   labels=c("Herbivory","Reduced Herbivory")) +
                       scale_fill_manual(name="Treatment",
                                         values = c("#FFB451", "#0b0055"),
                                         labels=c("Herbivory","Reduced Herbivory")) +
                       labs(x = NULL, y = "Seed set", title = loc) +
                       scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
                       coord_cartesian(ylim=c(180,215)) +
                       theme_bw(14))
}
sd_dot_i_kbs <- sd_dot_i("KBS")
sd_dot_i_kbs <- sd_dot_i_kbs + labs(title=NULL) + theme(axis.text.y = element_text(size=17),
                                                        axis.text.x = element_text(size=17),
                                                        axis.title.y = element_text(size=17),
                                                        legend.text = element_text(size=17),
                                                        legend.title = element_text(size=17))
sd_dot_i_umbs <- sd_dot_i("UMBS")
sd_dot_i_umbs <- sd_dot_i_umbs + labs(y=NULL, title=NULL) + theme(axis.title.y=element_blank(),
                                                                  axis.text.y = element_blank(),
                                                                  axis.text.x = element_text(size=17),
                                                                  legend.text = element_text(size=17),
                                                                  legend.title = element_text(size=17))

phen_dot_i <- ggpubr::ggarrange(gr_dot_i_kbs, gr_dot_i_umbs,
                                flwr_dot_i_kbs, flwr_dot_i_umbs,
                                sd_dot_i_kbs, sd_dot_i_umbs,
                                nrow = 3, ncol = 2, common.legend = T, legend="right",
                                widths = c(1.1, 0.9,1.1,0.9,1.1,0.9))
png("greenup_plots_L2_all_phenology_dot_insect.png", units="in", width=10, height=8, res=300)
annotate_figure(phen_dot_i,
                left = text_grob("Phenological event julian date", color = "black", rot = 90, size=17))
dev.off()



#### GDD and temp comparisons ####
green_kbsp <- subset(gr_plot_v2, site == "KBS")
green_umbsp <- subset(gr_plot_v2, site == 'UMBS')

# looking to see which temp variable best correlates with greenup dates
# note: when testing this for min dates, there are less data points because all plots had the same 
# min date of greenup per site, per year, whereas the plots had different half cover dates
kbs_gdd <- ggplot(green_kbsp, aes(x = GDD_cumulative, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Cumulative growing degree days (GDD)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_gdd <- ggplot(green_umbsp, aes(x = GDD_cumulative, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Cumulative growing degree days (GDD)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
kbs_mean <- ggplot(green_kbsp, aes(x = mean_temp, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Mean temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_mean <- ggplot(green_umbsp, aes(x = mean_temp, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Mean temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
kbs_med <- ggplot(green_kbsp, aes(x = median_temp, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Median temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_med <- ggplot(green_umbsp, aes(x = median_temp, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Median temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
kbs_max <- ggplot(green_kbsp, aes(x = max_temp, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Max temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_max <- ggplot(green_umbsp, aes(x = max_temp, y = med_half_cover_date, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Max temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#a6bddb", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))

gdd_temp_merge <- ggpubr::ggarrange(kbs_gdd,umbs_gdd,
                                    kbs_mean,umbs_mean,
                                    kbs_med,umbs_med,
                                    kbs_max,umbs_max,
                                    ncol = 2, nrow=4,common.legend=T,legend="bottom")
png("greenup_gdd_temp.png", units="in", width=7, height=8, res=300)
annotate_figure(gdd_temp_merge,
                left = text_grob("Green-up half cover julian date", color = "black", rot = 90, size=15),
                top = text_grob("KBS                                                      UMBS", color = "black", size=15))
dev.off()