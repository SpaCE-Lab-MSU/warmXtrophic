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

# making site names capital for cleaner plots
change_site <- function(df){
        df$site[df$site == "umbs"] <- "UMBS"
        df$site[df$site == "kbs"] <- "KBS"
        return(df)
}
herb <- change_site(herb)

# clean insecticide labels for plotting
insect_labels <- c("insects" = "Herbivory", "no_insects" = "Reduced Herbivory")


#### Total herb by site and year w/o insecticide treatment####
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
herb_u <- herb_plot_in("UMBS")
herb_k <- herb_plot_in("KBS")

final_herb <- ggarrange(herb_k, herb_u, nrow = 2,legend = "none")
#png("herbivory_plots_L2_yearly_barplot.png", units="in", width=8, height=8, res=300)
annotate_figure(final_herb,
                left = text_grob("Amount of leaf eaten (%)", color = "black", rot = 90),
                bottom = text_grob("Treatment", color = "black"))
#dev.off()

### Overall averages btwn treatments - boxplot
herb_overall <- function(loc) { 
        herb_plot <- subset(herb, site == loc)
        return(ggplot(herb_plot, aes(x = state, y = p_eaten, fill=state)) +
                       facet_wrap(~insecticide, labeller = as_labeller(insect_labels)) +
                       geom_boxplot(outlier.shape=NA, alpha=0.7) +
                       geom_jitter(aes(alpha=0.6, color=state, fill=state), shape=16, size=2) +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_color_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
                       ylim(0,10) +
                       theme_classic())
}
herb_overall_kbs <- herb_overall("KBS")
herb_overall_umbs <- herb_overall("UMBS")
herb_overall_comb <- ggpubr::ggarrange(herb_overall_kbs, herb_overall_umbs,
                                     nrow = 2, common.legend = T, legend="none")
png("herbivory_plots_L2_boxplot_overall.png", units="in", width=8, height=8, res=300)
annotate_figure(herb_overall_comb,
                left = text_grob("Amount of leaf eaten (%)", color = "black", rot = 90),
                bottom = text_grob("Treatment", color = "black"))
dev.off()


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
herb_violin_kbs <- herb_violin("KBS")
herb_violin_umbs <- herb_violin("UMBS")
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
#png("herbivory_plots_L2_barplot.png", units="in", width=8, height=8, res=300)
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


### Overall average - with insecticide ###
sum_herb_overall2 <- herb %>%
        group_by(site, state, insecticide) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
herb_insect_overall <- function(loc) { 
        herb_plot <- subset(sum_herb_overall2, site == loc)
        return(ggplot(herb_plot, aes(x = state, y = avg_eaten, fill = state)) +
        facet_grid(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = NULL, title=loc) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
        theme_classic())
}
herb_insect_overall_umbs <- herb_insect_overall("UMBS")
herb_insect_overall_kbs <- herb_insect_overall("KBS")
herb_insect_overall_comb<- ggarrange(herb_insect_overall_kbs, herb_insect_overall_umbs,
                         nrow = 2, common.legend = T, legend="none")
#png("herbivory_plots_L2_overall.png", units="in", width=8, height=8, res=300)
annotate_figure(herb_insect_overall_comb,
                left = text_grob("Amount of leaf eaten (%)", color = "black", rot = 90),
                bottom = text_grob("Treatment", color = "black"))
#dev.off()

### Overall average - boxplot ###
herb2 <- subset(herb2, insecticide == "insects")
#png("herbivory_plots_L2_boxplot.png", units="in", width=8, height=8, res=300)
ggplot(herb2, aes(x = state, y = p_eaten, fill = state)) +
        facet_grid(.~site) +
        geom_boxplot(col = "black") +
        labs(x = "State", y = "Average Percent of Leaf Eaten") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
        theme_classic()
#dev.off()

### Overall yearly averages - boxplot ###
herb_boxplot <- function(loc) { 
        herb_site <- subset(herb, site == loc)
        return(ggplot(herb_site, aes(x = state, y = p_eaten, fill = state)) +
                       facet_grid(.~year) +
                       geom_boxplot(color = "black") +
                       labs(x = NULL, y = NULL, title = loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       scale_x_discrete(labels=c("ambient" = "A", "warmed" = "W")) +
                       theme(legend.position = "none") +
                       theme_classic())
}


### Binomial response + amount eaten barplot ###
# selecting KBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_k <- herb %>%
        filter(site == "KBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k$p_eaten[herb_binom_k$p_eaten == 1] <- "Eaten"
herb_binom_k$p_eaten[herb_binom_k$p_eaten == 0] <- "Not Eaten"
herb_binom_sumk <- herb_binom_k %>%
        group_by(state, p_eaten) %>%
        count(state, p_eaten) %>%
        group_by(state) %>%
        mutate(n = n/sum(n) * 100)
# selecting UMBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_u <- herb %>%
        filter(site == "UMBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u$p_eaten[herb_binom_u$p_eaten == 1] <- "Eaten"
herb_binom_u$p_eaten[herb_binom_u$p_eaten == 0] <- "Not Eaten"
herb_binom_sumu <- herb_binom_u %>%
        group_by(state, p_eaten) %>%
        count(state, p_eaten) %>%
        group_by(state) %>%
        mutate(n = n/sum(n) * 100)
# plotting binary response
binom_plot_k <- ggplot(herb_binom_sumk, aes(x=state, y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha = p_eaten)) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide = "none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        labs(y="Proportion eaten or not (%)", x=NULL,title="KBS", subtitle="A", fill=NULL) +
        theme_classic() +
        theme(legend.position="none") +
        theme(plot.title = element_text(size = 17),
              plot.subtitle = element_text(size=13),
              axis.text.y = element_text(size=13),
              axis.text.x=element_blank(),
              axis.title.y=element_text(size=15),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14))
binom_plot_u <- ggplot(herb_binom_sumu, aes(x=state, y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha = p_eaten)) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        labs(y=NULL, x=NULL, title="UMBS",subtitle="B", fill=NULL) +
        theme_classic() +
        theme(plot.title = element_text(size = 17),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_blank(),
              axis.text.y = element_blank(),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14))
        

# amount eaten plot
sum_herb_overall_k <- herb %>%
        filter(site == "KBS",
               insecticide == "insects")
sum_herb_overall_k <- sum_herb_overall_k[sum_herb_overall_k$p_eaten != 0, ]
sum_herb_overall_k <- sum_herb_overall_k %>%
        group_by(state) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
sum_herb_overall_u <- herb %>%
        filter(site == "UMBS",
               insecticide == "insects")
sum_herb_overall_u <- sum_herb_overall_u[sum_herb_overall_u$p_eaten != 0, ]
sum_herb_overall_u <- sum_herb_overall_u %>%
        group_by(state) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
eaten_k <- ggplot(sum_herb_overall_k, aes(x = state, y = avg_eaten, fill = state)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL, subtitle="C") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        ylim(0,21) +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.text.y = element_text(size=13),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_text(size=13),
              axis.title.y=element_text(size=15))
eaten_u <- ggplot(sum_herb_overall_u, aes(x = state, y = avg_eaten, fill = state)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = NULL, title=NULL, subtitle="D") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        ylim(0,21) +
        theme_classic() +
        theme(legend.position="none") +
        theme(axis.text.y = element_blank(),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_text(size=13))

# plotting binary response & amount eaten on same figure
binary_overall <- ggarrange(binom_plot_k, binom_plot_u,
                            eaten_k, eaten_u,
                            nrow = 2, ncol = 2, common.legend = T, legend="top")
png("binary_combined_plot.png", units="in", width=8, height=8, res=300)
annotate_figure(binary_overall,
                bottom = text_grob("Treatment", color = "black",size=15))
dev.off()



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
herb_plot_spp("Popr", "UMBS")
herb_plot_spp("Eugr", "KBS")
herb_plot_spp("Soca", "KBS")


#### All species for each site ####
sum_herb_spp2 <- herb[herb$p_eaten != 0, ]
sum_herb_spp2 <- sum_herb_spp2 %>%
        group_by(site, state, insecticide, species) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
sum_herb_spp2 <- subset(sum_herb_spp2, insecticide == "insects")
herb_spp_overall <- function(loc) { 
        herb_spp <- subset(sum_herb_spp2, site == loc)
        return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = state)) +
                       facet_wrap(~species, ncol=4) +
                       geom_bar(position = "identity", stat = "identity", color = "black") +
                       geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                                     position = "identity") +
                       labs(x = NULL, y = NULL, title=loc) +
                       scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
                       #coord_cartesian(ylim = c(100, 250)) +
                       scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
                       theme_bw()) +
                theme(legend.position = "none")
}
kbs_herb_spp <- herb_spp_overall("KBS")
umbs_herb_spp <- herb_spp_overall("UMBS")
herb_overall_merge <- ggpubr::ggarrange(kbs_herb_spp, umbs_herb_spp,
                                         ncol = 2, legend="none")
png("herb_species.png", units="in", width=12, height=8, res=300)
annotate_figure(herb_overall_merge,
                left = text_grob("Amount eaten (%)", color = "black", rot = 90, size=15),
                bottom = text_grob("Treatment", color = "black", size=15))
dev.off()


### All species binomial ###
# selecting KBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_k <- herb %>%
        filter(site == "KBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k$p_eaten[herb_binom_k$p_eaten == 1] <- "Eaten"
herb_binom_k$p_eaten[herb_binom_k$p_eaten == 0] <- "Not Eaten"
herb_binom_sumk2 <- herb_binom_k %>%
        group_by(state, species, p_eaten) %>%
        count(p_eaten) %>%
        group_by(species, state) %>%
        mutate(n = n/sum(n) * 100)
# selecting UMBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_u <- herb %>%
        filter(site == "UMBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u$p_eaten[herb_binom_u$p_eaten == 1] <- "Eaten"
herb_binom_u$p_eaten[herb_binom_u$p_eaten == 0] <- "Not Eaten"
herb_binom_sumu2 <- herb_binom_u %>%
        group_by(state, species, p_eaten) %>%
        count(p_eaten) %>%
        group_by(species, state) %>%
        mutate(n = n/sum(n) * 100)
# plotting binary response
binom_plot_k2 <- ggplot(herb_binom_sumk2, aes(x=state, y=n, fill =state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha=p_eaten)) +
        facet_wrap(~species, ncol=4) +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        labs(y=NULL, x=NULL,title="KBS", fill=NULL) +
        theme_bw() +
        theme(legend.position="none")
binom_plot_u2 <- ggplot(herb_binom_sumu2, aes(x=state, y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha=p_eaten)) +
        facet_wrap(~species, ncol=4) +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        labs(y=NULL, x=NULL, title="UMBS",fill=NULL) +
        theme_bw()
herb_spp_binom_merge <- ggpubr::ggarrange(binom_plot_k2, binom_plot_u2,
                                        ncol = 2, common.legend=T, legend="right")
png("herb_species_binom.png", units="in", width=12, height=8, res=300)
annotate_figure(herb_spp_binom_merge,
                left = text_grob("Proportion eaten or not (%)", color = "black", rot = 90, size=15),
                bottom = text_grob("Treatment", color = "black", size=15))
dev.off()



### Origin - binomial ###
# plotting binary response
# selecting KBS & herbivory plots =, making binary response for if eaten or not overall
herb_org_rem <- herb %>%
        filter(!(origin == 'Both' |
                         origin == ""))
herb_binom_k_org <- herb_org_rem %>%
        filter(site == "KBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k_org$p_eaten[herb_binom_k_org$p_eaten == 1] <- "Eaten"
herb_binom_k_org$p_eaten[herb_binom_k_org$p_eaten == 0] <- "Not Eaten"
herb_binom_k_org2 <- herb_binom_k_org %>%
        group_by(state, origin, p_eaten) %>%
        count(p_eaten) %>%
        group_by(origin, state) %>%
        mutate(n = n/sum(n) * 100)
# selecting UMBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_u_org <- herb_org_rem %>%
        filter(site == "UMBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u_org$p_eaten[herb_binom_u_org$p_eaten == 1] <- "Eaten"
herb_binom_u_org$p_eaten[herb_binom_u_org$p_eaten == 0] <- "Not Eaten"
herb_binom_u_org2 <- herb_binom_u_org %>%
        group_by(state, origin, p_eaten) %>%
        count(p_eaten) %>%
        group_by(origin, state) %>%
        mutate(n = n/sum(n) * 100)
# merging both categorical variables (state and origin)
herb_binom_k_org2$Treatment <- paste(herb_binom_k_org2$origin, "_", herb_binom_k_org2$state)
herb_binom_u_org2$Treatment <- paste(herb_binom_u_org2$origin, "_", herb_binom_u_org2$state)

# plot
#Turn your 'treatment' column into a character vector
herb_binom_k_org2$Treatment <- as.character(herb_binom_k_org2$Treatment)
herb_binom_u_org2$Treatment <- as.character(herb_binom_u_org2$Treatment)
#Then turn it back into a factor with the levels in the correct order
herb_binom_k_org2$Treatment <- factor(herb_binom_k_org2$Treatment, levels=unique(herb_binom_k_org2$Treatment))
herb_binom_k_org2$Treatment <- factor(herb_binom_k_org2$Treatment,
                                      levels=c("Native _ warmed", "Exotic _ warmed", "Native _ ambient","Exotic _ ambient"))
herb_binom_u_org2$Treatment <- factor(herb_binom_u_org2$Treatment, levels=unique(herb_binom_u_org2$Treatment))
herb_binom_u_org2$Treatment <- factor(herb_binom_u_org2$Treatment,
                                      levels=c("Native _ warmed", "Exotic _ warmed", "Native _ ambient","Exotic _ ambient"))

binom_plot_k_org <- ggplot(herb_binom_k_org2,
                           aes(x= Treatment,y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha=p_eaten)) +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        #scale_x_discrete(limits=c("Native_warmed","Exotic_warmed","Native_ambient","Exotic_ambient")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        labs(y="Proportion eaten or not (%)", x=NULL,title="KBS", subtitle="A",fill=NULL) +
        theme_classic() +
        theme(plot.title = element_text(size = 17),
              plot.subtitle = element_text(size=13),
              axis.text.y = element_text(size=13),
              axis.text.x=element_blank(),
              axis.title.y=element_text(size=15),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14)) +
        theme(legend.position="none")
binom_plot_u_org <- ggplot(herb_binom_u_org2,
                           aes(x=Treatment, y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha=p_eaten)) +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        labs(y=NULL, x=NULL, title="UMBS",subtitle="B",fill=NULL) +
        theme_classic() +
        theme(plot.title = element_text(size = 17),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_blank(),
              axis.text.y = element_blank(),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14))

# amount eaten plot
sum_herb_org_k <- herb_org_rem %>%
        filter(site == "KBS",
               insecticide == "insects")
sum_herb_org_k <- sum_herb_org_k[sum_herb_org_k$p_eaten != 0, ]
sum_herb_org_k <- sum_herb_org_k %>%
        group_by(state, origin) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
sum_herb_org_u <- herb_org_rem %>%
        filter(site == "UMBS",
               insecticide == "insects")
sum_herb_org_u <- sum_herb_org_u[sum_herb_org_u$p_eaten != 0, ]
sum_herb_org_u <- sum_herb_org_u %>%
        group_by(state, origin) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
# merging both categorical variables (state and origin)
sum_herb_org_k$Treatment <- paste(sum_herb_org_k$origin, "_", sum_herb_org_k$state)
sum_herb_org_u$Treatment <- paste(sum_herb_org_u$origin, "_", sum_herb_org_u$state)

# plot
#Turn your 'treatment' column into a character vector
sum_herb_org_k$Treatment <- as.character(sum_herb_org_k$Treatment)
sum_herb_org_u$Treatment <- as.character(sum_herb_org_u$Treatment)
#Then turn it back into a factor with the levels in the correct order
sum_herb_org_k$Treatment <- factor(sum_herb_org_k$Treatment, levels=unique(sum_herb_org_k$Treatment))
sum_herb_org_k$Treatment <- factor(sum_herb_org_k$Treatment,
                                      levels=c("Native _ warmed", "Exotic _ warmed", "Native _ ambient","Exotic _ ambient"))
sum_herb_org_u$Treatment <- factor(sum_herb_org_u$Treatment, levels=unique(sum_herb_org_u$Treatment))
sum_herb_org_u$Treatment <- factor(sum_herb_org_u$Treatment,
                                      levels=c("Native _ warmed", "Exotic _ warmed", "Native _ ambient","Exotic _ ambient"))

eaten_k_org <- ggplot(sum_herb_org_k, aes(x = Treatment, y = avg_eaten, fill = state)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL, subtitle="C") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        theme_classic() +
        ylim(0,20) + 
        scale_x_discrete(labels=c("Native _ warmed" = "Native \n Warmed",
                                  "Exotic _ warmed" = "Exotic \n Warmed",
                                  "Native _ ambient" = "Native \n Ambient",
                                  "Exotic _ ambient" = "Exotic \n Ambient")) +
        theme(legend.position="none") +
        theme(axis.text.y = element_text(size=13),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_text(size=13),
              axis.title.y=element_text(size=15))
#ggplot(sum_herb_org_k, aes(x = state, y = avg_eaten, fill = state)) +
#        geom_bar(position = "identity", stat = "identity", col = "black") +
#        facet_wrap(.~origin) +
#        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
#                      position = "identity") +
#        labs(x = NULL, y = "Amount eaten (%)", title=NULL, subtitle="C") +
#        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
#        theme_classic() +
#        ylim(0,9) + 
#        #scale_x_discrete(labels=c("Native _ warmed" = "Native \n Warmed",
#        #                          "Exotic _ warmed" = "Exotic \n Warmed",
#        #                          "Native _ ambient" = "Native \n Ambient",
#        #                          "Exotic _ ambient" = "Exotic \n Ambient")) +
#        theme(legend.position="none") +
#        theme(axis.text.y = element_text(size=13),
#              plot.subtitle = element_text(size=13),
#              axis.text.x=element_text(size=13),
#              axis.title.y=element_text(size=15))
eaten_u_org <- ggplot(sum_herb_org_u, aes(x = Treatment, y = avg_eaten, fill = state)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = NULL, title=NULL, subtitle="D") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        theme_classic() +
        ylim(0,20) +
        scale_x_discrete(labels=c("Native _ warmed" = "Native \n Warmed",
                                  "Exotic _ warmed" = "Exotic \n Warmed",
                                  "Native _ ambient" = "Native \n Ambient",
                                  "Exotic _ ambient" = "Exotic \n Ambient")) +
        theme(legend.position="none") +
        theme(axis.text.y = element_blank(),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_text(size=13))

# plotting binary response & amount eaten on same figure
binary_org_overall <- ggarrange(binom_plot_k_org, binom_plot_u_org,
                            eaten_k_org, eaten_u_org,
                            nrow = 2, ncol = 2, common.legend = T, legend="top")
png("binary_org_combined_plot.png", units="in", width=8, height=8, res=300)
annotate_figure(binary_org_overall,
                bottom = text_grob("Treatment", color = "black",size=15))
dev.off()

### Growth form - binomial ###
# plotting binary response
# selecting KBS & herbivory plots =, making binary response for if eaten or not overall
herb_gr_rem <- herb %>%
        filter(!(growth_habit == 'Vine' |
                         growth_habit == "Shrub/tree" |
                         growth_habit == "Shrub" |
                         growth_habit == "Tree" |
                         growth_habit == ""))
herb_binom_k_gr <- herb_gr_rem %>%
        filter(site == "KBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k_gr$p_eaten[herb_binom_k_gr$p_eaten == 1] <- "Eaten"
herb_binom_k_gr$p_eaten[herb_binom_k_gr$p_eaten == 0] <- "Not Eaten"
herb_binom_k_gr2 <- herb_binom_k_gr %>%
        group_by(state, growth_habit, p_eaten) %>%
        count(p_eaten) %>%
        group_by(growth_habit, state) %>%
        mutate(n = n/sum(n) * 100)
# selecting UMBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_u_gr <- herb_gr_rem %>%
        filter(site == "UMBS",
               insecticide == "insects") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u_gr$p_eaten[herb_binom_u_gr$p_eaten == 1] <- "Eaten"
herb_binom_u_gr$p_eaten[herb_binom_u_gr$p_eaten == 0] <- "Not Eaten"
herb_binom_u_gr2 <- herb_binom_u_gr %>%
        group_by(state, growth_habit, p_eaten) %>%
        count(p_eaten) %>%
        group_by(growth_habit, state) %>%
        mutate(n = n/sum(n) * 100)
# merging both categorical variables (state and origin)
herb_binom_k_gr2$Treatment <- paste(herb_binom_k_gr2$growth_habit, "_", herb_binom_k_gr2$state)
herb_binom_u_gr2$Treatment <- paste(herb_binom_u_gr2$growth_habit, "_", herb_binom_u_gr2$state)

# plot
#Turn your 'treatment' column into a character vector
herb_binom_k_gr2$Treatment <- as.character(herb_binom_k_gr2$Treatment)
herb_binom_u_gr2$Treatment <- as.character(herb_binom_u_gr2$Treatment)
#Then turn it back into a factor with the levels in the correct order
herb_binom_k_gr2$Treatment <- factor(herb_binom_k_gr2$Treatment, levels=unique(herb_binom_k_gr2$Treatment))
herb_binom_k_gr2$Treatment <- factor(herb_binom_k_gr2$Treatment,
                                      levels=c("Forb _ warmed", "Graminoid _ warmed", "Forb _ ambient","Graminoid _ ambient"))
herb_binom_u_gr2$Treatment <- factor(herb_binom_u_gr2$Treatment, levels=unique(herb_binom_u_gr2$Treatment))
herb_binom_u_gr2$Treatment <- factor(herb_binom_u_gr2$Treatment,
                                      levels=c("Forb _ warmed", "Graminoid _ warmed", "Forb _ ambient","Graminoid _ ambient"))

binom_plot_k_gr <- ggplot(herb_binom_k_gr2,
                           aes(x= Treatment,y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha=p_eaten)) +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        #scale_x_discrete(limits=c("Native_warmed","Exotic_warmed","Native_ambient","Exotic_ambient")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        labs(y="Proportion eaten or not (%)", x=NULL,title="KBS",subtitle="A", fill=NULL) +
        theme_classic() +
        theme(plot.title = element_text(size = 17),
              plot.subtitle = element_text(size=13),
              axis.text.y = element_text(size=13),
              axis.text.x=element_blank(),
              axis.title.y=element_text(size=15),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14)) +
        theme(legend.position="none")
binom_plot_u_gr <- ggplot(herb_binom_u_gr2,
                           aes(x=Treatment, y=n, fill = state, label = paste0(round(n, 2), "%"))) +
        geom_col(col="black",aes(alpha=p_eaten)) +
        geom_text(position=position_stack(0.5), aes(group=p_eaten)) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          guide="none") +
        scale_alpha_manual(values=c(0.3, 1),
                           labels = c("Eaten","Not Eaten"),
                           name = "Proportion") +
        labs(y=NULL, x=NULL, title="UMBS",subtitle="B",fill=NULL) +
        theme_classic() +
        theme(plot.title = element_text(size = 17),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_blank(),
              axis.text.y = element_blank(),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14))

# amount eaten plot
sum_herb_gr_k <- herb_gr_rem %>%
        filter(site == "KBS",
               insecticide == "insects")
sum_herb_gr_k <- sum_herb_gr_k[sum_herb_gr_k$p_eaten != 0, ]
sum_herb_gr_k <- sum_herb_gr_k %>%
        group_by(state, growth_habit) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
sum_herb_gr_u <- herb_gr_rem %>%
        filter(site == "UMBS",
               insecticide == "insects")
sum_herb_gr_u <- sum_herb_gr_u[sum_herb_gr_u$p_eaten != 0, ]
sum_herb_gr_u <- sum_herb_gr_u %>%
        group_by(state, growth_habit) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
# merging both categorical variables (state and origin)
sum_herb_gr_k$Treatment <- paste(sum_herb_gr_k$growth_habit, "_", sum_herb_gr_k$state)
sum_herb_gr_u$Treatment <- paste(sum_herb_gr_u$growth_habit, "_", sum_herb_gr_u$state)

# plot
#Turn your 'treatment' column into a character vector
sum_herb_gr_k$Treatment <- as.character(sum_herb_gr_k$Treatment)
sum_herb_gr_u$Treatment <- as.character(sum_herb_gr_u$Treatment)
#Then turn it back into a factor with the levels in the correct order
sum_herb_gr_k$Treatment <- factor(sum_herb_gr_k$Treatment, levels=unique(sum_herb_gr_k$Treatment))
sum_herb_gr_k$Treatment <- factor(sum_herb_gr_k$Treatment,
                                   levels=c("Forb _ warmed", "Graminoid _ warmed", "Forb _ ambient","Graminoid _ ambient"))
sum_herb_gr_u$Treatment <- factor(sum_herb_gr_u$Treatment, levels=unique(sum_herb_gr_u$Treatment))
sum_herb_gr_u$Treatment <- factor(sum_herb_gr_u$Treatment,
                                   levels=c("Forb _ warmed", "Graminoid _ warmed", "Forb _ ambient","Graminoid _ ambient"))

eaten_k_gr <- ggplot(sum_herb_gr_k, aes(x = Treatment, y = avg_eaten, fill = state)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL, subtitle="C") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        theme_classic() +
        ylim(0,27) + 
        scale_x_discrete(labels=c("Forb _ warmed" = "Forb \n Warmed",
                                  "Graminoid _ warmed" = "Graminoid \n Warmed",
                                  "Forb _ ambient" = "Forb \n Ambient",
                                  "Graminoid _ ambient" = "Graminoid \n Ambient")) +
        theme(legend.position="none") +
        theme(axis.text.y = element_text(size=13),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_text(size=13),
              axis.title.y=element_text(size=15))
eaten_u_gr <- ggplot(sum_herb_gr_u, aes(x = Treatment, y = avg_eaten, fill = state)) +
        geom_bar(position = "identity", stat = "identity", col = "black") +
        geom_errorbar(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), width = 0.2,
                      position = "identity") +
        labs(x = NULL, y = NULL, title=NULL, subtitle="D") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a")) +
        theme_classic() +
        ylim(0,27) +
        scale_x_discrete(labels=c("Forb _ warmed" = "Forb \n Warmed",
                                  "Graminoid _ warmed" = "Graminoid \n Warmed",
                                  "Forb _ ambient" = "Forb \n Ambient",
                                  "Graminoid _ ambient" = "Graminoid \n Ambient")) +
        theme(legend.position="none") +
        theme(axis.text.y = element_blank(),
              plot.subtitle = element_text(size=13),
              axis.text.x=element_text(size=13))

# plotting binary response & amount eaten on same figure
binary_gr_overall <- ggarrange(binom_plot_k_gr, binom_plot_u_gr,
                                eaten_k_gr, eaten_u_gr,
                                nrow = 2, ncol = 2, common.legend = T, legend="top")
png("binary_gr_combined_plot.png", units="in", width=8, height=8, res=300)
annotate_figure(binary_gr_overall,
                bottom = text_grob("Treatment", color = "black",size=15))
dev.off()

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
