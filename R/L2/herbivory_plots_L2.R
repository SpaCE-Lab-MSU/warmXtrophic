# TITLE:          Herbivory plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L1 herbivory folder
# DATA OUTPUT:    Plots for herbivory for each species and per site
# PROJECT:        warmXtrophic
# DATE:           May 2021; updated April 2023

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

# Fix date column
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

# only keep species that were recorded in both warmed and ambient plots
herb <- herb %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))

# keeping only one date per site, per year to avoid replication
# can keep >1 date per year if species/plots are unique for each date
# notes for how I determined these are in herbivory analyses L2 script
herb <- herb %>%
        filter(!(site == "KBS" & date == "2015-09-04" |
                         site == "UMBS" & date == "2015-08-12" |
                         site == "UMBS" & date == "2020-08-24" & plot == "B4")) 



##### chance of being eaten + amount eaten - both sites dot plots #####
# selecting KBS, making binary response for if eaten or not overall
herb_binom_k_i <- herb %>%
        filter(site == "KBS") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k_i$p_eaten[herb_binom_k_i$p_eaten == 1] <- "Eaten"
herb_binom_k_i$p_eaten[herb_binom_k_i$p_eaten == 0] <- "Not Eaten"
herb_binom_sumk_i <- herb_binom_k_i %>%
        group_by(plot, state, insecticide,p_eaten) %>%
        count(state, insecticide,p_eaten) %>%
        group_by(plot, state,insecticide) %>%
        mutate(n = n/sum(n)) %>%
        group_by(state,insecticide,p_eaten) %>%
        summarize(mean_n = mean(n),
                  se = std.error(n))
herb_binom_eaten_k <- herb_binom_sumk_i %>%
        filter(p_eaten == "Eaten")
# selecting UMBS, making binary response for if eaten or not overall
herb_binom_u_i <- herb %>%
        filter(site == "UMBS") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u_i$p_eaten[herb_binom_u_i$p_eaten == 1] <- "Eaten"
herb_binom_u_i$p_eaten[herb_binom_u_i$p_eaten == 0] <- "Not Eaten"
herb_binom_sumu_i <- herb_binom_u_i %>%
        group_by(plot, state, insecticide,p_eaten) %>%
        count(state, insecticide,p_eaten) %>%
        group_by(plot, state,insecticide) %>%
        mutate(n = n/sum(n)) %>%
        group_by(state,insecticide,p_eaten) %>%
        summarize(mean_n = mean(n),
                  se = std.error(n))
herb_binom_eaten_u <- herb_binom_sumu_i %>%
        filter(p_eaten == "Eaten")
# plotting
binom_dot_k <- ggplot(herb_binom_eaten_k, aes(x = state, y = mean_n, fill=insecticide)) +
        geom_pointrange(aes(ymin = mean_n - se, ymax = mean_n + se), ,pch=21,size=1,position=position_dodge(0.2)) +
        labs(x = NULL, y = "Probability of being eaten", title="KBS") +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        ylim(0.18,0.50) +
        annotate("text", x = 0.5, y=0.50, label = "A", size=5) +
        theme_bw() +
        theme(legend.position="none") +
        theme(plot.title = element_text(size = 20),
              axis.text.y = element_text(size=17),
              axis.text.x=element_blank(),
              axis.title.y=element_text(size=17),
              legend.title=element_text(size=17), 
              legend.text=element_text(size=17)) +
        guides(color = "none") +
        scale_y_continuous(breaks = c(0.2,0.3,0.4,0.5), 
                           labels = c("  0.2", "  0.3", "  0.4", "  0.5"))
binom_dot_u <- ggplot(herb_binom_eaten_u, aes(x = state, y = mean_n, fill=insecticide)) +
        geom_pointrange(aes(ymin = mean_n - se, ymax = mean_n + se), ,pch=21,size=1,position=position_dodge(0.2)) +
        labs(x = NULL, y = "Probability of being eaten", title="UMBS") +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        ylim(0.18,0.50) +
        annotate("text", x = 0.5, y=0.50, label = "B", size=5) +
        theme_bw() +
        theme(legend.position="none") +
        theme(plot.title = element_text(size = 20),
              axis.text.x=element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              legend.title=element_text(size=17), 
              legend.text=element_text(size=17)) +
        guides(color = "none")

# amount eaten plot
# selecting KBS, average amount eaten if herbivory > 0
sum_herb_overall_k_i <- herb %>%
        filter(site == "KBS")
#sum_herb_overall_k_i <- sum_herb_overall_k_i[sum_herb_overall_k_i$p_eaten != 0, ]
sum_herb_overall_k_i <- sum_herb_overall_k_i %>%
        group_by(state, insecticide) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
# selecting UMBS, average amount eaten if herbivory > 0
sum_herb_overall_u_i <- herb %>%
        filter(site == "UMBS")
#sum_herb_overall_u_i <- sum_herb_overall_u_i[sum_herb_overall_u_i$p_eaten != 0, ]
sum_herb_overall_u_i <- sum_herb_overall_u_i %>%
        group_by(state,insecticide) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
# plotting
eaten_k_i <- ggplot(sum_herb_overall_k_i, aes(x = state, y = avg_eaten, fill=insecticide)) +
        geom_pointrange(aes(ymin = avg_eaten - se, ymax = avg_eaten + se),pch=21,size=1,position=position_dodge(0.2)) +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        ylim(1,10) +
        annotate("text", x = 0.5, y=10, label = "C", size=5) +
        theme_bw() +
        theme(legend.position="none") +
        theme(axis.text.y = element_text(size=17),
              axis.text.x=element_text(size=17),
              axis.title.y=element_text(size=17))
eaten_u_i <- ggplot(sum_herb_overall_u_i, aes(x = state, y = avg_eaten,fill=insecticide)) +
        geom_pointrange(aes(ymin = avg_eaten - se, ymax = avg_eaten + se),pch=21,size=1,position=position_dodge(0.2)) +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        ylim(1,10) +
        annotate("text", x = 0.5, y=10, label = "D", size=5) +
        theme_bw() +
        theme(legend.position="none") +
        theme(axis.text.y = element_blank(),
              axis.text.x=element_text(size=17),
              axis.title.y=element_blank())

# plotting binary response & amount eaten on same figure
png("binary_dots_insecticide.png", units="in", width=9, height=7, res=300)
ggarrange(binom_dot_k, binom_dot_u,
                              eaten_k_i, eaten_u_i,
                              nrow = 2, ncol = 2, common.legend = T, legend="right",widths = c(1.1, 0.9))
dev.off()



#### herbivory per year ####
# selecting KBS, making binary response for if eaten or not overall
herb_binom_k_year <- herb %>%
        filter(site == "KBS") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k_year$p_eaten[herb_binom_k_year$p_eaten == 1] <- "Eaten"
herb_binom_k_year$p_eaten[herb_binom_k_year$p_eaten == 0] <- "Not Eaten"
herb_binom_sumk_year <- herb_binom_k_year %>%
        group_by(plot, year, state, insecticide, p_eaten) %>%
        count(year,state, insecticide, p_eaten) %>%
        group_by(plot, state, insecticide, year) %>%
        mutate(n = n/sum(n)) %>%
        group_by(year, state, insecticide, p_eaten) %>%
        summarize(mean_n = mean(n),
                  se = std.error(n))
herb_binom_eaten_k_year <- herb_binom_sumk_year %>%
        filter(p_eaten == "Eaten")
# selecting UMBS, making binary response for if eaten or not overall
herb_binom_u_year <- herb %>%
        filter(site == "UMBS") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u_year$p_eaten[herb_binom_u_year$p_eaten == 1] <- "Eaten"
herb_binom_u_year$p_eaten[herb_binom_u_year$p_eaten == 0] <- "Not Eaten"
herb_binom_sumu_year <- herb_binom_u_year %>%
        group_by(plot, year, state, insecticide, p_eaten) %>%
        count(year,state, insecticide, p_eaten) %>%
        group_by(plot, state, insecticide, year) %>%
        mutate(n = n/sum(n)) %>%
        group_by(year, state, insecticide, p_eaten) %>%
        summarize(mean_n = mean(n),
                  se = std.error(n))
herb_binom_eaten_u_year <- herb_binom_sumu_year %>%
        filter(p_eaten == "Eaten")
# plotting
herb_binom_eaten_k_year$year <- as.factor(herb_binom_eaten_k_year$year)
herb_binom_eaten_k_year$full_treat <- paste(herb_binom_eaten_k_year$state, herb_binom_eaten_k_year$insecticide, sep="_")
binom_dot_k_year <- ggplot(herb_binom_eaten_k_year, aes(x = year, y = mean_n, group=full_treat, color=full_treat, fill=full_treat, linetype=full_treat)) +
        geom_line(size = 1) +
        geom_pointrange(aes(ymin = mean_n - se, ymax = mean_n + se), linetype="solid",pch=21,size=0.6) +
        labs(x = NULL, y = "Probability of being eaten", title="KBS") +
        #ylim(0.15,0.70) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"),# "#0b0055", "#0b0055", "#FFB451", "#FFB451"
                           labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_fill_manual(name="Treatment",
                          values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"), # "#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"
                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_linetype_manual(name="Treatment",
                              values = c("solid", "dotted", "solid", "dotted"),
                              labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        theme_bw() +
       # theme(legend.position="none") +
        theme(plot.title = element_text(size = 17),
              axis.text.y = element_text(size=13),
              axis.text.x = element_blank(),
              axis.title.y=element_text(size=15),
              legend.title=element_text(size=14), 
              legend.text=element_text(size=12))

herb_binom_eaten_u_year$year <- as.factor(herb_binom_eaten_u_year$year)
herb_binom_eaten_u_year$full_treat <- paste(herb_binom_eaten_u_year$state, herb_binom_eaten_u_year$insecticide, sep="_")
binom_dot_u_year <- ggplot(herb_binom_eaten_u_year, aes(x = year, y = mean_n, group=full_treat, color=full_treat, fill=full_treat, linetype=full_treat)) +
        geom_line(size = 1) +
        geom_pointrange(aes(ymin = mean_n - se, ymax = mean_n + se), linetype="solid",pch=21,size=0.6) +
        labs(x = NULL, y = "Probability of being eaten", title="UMBS") +
        #ylim(0.15,0.70) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"),# "#0b0055", "#0b0055", "#FFB451", "#FFB451"
                           labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_fill_manual(name="Treatment",
                          values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"), # "#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"
                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_linetype_manual(name="Treatment",
                              values = c("solid", "dotted", "solid", "dotted"),
                              labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        theme_bw() +
        # theme(legend.position="none") +
        theme(plot.title = element_text(size = 17),
              axis.text.y = element_text(size=13),
              axis.text.x = element_blank(),
              axis.title.y=element_blank(),
              legend.title=element_text(size=14), 
              legend.text=element_text(size=12))

# amount eaten plot
# selecting KBS, average amount eaten if herbivory > 0
sum_herb_overall_k_year <- herb %>%
        filter(site == "KBS")
sum_herb_overall_k_year <- sum_herb_overall_k_year %>%
        group_by(year, state, insecticide,) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
# selecting KBS, average amount eaten if herbivory > 0
sum_herb_overall_u_year <- herb %>%
        filter(site == "UMBS")
sum_herb_overall_u_year <- sum_herb_overall_u_year %>%
        group_by(year, state, insecticide,) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
# plotting
sum_herb_overall_k_year$year <- as.factor(sum_herb_overall_k_year$year)
sum_herb_overall_k_year$full_treat <- paste(sum_herb_overall_k_year$state, sum_herb_overall_k_year$insecticide, sep="_")
eaten_k_year <- ggplot(sum_herb_overall_k_year, aes(x = year, y = avg_eaten, group=full_treat, color=full_treat, fill=full_treat, linetype=full_treat)) +
        geom_line(size = 1) +
        geom_pointrange(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), linetype="solid",pch=21,size=0.6) +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL) +
        #ylim(0.15,0.70) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"),# "#0b0055", "#0b0055", "#FFB451", "#FFB451"
                           labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_fill_manual(name="Treatment",
                          values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"), # "#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"
                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_linetype_manual(name="Treatment",
                              values = c("solid", "dotted", "solid", "dotted"),
                              labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        theme_bw() +
        # theme(legend.position="none") +
        theme(plot.title = element_text(size = 17),
              axis.text.y = element_text(size=13),
              axis.text.x = element_text(size=13),
              axis.title.y=element_text(size=15),
              legend.title=element_text(size=14), 
              legend.text=element_text(size=12))

sum_herb_overall_u_year$year <- as.factor(sum_herb_overall_u_year$year)
sum_herb_overall_u_year$full_treat <- paste(sum_herb_overall_u_year$state, sum_herb_overall_u_year$insecticide, sep="_")
eaten_u_year <- ggplot(sum_herb_overall_u_year, aes(x = year, y = avg_eaten, group=full_treat, color=full_treat, fill=full_treat, linetype=full_treat)) +
        geom_line(size = 1) +
        geom_pointrange(aes(ymin = avg_eaten - se, ymax = avg_eaten + se), linetype="solid",pch=21,size=0.6) +
        labs(x = NULL, y = "Amount eaten (%)", title=NULL) +
        #ylim(0.15,0.70) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"),# "#0b0055", "#0b0055", "#FFB451", "#FFB451"
                           labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_fill_manual(name="Treatment",
                          values = c("#a6bddb", "#a6bddb", "#AE1F00", "#AE1F00"), # "#a6bddb", "#a6bddb", "#fb6a4a", "#fb6a4a"
                          labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        scale_linetype_manual(name="Treatment",
                              values = c("solid", "dotted", "solid", "dotted"),
                              labels=c("Ambient + Herbivory","Ambient + Reduced Herbivory","Warmed + Herbivory", "Warmed + Reduced Herbivory")) +
        theme_bw() +
        # theme(legend.position="none") +
        theme(plot.title = element_text(size = 17),
              axis.text.y = element_text(size=13),
              axis.text.x = element_text(size=13),
              axis.title.y=element_blank(),
              legend.title=element_text(size=14), 
              legend.text=element_text(size=12))

# plotting binary response & amount eaten on same figure
binary_overall_year <- ggarrange(binom_dot_k_year, binom_dot_u_year,
                              eaten_k_year, eaten_u_year,
                              nrow = 2, ncol = 2, common.legend = T, legend="right",widths = c(1, 0.9))
png("herb_binary_dots_year.png", units="in", width=9.5, height=7, res=300)
annotate_figure(binary_overall_year,
                bottom = text_grob("Year", color = "black",size=15))
dev.off()



##### amount eaten for all species at each site #####
sum_herb_spp2 <- herb[herb$p_eaten != 0, ]
sum_herb_spp2 <- sum_herb_spp2 %>%
        group_by(site, year, species) %>%
        filter(length(plot) >= 12) %>% # only keeping species present in at least 12 plots each year
        filter(all(c('warmed', 'ambient') %in% state)) %>% 
        group_by(site, state, insecticide, species) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
sum_herb_spp2 <- subset(sum_herb_spp2)
herb_spp_overall <- function(loc) { 
        herb_spp <- subset(sum_herb_spp2, site == loc)
        return(ggplot(herb_spp, aes(x = state, y = avg_eaten, fill = insecticide)) +
                       facet_wrap(~species, ncol=4, scales="free") +
                       geom_pointrange(aes(ymin = avg_eaten - se, ymax = avg_eaten + se),pch=21,size=0.9,position=position_dodge(0.4)) +
                       labs(x = NULL, y = NULL, title=loc) +
                       scale_fill_manual(name="Treatment",
                                         values = c("#FFB451", "#0b0055"),
                                         labels = c("Herbivory","Reduced Herbivory")) +
                       #coord_cartesian(ylim = c(100, 250)) +
                       scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
                       theme_bw(14)) +
                theme(legend.position = "none")
}
kbs_herb_spp <- herb_spp_overall("KBS")
umbs_herb_spp <- herb_spp_overall("UMBS")
herb_overall_merge <- ggpubr::ggarrange(kbs_herb_spp, umbs_herb_spp,
                                        ncol = 2, common.legend=T, legend="right")
png("herb_species.png", units="in", width=18, height=8, res=300)
annotate_figure(herb_overall_merge,
                left = text_grob("Amount eaten (%)", color = "black", rot = 90, size=15),
                bottom = text_grob("Treatment", color = "black", size=15))
dev.off()



##### all species binomial response at each site #####
# selecting KBS & herbivory plots, making binary response for if eaten or not overall
herb_binom_k <- herb %>%
        filter(site == "KBS") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k$p_eaten[herb_binom_k$p_eaten == 1] <- "Eaten"
herb_binom_k$p_eaten[herb_binom_k$p_eaten == 0] <- "Not Eaten"
herb_binom_sumk2 <- herb_binom_k %>%
        group_by(site, year, species) %>%
        filter(length(plot) >= 12) %>% # only keeping species present in at least 12 plots each year
        filter(all(c('warmed', 'ambient') %in% state)) %>%
        group_by(plot,state, insecticide, species, p_eaten) %>%
        count(p_eaten) %>%
        group_by(plot,species, state, insecticide) %>%
        mutate(n = n/sum(n)) %>%
        group_by(species,state,insecticide,p_eaten) %>%
        summarize(mean_n = mean(n),
                  se = std.error(n))
herb_binom_sumk3 <- herb_binom_sumk2 %>%
        filter(p_eaten == "Eaten") %>%
        group_by(species) %>%
        filter(all(c('warmed', 'ambient') %in% state))
# selecting UMBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_u <- herb %>%
        filter(site == "UMBS") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u$p_eaten[herb_binom_u$p_eaten == 1] <- "Eaten"
herb_binom_u$p_eaten[herb_binom_u$p_eaten == 0] <- "Not Eaten"
herb_binom_sumu2 <- herb_binom_u %>%
        group_by(site, year, species) %>%
        filter(length(plot) >= 12) %>% # only keeping species present in at least 12 plots each year
        filter(all(c('warmed', 'ambient') %in% state)) %>%
        group_by(plot,state, insecticide, species, p_eaten) %>%
        count(p_eaten) %>%
        group_by(plot,species, state, insecticide) %>%
        mutate(n = n/sum(n)) %>%
        group_by(species,state,insecticide,p_eaten) %>%
        summarize(mean_n = mean(n),
                  se = std.error(n))
herb_binom_sumu3 <- herb_binom_sumu2 %>%
        filter(p_eaten == "Eaten") %>%
        group_by(species) %>%
        filter(all(c('warmed', 'ambient') %in% state))
# plotting binary response
binom_plot_k2 <- ggplot(herb_binom_sumk3, aes(x=state, y=mean_n, fill=insecticide)) +
        facet_wrap(~species, ncol=4) +
        geom_pointrange(aes(ymin = mean_n - se, ymax = mean_n + se),pch=21,size=1,position=position_dodge(0.2)) +
        labs(x = NULL, y = NULL, title="KBS") +
        scale_x_discrete(labels=c("ambient" = "Ambient ", "warmed" = " Warmed")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #ylim(5,21) +
        theme_bw() +
        theme(legend.title=element_text(size=15), 
              legend.text=element_text(size=15),
              axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              title=element_text(size=17),
              strip.text=element_text(size=15)) +
        guides(color = "none")
binom_plot_u2 <- ggplot(herb_binom_sumu3, aes(x=state, y=mean_n, fill = insecticide)) +
        facet_wrap(~species, ncol=4) +
        geom_pointrange(aes(ymin = mean_n - se, ymax = mean_n + se),pch=21,size=1,position=position_dodge(0.2)) +
        labs(x = NULL, y = NULL, title="UMBS") +
        scale_x_discrete(labels=c("ambient" = "Ambient ", "warmed" = " Warmed")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #ylim(5,21) +
        theme_bw() +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=15),
              title=element_text(size=17),
              strip.text=element_text(size=15)) +
        guides(color = "none")
herb_spp_binom_merge <- ggpubr::ggarrange(binom_plot_k2, binom_plot_u2,
                                          ncol = 2, common.legend=T, legend="right")
png("herb_species_binom.png", units="in", width=18, height=8, res=300)
annotate_figure(herb_spp_binom_merge,
                left = text_grob("Probability of being eaten", color = "black", rot = 90, size=17),
                bottom = text_grob("Treatment                           ", color = "black", size=17))
dev.off()



### mean temp w/ percent eaten and amount eaten ###
# selecting KBS, making binary response for if eaten or not overall
herb_binom_k_i2 <- herb %>%
        filter(site == "KBS" & state == "ambient") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_k_i2$p_eaten[herb_binom_k_i2$p_eaten == 1] <- "Eaten"
herb_binom_k_i2$p_eaten[herb_binom_k_i2$p_eaten == 0] <- "Not Eaten"
herb_binom_sumk_i2 <- herb_binom_k_i2 %>%
        group_by(plot, species, state, year, p_eaten) %>%
        count(state, year, p_eaten) %>%
        group_by(plot, species, state, year) %>%
        mutate(n = n/sum(n))
kbs_herb_binom_temp <- merge(x = herb_binom_sumk_i2, y = herb_binom_k_i2[ , c("state", "year","mean_temp")], by = c("state","year"), all.x=TRUE)
kbs_herb_binom_temp <- kbs_herb_binom_temp[!duplicated(kbs_herb_binom_temp), ]
kbs_herb_binom_temp <- kbs_herb_binom_temp %>%
        filter(p_eaten == "Eaten")
# selecting UMBS & herbivory plots =, making binary response for if eaten or not overall
herb_binom_u_i2 <- herb %>%
        filter(site == "UMBS" & state == "ambient") %>%
        mutate_at(vars(contains('p_eaten')), ~1 * (. != 0))
herb_binom_u_i2$p_eaten[herb_binom_u_i2$p_eaten == 1] <- "Eaten"
herb_binom_u_i2$p_eaten[herb_binom_u_i2$p_eaten == 0] <- "Not Eaten"
herb_binom_sumu_i2 <- herb_binom_u_i2 %>%
        group_by(plot,species,state, year,p_eaten) %>%
        count(state, year,p_eaten) %>%
        group_by(plot,species,state,year) %>%
        mutate(n = n/sum(n))
umbs_herb_binom_temp <- merge(x = herb_binom_sumu_i2, y = herb_binom_u_i2[ , c("state", "year","mean_temp")], by = c("state","year"), all.x=TRUE)
umbs_herb_binom_temp <- umbs_herb_binom_temp[!duplicated(umbs_herb_binom_temp), ]
umbs_herb_binom_temp <- umbs_herb_binom_temp %>%
        filter(p_eaten == "Eaten")
# plot
temp_prob_eaten_k <- ggplot(kbs_herb_binom_temp, aes(x = mean_temp, y = n, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = NULL,y="Probability of being eaten", title="KBS",subtitle="A") +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        xlim(14.9,15.7) +
        theme(legend.title=element_text(size=13), 
              legend.text=element_text(size=13),
              title=element_text(size=14),
              plot.subtitle=element_text(size=13),
              axis.text.y = element_text(size=13),
              axis.text.x=element_blank(),
              axis.title.y=element_text(size=14))
temp_prob_eaten_u <- ggplot(umbs_herb_binom_temp, aes(x = mean_temp, y = n, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = NULL,y=NULL, title="UMBS",subtitle="B") +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        xlim(14.9,17.1) +
        theme(legend.title=element_text(size=13), 
              legend.text=element_text(size=13),
              title=element_text(size=14),
              plot.subtitle=element_text(size=13),
              axis.text.y = element_blank(),
              axis.text.x=element_blank(),
              axis.title.y=element_blank())

# amount eaten plot
sum_herb_overall_k_i2 <- herb %>%
        filter(site == "KBS" & state == "ambient")
#sum_herb_overall_k_i3 <- sum_herb_overall_k_i2[sum_herb_overall_k_i2$p_eaten != 0, ]
sum_herb_overall_k_i3 <- sum_herb_overall_k_i2 %>%
        group_by(plot,species,year,state) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
kbs_herb_am_temp <- merge(x = sum_herb_overall_k_i3, y = sum_herb_overall_k_i2[ , c("state", "year","mean_temp")], by = c("year","state"), all.x=TRUE)
kbs_herb_am_temp <- kbs_herb_am_temp[!duplicated(kbs_herb_am_temp), ]

sum_herb_overall_u_i2 <- herb %>%
        filter(site == "UMBS" & state == "ambient")
#sum_herb_overall_u_i3 <- sum_herb_overall_u_i2[sum_herb_overall_u_i2$p_eaten != 0, ]
sum_herb_overall_u_i3 <- sum_herb_overall_u_i2 %>%
        group_by(plot,species,year,state) %>%
        summarize(avg_eaten = mean(p_eaten, na.rm = TRUE),
                  se = std.error(p_eaten, na.rm = TRUE))
umbs_herb_am_temp <- merge(x = sum_herb_overall_u_i3, y = sum_herb_overall_u_i2[ , c("state", "year","mean_temp")], by = c("year","state"), all.x=TRUE)
umbs_herb_am_temp <- umbs_herb_am_temp[!duplicated(umbs_herb_am_temp), ]

temp_amount_eaten_k <- ggplot(kbs_herb_am_temp, aes(x = mean_temp, y = avg_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Mean temperature (°C)",y="Amount eaten (%)",subtitle="C") +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        xlim(14.9,15.7) +
        theme(legend.title=element_text(size=13), 
              legend.text=element_text(size=13),
              plot.subtitle=element_text(size=13),
              axis.text.y = element_text(size=13),
              axis.text.x=element_text(size=13),
              axis.title.y=element_text(size=14),
              axis.title.x=element_blank())
temp_amount_eaten_u <- ggplot(umbs_herb_am_temp, aes(x = mean_temp, y = avg_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Mean temperature (°C)",y=NULL,subtitle="D") +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        xlim(14.9,17.1) +
        theme(legend.title=element_text(size=13), 
              legend.text=element_text(size=13),
              plot.subtitle=element_text(size=13),
              axis.text.y = element_blank(),
              axis.text.x=element_text(size=13),
              axis.title.y=element_blank(),
              axis.title.x=element_blank())
# merge fig
gdd_mean_temp_merge <- ggpubr::ggarrange(temp_prob_eaten_k,temp_prob_eaten_u,
                                         temp_amount_eaten_k,temp_amount_eaten_u,
                                         ncol = 2, nrow=2,common.legend=T,legend="none",widths=c(1,0.9))
png("herb_gdd_mean_temp.png", units="in", width=7, height=6, res=300)
annotate_figure(gdd_mean_temp_merge,
                bottom = text_grob("Mean temperature (°C)              ", color = "black", size=14))
dev.off()











########### figs not in manuscript ############
# temp data regressions
herb_kbs <- herb %>%
        filter(site == "KBS" & state == "ambient")
herb_umbs <- herb %>%
        filter(site == "UMBS" & state == "ambient")
# looking to see which temp variable best correlates with herbivory
kbs_gdd <- ggplot(herb_kbs, aes(x = GDD_cumulative, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Cumulative growing degree days (GDD)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_gdd <- ggplot(herb_umbs, aes(x = GDD_cumulative, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Cumulative growing degree days (GDD)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
kbs_mean <- ggplot(herb_kbs, aes(x = mean_temp, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Mean temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_mean <- ggplot(herb_umbs, aes(x = mean_temp, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Mean temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
kbs_med <- ggplot(herb_kbs, aes(x = median_temp, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Median temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_med <- ggplot(herb_umbs, aes(x = median_temp, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Median temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
kbs_max <- ggplot(herb_kbs, aes(x = max_temp, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Max temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=12))
umbs_max <- ggplot(herb_umbs, aes(x = max_temp, y = p_eaten, color = state))+
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = state)) + 
        labs(x = "Max temperature (°C)",y=NULL) +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
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
                                    ncol = 2, nrow=4,common.legend=T,legend="none")
png("herb_gdd_temp.png", units="in", width=7, height=8, res=300)
annotate_figure(gdd_temp_merge,
                left = text_grob("Percent eaten (%)", color = "black", rot = 90, size=15),
                top = text_grob("KBS                                                      UMBS", color = "black", size=15))
dev.off()
