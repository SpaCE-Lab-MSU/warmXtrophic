# TITLE:          Leaf trait plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Moriah Young, Phoebe Zarnetske, Mark Hammond
# DATA INPUT:     Data imported as csv files from shared Google drive L1 plant comp folder
# DATA OUTPUT:    Plots of SLA, C, and N data. Biomass data is also merged into a figure here
# PROJECT:        warmXtrophic
# DATE:           July 2023


# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(plotrix)

# Set working directory
L1_dir<-Sys.getenv("L1DIR")


# Read in data
cn <- read.csv(file.path(L1_dir, "CN/CN_L1.csv"))
sla <- read.csv(file.path(L1_dir,"SLA/SLA_L1_nooutliers.csv")) # this L1 csv was made in the SLA_analyses script
# reading in biomass data here to merge w/ leaf traits in a fig
kbs_biomass_21 <- read.csv(file.path(L1_dir, "ANPP/kbs_biomass_2021_L1.csv"))
umbs_biomass_21 <- read.csv(file.path(L1_dir, "ANPP/umbs_biomass_2021_L1.csv"))
kbs_biomass_21 <- kbs_biomass_21 %>% dplyr::select(-X) # get rid of "X" column that shows up (could fix this in cleaning script)
umbs_biomass_21 <- umbs_biomass_21 %>% dplyr::select(-X)


# manipulating CN data
# making plant ID
cn$plant_id <- paste(cn$plot, cn$plant_number, sep = "_")
# take the plant-level mean
cn <- cn %>%
        rename(treatment = state)
cn1 <- cn %>%
        group_by(plot,site,year,species,treatment, insecticide, plant_id,mean_temp,median_temp,max_temp,GDD_cumulative) %>%
        summarize(nitrogen = mean(nitrogen,na.rm=T), carbon = mean(carbon,na.rm=T), weight_mg = mean(weight_mg,na.rm=T)) 
# remove outliers (found in CN analyses script)
cn1<-cn1 %>% filter(carbon < 55)
cn1<-cn1 %>% filter(nitrogen < 4)
# take the treatment-level mean
cn_treatment <- cn1 %>%
        group_by(site, plot, species, treatment, insecticide) %>%
        summarize(nitrogen = mean(nitrogen,na.rm=T), carbon = mean(carbon,na.rm=T), weight_mg = mean(weight_mg,na.rm=T)) %>%    
        group_by(site,treatment,insecticide) %>%
        summarize(nitrogen_mean = mean(nitrogen,na.rm=T), carbon_mean = mean(carbon,na.rm=T), weight_mg = mean(weight_mg,na.rm=T),
                  n_se = std.error(nitrogen,na.rm=T), c_se = std.error(carbon,na.rm=T)) 
# take the average by year + species
cn_spp <- cn1 %>%
        group_by(site,year, species, treatment) %>%
        summarize(nitrogen_mean = mean(nitrogen), carbon_mean = mean(carbon), weight_mg = mean(weight_mg),
                  n_se = std.error(nitrogen), c_se = std.error(carbon)) 
#cn_ma2 <- cn1 %>%
#        group_by(site,species, treatment) %>%
#        filter(year == max(year)) %>%
#        group_by(site,species, treatment) %>%
#        summarize(nitrogen_mean = mean(nitrogen,na.rm=T), carbon_mean = mean(carbon,na.rm=T), weight_mg = mean(weight_mg,na.rm=T),
#                  n_se = std.error(nitrogen,na.rm=T), c_se = std.error(carbon,na.rm=T),
#                  count=n())
# summary of data
with(cn1,table(cn$site,cn$species)) 
with(cn1,table(cn$year,cn$species)) 
with(cn1,table(cn$year,cn$site)) 


# manipulating SLA data
# take the plant-level mean
sla1 <- sla %>%
        group_by(plot,site,year,species,state, insecticide, plant_number) %>%
        summarize(sla = mean(sla,na.rm=T),
                  se = std.error(sla,na.rm=T)) 
# take the yearly treatment-level mean
sla_treatment <- sla1 %>%
        group_by(site, plot, state,year) %>%
        summarize(sla = mean(sla,na.rm=T)) %>%    
        group_by(site,year,state) %>%
        summarize(sla_mean = mean(sla,na.rm=T),
                  se = std.error(sla,na.rm=T)) 
# take the overall treatment-level mean
sla_treatment2 <- sla1 %>%
        group_by(site, plot, state, insecticide) %>%
        summarize(sla = mean(sla,na.rm=T)) %>%    
        group_by(site,state, insecticide) %>%
        summarize(sla_mean = mean(sla,na.rm=T),
                  se = std.error(sla,na.rm=T)) 
# take the species treatment-level mean
sla_treatment3 <- sla1 %>%
        group_by(site, plot, species, state) %>%
        summarize(sla = mean(sla,na.rm=T)) %>%    
        group_by(site,species, state) %>%
        summarize(sla_mean = mean(sla,na.rm=T),
                  se = std.error(sla,na.rm=T)) 
#sla_ma2 <- sla1 %>%
#        group_by(site,species, state) %>%
#        filter(year == max(year)) %>%
#        group_by(site,species, state) %>%
#        summarize(mean_sla = mean(sla,na.rm=T),
#                  se = std.error(sla,na.rm=T),
#                  count=n())


# making separate dataframe for biomass - easier in plots
kbs_biomass_only <- kbs_biomass_21 %>%
        dplyr::select(-cover) %>%
        drop_na(weight_g)
umbs_biomass_only <- umbs_biomass_21 %>%
        dplyr::select(-cover) %>%
        drop_na(weight_g)
# remove uninformative species from biomass
kbs_biomass_live <- kbs_biomass_only[!grepl("Litter", kbs_biomass_only$species),]
kbs_biomass_live <- kbs_biomass_live[!grepl("Umsp", kbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_only[!grepl("Litter", umbs_biomass_only$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Standing_Dead", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Surface_Litter", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Lisp", umbs_biomass_live$species),]
umbs_biomass_live <- umbs_biomass_live[!grepl("Umsp", umbs_biomass_live$species),]
# keeping species found only in w and a
kbs_biomass_live2 <- kbs_biomass_live %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
umbs_biomass_live2 <- umbs_biomass_live %>%
        group_by(species) %>% 
        filter(all(c('warmed', 'ambient') %in% state))
# summarizing data
biomass_sum_k2 <- kbs_biomass_live %>%
        group_by(plot, state, insecticide) %>%
        summarize(avg_weight = sum(weight_g, na.rm = TRUE))
biomass_sum_k3 <- biomass_sum_k2 %>%
        group_by(state, insecticide) %>%
        summarize(average_weight = mean(avg_weight, na.rm = TRUE),
                  se = std.error(avg_weight, na.rm = TRUE))
biomass_sum_u2 <- umbs_biomass_live %>%
        group_by(plot, state, insecticide) %>%
        summarize(avg_weight = sum(weight_g, na.rm = TRUE))
biomass_sum_u3 <- biomass_sum_u2 %>%
        group_by(state, insecticide) %>%
        summarize(average_weight = mean(avg_weight, na.rm = TRUE),
                  se = std.error(avg_weight, na.rm = TRUE))


# clean insecticide + site labels for plotting
insect_labels <- c("insects" = "Herbivory", "no_insects" = "Reduced Herbivory")
site_label <- c("kbs" = "KBS", "umbs" = "UMBS")


###### CN figures ######
# Carbon warming + insecticide treatments
cn_treat_kbs <- cn_treatment %>%
        filter(site == "kbs")
c_kbs <- ggplot(cn_treat_kbs, aes(x = treatment, y = carbon_mean, fill = insecticide)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=carbon_mean-c_se, ymax=carbon_mean+c_se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = "Carbon (%)", fill = "Treatment", title="KBS") +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
        #                  labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_y_continuous(breaks = c(45.0,45.5,46.0,46.5), 
                           labels = c(" 45.0", " 45.5", " 46.0", " 46.5")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        annotate("text", x = 0.5, y=46.9, label = "A", size=5) +
        theme_bw(14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
cn_treat_umbs <- cn_treatment %>%
        filter(site == "umbs")
c_umbs <- ggplot(cn_treat_umbs, aes(x = treatment, y = carbon_mean, fill = insecticide)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=carbon_mean-c_se, ymax=carbon_mean+c_se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = NULL, fill = "Treatment", title="UMBS") +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
        #                  labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        annotate("text", x = 0.5, y=45.75, label = "B", size=5) +
        theme_bw(14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
c_comb <- ggpubr::ggarrange(c_kbs, c_umbs,
                            ncol = 2, common.legend = T, legend="none")
png("carbon_treatmentlevel_L2.png", units="in", width=9, height=4, res=300)
c_content_main <- annotate_figure(c_comb,
                left = text_grob("Carbon content (%)", color = "black", rot = 90, size=17))
dev.off()


# Carbon year + species + warming
png("carbon_species_L2.png", units="in", width=6, height=4, res=300)
ggplot(cn_spp, aes(x = year, y = carbon_mean, fill = treatment)) +
        facet_wrap(.~species) +
        geom_pointrange(aes(ymin=carbon_mean-c_se, ymax=carbon_mean+c_se), pch=21,size=0.5,position=position_dodge(0.4)) +
        #geom_bar(position = "dodge", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = position_dodge(0.9)) +
        labs(fill = "Treatment", x = "Year", y = "Carbon content (%)") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        coord_cartesian(ylim=c(40,50)) +
        theme_bw(14) +
        theme(legend.position = "right")
dev.off()


# Nitrogen warming + insecticide treatments
cn_treat_kbs <- cn_treatment %>%
        filter(site == "kbs")
n_kbs <- ggplot(cn_treat_kbs, aes(x = treatment, y = nitrogen_mean, fill = insecticide)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=nitrogen_mean-n_se, ymax=nitrogen_mean+n_se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = "Nitrogen (%)", fill = "Treatment", title=NULL) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
        #                  labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_y_continuous(breaks = c(1.90,1.95,2.00,2.05,2.10,2.15), 
                           labels = c(" 1.90", " 1.95", " 2.00", " 2.05"," 2.10"," 2.15")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        annotate("text", x = 0.5, y=2.145, label = "C", size=5) +
        theme_bw(14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
cn_treat_umbs <- cn_treatment %>%
        filter(site == "umbs")
n_umbs <- ggplot(cn_treat_umbs, aes(x = treatment, y = nitrogen_mean, fill = insecticide)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=nitrogen_mean-n_se, ymax=nitrogen_mean+n_se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = nitrogen_mean - n_se, ymax = nitrogen_mean + n_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = NULL, fill = "Treatment", title=NULL) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
        #                  labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_y_continuous(breaks = c(1.5,1.6,1.7,1.8,1.9), 
                           labels = c("    1.5", "    1.6", "    1.7", "    1.8","    1.9")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        annotate("text", x = 0.5, y=2, label = "D", size=5) +
        theme_bw(14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
n_comb <- ggpubr::ggarrange(n_kbs, n_umbs,
                            ncol = 2, common.legend = T, legend="none")
png("nitrogen_treatmentlevel_L2.png", units="in", width=9, height=4, res=300)
n_content_main <- annotate_figure(n_comb,
                left = text_grob("Nitrogen content (%)", color = "black", rot = 90, size=17))
dev.off()


# Nitrogen species + year + warming
png("nitrogen_species_L2.png", units="in", width=6, height=4, res=300)
ggplot(cn_spp, aes(x = year, y = nitrogen_mean, fill = treatment)) +
        facet_wrap(.~species) +
        geom_pointrange(aes(ymin=nitrogen_mean-n_se, ymax=nitrogen_mean+n_se), pch=21,size=0.5,position=position_dodge(0.4)) +
        #geom_bar(position = "dodge", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = position_dodge(0.9)) +
        labs(fill = "Treatment", x = "Year", y = "Nitrogen content (%)") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(40,50)) +
        theme_bw(14) +
        theme(legend.position = "right")
dev.off()

# Temp & GDD figure
# select ambient plots & take species/plot level mean
cn_amb <- cn1 %>%
        filter(treatment == "ambient") %>%
        group_by(site,plot,year,species,treatment,mean_temp,median_temp,max_temp,GDD_cumulative) %>%
        summarize(nitrogen = mean(nitrogen,na.rm=T), carbon = mean(carbon,na.rm=T), weight_mg = mean(weight_mg,na.rm=T))
        
        
png("carbon_temp_L2.png", units="in", width=6, height=4, res=300)
ggplot(cn_amb, aes(x = mean_temp, y = carbon, color=treatment))+
        facet_wrap(.~site, labeller=as_labeller(site_label), scales="free") +
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = treatment)) + 
        labs(x = "Mean temperature (°C)",y="Carbon content (%)") +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.position = "none")
dev.off()

png("nitrogen_species_L2.png", units="in", width=6, height=4, res=300)
ggplot(cn_amb, aes(x = mean_temp, y = nitrogen, color=treatment))+
        facet_wrap(.~site, labeller=as_labeller(site_label), scales="free") +
        geom_point() +
        geom_smooth(method = 'lm', aes(fill = treatment)) + 
        labs(x = "Mean temperature (°C)",y="Nitrogen content (%)") +
        scale_color_manual(values = c("#0b0055", "#fb6a4a"),
                           labels = c("Ambient","Warmed"),
                           name="Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("Ambient","Warmed"),
                          name = "Treatment") +
        theme_classic() +
        theme(legend.position = "none")
dev.off()




###### SLA Figures ######
# warming and insecticide treatments
sla_treatment_kbs <- sla_treatment2 %>%
        filter(site == "kbs")
sla_treatment_umbs <- sla_treatment2 %>%
        filter(site == "umbs")
sla_kbs <- ggplot(sla_treatment_kbs, aes(x = state, y = sla_mean, fill = insecticide)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=sla_mean-se, ymax=sla_mean+se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = bquote("SLA " (cm^2/g)), fill = "Treatment", title=NULL) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
        #                  labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        annotate("text", x = 0.5, y=222, label = "E", size=5) +
        theme_bw(14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
sla_umbs <- ggplot(sla_treatment_umbs, aes(x = state, y = sla_mean, fill = insecticide)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=sla_mean-se, ymax=sla_mean+se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = NULL, fill = "Treatment", title=NULL) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        #scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
        #                  labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_y_continuous(breaks = c(110,120,130,140), 
                           labels = c("   110", "   120", "   130", "   140")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        annotate("text", x = 0.5, y=140, label = "F", size=5) +
        theme_bw(14) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
sla_comb <- ggpubr::ggarrange(sla_kbs, sla_umbs,
                              ncol = 2, common.legend = T, legend="none")
png("sla_treatmentlevel_L2.png", units="in", width=9, height=4, res=300)
sla_main <- annotate_figure(sla_comb,
                left = text_grob(bquote("Specific leaf area " (cm^2/g)), color = "black",
                                 rot = 90, size=17))
dev.off()


# Year and warming #
sla_treatment_kbs_year <- sla_treatment %>%
        filter(site == "kbs")
sla_treatment_umbs_year <- sla_treatment %>%
        filter(site == "umbs")
sla_kbs_year <- ggplot(sla_treatment_kbs_year, aes(x = as.factor(year), y = sla_mean, fill = state)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=sla_mean-se, ymax=sla_mean+se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = NULL, fill = "Treatment", title="KBS") +
        #scale_fill_manual(name="Treatment",
        #                                 values = c("#FFB451", "#0b0055"),
        #                                 labels=c("Herbivory","Reduced Herbivory")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        theme_classic() +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              title = element_text(size=17),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15))
sla_umbs_year <- ggplot(sla_treatment_umbs_year, aes(x = as.factor(year), y = sla_mean, fill = state)) +
        #facet_wrap(.~insecticide, labeller = as_labeller(insect_labels)) +
        geom_pointrange(aes(ymin=sla_mean-se, ymax=sla_mean+se), pch=21,size=1,position=position_dodge(0.3)) +
        #geom_bar(position = "identity", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = carbon_mean - c_se, ymax = carbon_mean + c_se), width = 0.2,
        #        position = "identity") +
        labs(x = NULL, y = NULL, fill = "Treatment", title="UMBS") +
        #scale_fill_manual(name="Treatment",
        #                                 values = c("#FFB451", "#0b0055"),
        #                                 labels=c("Herbivory","Reduced Herbivory")) +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        #coord_cartesian(ylim=c(42,48)) +
        theme_classic() +
        theme(axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              title = element_text(size=17),
              legend.text = element_text(size=15),
              legend.title = element_text(size=15))
sla_comb <- ggpubr::ggarrange(sla_kbs_year, sla_umbs_year,
                              ncol = 2, common.legend = T, legend="right")
png("sla_treatmentlevel_year_L2.png", units="in", width=9, height=4, res=300)
annotate_figure(sla_comb,
                left = text_grob(bquote("Specific leaf area " (cm^2/g)), color = "black", rot = 90, size=17))
dev.off()


# Species and warming
png("sla_spp_L2.png", units="in", width=8, height=4, res=300)
ggplot(sla_treatment3, aes(x = species, y = sla_mean, fill = state)) +
        facet_wrap(.~site, labeller = as_labeller(site_label)) +
        #geom_point(aes(fill=state),pch=21, size=3) +
        geom_pointrange(aes(ymin=sla_mean-se, ymax=sla_mean+se),
                        pch=21,size=0.6,position=position_dodge(0.8)) +
        #geom_bar(position = "dodge", stat = "identity", color = 'black') +
        #geom_errorbar(aes(ymin = sla_mean - se, ymax = sla_mean + se), width = 0.2) +
        labs(x = "Species", y = bquote("Specific leaf area " (cm^2/g)), fill = "Treatment") +
        scale_fill_manual(values = c("#a6bddb", "#fb6a4a"),
                          labels = c("ambient" = "Ambient", "warmed" = "Warmed")) +
        #scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_color_manual(values = c("ambient" = "#a6bddb", "warmed" = "#fb6a4a")) +
        theme_bw(14) +
        theme(panel.background = element_rect(fill = NA, color = "black")) +
        theme(axis.text=element_text(size=10),
              axis.title=element_text(size=14),
              strip.text = element_text(size = 14),
              legend.title = element_text(size=14),
              legend.text = element_text(size=12),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()



### Overall averages btwn treatments biomass - dot plot
# plot level KBS
kbs_bio_dot <- ggplot(biomass_sum_k3, aes(x = state, y = average_weight, fill = insecticide)) +
        geom_pointrange(aes(ymin=average_weight-se, ymax=average_weight+se), pch=21,size=1,position=position_dodge(0.2)) +
        #scale_shape_manual(name="Treatment",
        #                   values = c(1, 19),
        #                   labels=c("Herivory","Reduced Herbivory")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        labs(title=NULL, y=bquote("Biomass " ('g/0.20m'^2)), x=NULL) +
        scale_x_discrete(labels=c("ambient" = "Ambient", "warmed" = "Warmed")) +
        scale_y_continuous(breaks = c(90,120,150,180), 
                           labels = c("90", "120", "150", "180")) +
        annotate("text", x = 0.5, y=190, label = "G", size=5) +
        #ylim(90,200) +
        theme_bw(14) +
        theme(axis.text.x = element_text(size=17),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17))
# plot level UMBS
umbs_bio_dot <- ggplot(biomass_sum_u3, aes(x = state, y = average_weight, fill = insecticide)) +
        geom_pointrange(aes(ymin=average_weight-se, ymax=average_weight+se), pch=21,size=1,position=position_dodge(0.2)) +
        #scale_shape_manual(name="Treatment",
        #                   values = c(1, 19),
        #                   labels=c("Herbivory","Reduced Herbivory")) +
        scale_fill_manual(name="Treatment",
                          values = c("#FFB451", "#0b0055"),
                          labels=c("Herbivory","Reduced Herbivory")) +
        labs(title=NULL, y=NULL, x=NULL) +
        scale_y_continuous(breaks = c(30,40,50,60), 
                           labels = c("     30", "     40", "     50", "     60")) +
        annotate("text", x = 0.5, y=58, label = "H", size=5) +
        #ylim(30,65) +
        theme_bw(14) +
        theme(axis.text.x = element_text(size=17),
              axis.text.y = element_text(size=17),
              title = element_text(size=20),
              legend.text = element_text(size=17),
              legend.title = element_text(size=17)) +
        scale_x_discrete(labels=c("ambient"="Ambient","warmed"="Warmed"))
biomass_comb_dot <- ggpubr::ggarrange(kbs_bio_dot, umbs_bio_dot,
                                      ncol = 2, common.legend = T, legend="right")



### combining C, N, SLA, and biomass figs ###
png("leaf_traits_biomass_L2.png", units="in", width=10, height=11, res=300)
ggpubr::ggarrange(c_kbs,c_umbs,n_kbs,n_umbs,sla_kbs,sla_umbs,kbs_bio_dot,umbs_bio_dot,
                  ncol = 2, nrow = 4, common.legend = T, legend="right",
                  widths = c(1, 1),
                  heights = c(1, 0.9, 0.95))
dev.off()

