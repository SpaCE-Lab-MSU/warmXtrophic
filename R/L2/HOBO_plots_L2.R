# TITLE: warmXtrophic: OTC data plots
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the L1 hobo folder in the shared Google drive
# DATA OUTPUT: Plots of the data (also in the .Rmd plot file)
    ## note: plots are saved at each step in the script and merged into final figures at the end
# PROJECT: warmXtrophic
# DATE: July 2020, updated June 2021, updated April 2022

# clear all existing data
rm(list=ls())

# load in packages
library(tidyverse)
library(plotrix)
library(ggpubr)

# Set working directory from Renviron
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)

# load in the data
KBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pairedsensors_dataremoved_L1.csv"))
KBS_pend <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_dataremoved_L1.csv"))

UMBS <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pairedsensors_dataremoved_L1.csv"))
UMBS_pend <- read.csv(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.csv"))

KBS_par <- read.csv(file.path(L1_dir,"PAR/KBS_PAR_L1.csv"))
UMBS_par <- read.csv(file.path(L1_dir,"PAR/UMBS_PAR_L1.csv"))

# date is a character column - convert to date format
KBS$Date_Time <- as.POSIXct(KBS$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS$Date_Time <- as.POSIXct(UMBS$Date_Time, format = "%Y-%m-%d %H:%M")
KBS_pend$Date_Time <- as.POSIXct(KBS_pend$Date_Time, format = "%Y-%m-%d %H:%M")
UMBS_pend$Date_Time <- as.POSIXct(UMBS_pend$Date_Time, format = "%Y-%m-%d %H:%M")
KBS_par$Date_Time <- as.POSIXct(KBS_par$Date_Time, format = "%Y-%m-%d")
UMBS_par$Date_Time <- as.POSIXct(UMBS_par$Date_Time, format = "%Y-%m-%d")


#########################################
              # KBS #
#########################################

###### microstation air/soil temperatures ######

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
# this is mainly what we'll work with for figures (could always adjust the parameters as needed)
KBS_season <- KBS
KBS_season$month <- format(KBS_season$Date_Time,format="%m")
KBS_season$year <- format(KBS_season$Date_Time,format="%Y")
KBS_season$hour <- format(KBS_season$Date_Time, format="%H")

KBS_season_air <- KBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  dplyr::select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m, sensor)

# another one for both heights of air measurements
KBS_season_air2 <- KBS_season %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m,XU_warmed_air_10cm, XU_ambient_air_10cm, sensor)

# soil temps
KBS_season_soil <- KBS_season %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm, sensor)

# soil moisture
KBS_season_soilmo <- KBS_season %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XH_warmed_soil_moisture_5cm, XH_ambient_soil_moisture_5cm, sensor)

# create new dataframes for temperatures averaged for plotting
KBS_avg_month <- KBS_season_air %>%  # by month and year, 1m temp only
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

KBS_avg_year <- KBS_season_air %>%  # by year, 1m temp only
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
  group_by(year, treatment) %>%
  summarize(count = n(),
            average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))
KBS_avg_year_air <- KBS_season_air2 %>%  # by year, 1m and 10cm; also taking sensor-level average here
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
        group_by(year, sensor, treatment) %>%
        summarize(average_temp = mean(temp, na.rm = TRUE),
                  se = std.error(temp, na.rm = TRUE))

KBS_avg_year_air2 <- KBS_avg_year_air %>%  # summarizing over all sensors 
        group_by(year, treatment) %>%
        summarize(avg = mean(average_temp, na.rm = TRUE),
                  se = std.error(average_temp, na.rm = TRUE))

KBS_avg_air <- KBS_season_air2 %>%  # overall
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor)

KBS_avg_year_soil <- KBS_season_soil %>%  # soil temp by year
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
        group_by(year, treatment, sensor) %>%
        summarize(count = n(),
                  average = mean(temp, na.rm = TRUE),
                  se = std.error(temp, na.rm = TRUE))
KBS_avg_year_soil2 <- KBS_avg_year_soil %>%  # summarizing over all sensors 
        group_by(year, treatment) %>%
        summarize(average_temp = mean(average, na.rm = TRUE),
                  se = std.error(average, na.rm = TRUE))

KBS_avg_year_soilmo <- KBS_season_soilmo %>%  # soil moisture by year
        gather(key = "treatment", value = "moisture", -year, -month, -hour, -Date_Time, -sensor) %>%
        group_by(year, treatment, sensor) %>%
        summarize(count = n(),
                  average = mean(moisture, na.rm = TRUE),
                  se = std.error(moisture, na.rm = TRUE))
KBS_avg_year_soilmo2 <- KBS_avg_year_soilmo %>%  # summarizing over all sensors 
        group_by(year, treatment) %>%
        summarize(average_moist = mean(average, na.rm = TRUE),
                  se = std.error(average, na.rm = TRUE))

KBS_soil_merged <- rbind(KBS_avg_year_soil2, KBS_avg_year_soilmo2)

# figures #
# store the data plots - to be combined with UMBS plots later in the script
# this plot is not averaged by sensor
k_month <- ggplot(KBS_avg_month, aes(x = month, y = average_temp, fill = treatment)) + 
  facet_grid(.~year) +
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(x = NULL, y = NULL, fill = "Treatment")

# this plot is not averaged by sensor
k_year <- ggplot(KBS_avg_year, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha=0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "KBS", x=NULL, y = NULL, fill = "Treatment")

Fig1_kbs <- ggplot(KBS_avg_year_air2, aes(x=year, y=avg, fill=treatment, shape=treatment)) +
  geom_pointrange(aes(ymin = avg - se, ymax = avg + se), size=1, color="black") +
  scale_fill_manual(labels = c("Ambient (1m)", "Warmed (1m)", "Ambient (10cm)", "Warmed (10cm)"), values=c('steelblue3','#fb6a4a','steelblue3','#fb6a4a'))+
  scale_shape_manual(labels = c("Ambient (1m)", "Warmed (1m)", "Ambient (10cm)", "Warmed (10cm)"), values=c(22, 22, 21, 21))+
  labs(title="KBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
  theme(legend.position="bottom") +
  theme_classic()

Fig1_overall_air_kbs <- ggplot(KBS_avg_air, aes(x=treatment, y=temp)) +
        geom_boxplot(color="black") +
        labs(title="KBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_soil_kbs <- ggplot(KBS_avg_year_soil2, aes(x=year, y=average_temp, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(ymin = average_temp - se, ymax = average_temp + se), size=1, color="black") +
        scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient", "Warmed"), values=c(21, 21))+
        labs(title="KBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_soil_line_kbs <- ggplot(KBS_avg_year_soil2, aes(x=year, y=average_temp, group=treatment, color=treatment)) +
        geom_errorbar(aes(ymin=average_temp-se, ymax=average_temp+se), width=.1,color="black",linetype="solid") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient","Warmed")) +
        labs(title=NULL,y=NULL, x=NULL) +
        ylim(14.5,18.5) +
        theme_bw(14) +
        theme(legend.title=element_text(size=15), 
              legend.text=element_text(size=14),
              axis.text.x = element_text(size=13),
              axis.text.y = element_text(size=13))
        

Fig1_soil_moist_kbs <- ggplot(KBS_avg_year_soilmo2, aes(x=year, y=average_moist, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(ymin = average_moist - se, ymax = average_moist + se), size=1, color="black") +
        scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient", "Warmed"), values=c(21, 21))+
        labs(title="KBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_soil_moist_line_kbs <- ggplot(KBS_avg_year_soilmo2, aes(x=year, y=average_moist, group=treatment, color=treatment)) +
        geom_errorbar(aes(ymin=average_moist-se, ymax=average_moist+se), width=.1,color="black",linetype="solid") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient","Warmed")) +
        labs(title="KBS",y=NULL, x=NULL) +
        ylim(0.07,0.21) +
        theme_bw(14)

Fig1_soil_kbs_dualy <- ggplot(KBS_soil_merged, aes(x=year, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(y=average_temp, ymin = average_temp - se, ymax = average_temp + se), size=1, color="black") +
        geom_pointrange(aes(y=average_moist*100, ymin = average_moist*100 - se*100, ymax = average_moist*100 + se*100), size=1, color="black") +
        scale_y_continuous(
                name = NULL,
                sec.axis = sec_axis(~./100, name=NULL)) +
        scale_fill_manual(labels = c("Ambient soil moisture (5cm)", "Warmed soil moisture (5cm)","Ambient soil temperature (5cm)","Warmed soil temperature (5cm)"), values=c('steelblue3','#fb6a4a','steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient soil moisture (5cm)", "Warmed soil moisture (5cm)","Ambient soil temperature (5cm)","Warmed soil temperature (5cm)"), values=c(22,22,21,21))+
        labs(title="KBS",y=NULL, x="Year", fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_temp_line_kbs <- ggplot(KBS_avg_year_air2, aes(x = year, y = avg, group=treatment, color = treatment, linetype=treatment)) +
        geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1,color="black",linetype="solid") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a", "#a6bddb", "#fb6a4a"),
                           labels=c("Ambient \n 1m","Warmed \n 1m","Ambient \n 10cm", "Warmed \n 10cm")) +
        scale_linetype_manual(name="Treatment",
                              values = c("solid", "solid", "dotdash", "dotdash"),
                              labels=c("Ambient \n 1m","Warmed \n 1m","Ambient \n 10cm", "Warmed \n 10cm")) +
        ylim(16,26) +
        labs(title="KBS",y=NULL, x=NULL) +
        theme_bw(14) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank()) +
        theme(legend.position="none") +
        theme(plot.title = element_text(size = 17),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14),
              axis.text.y = element_text(size=13))

###### par data ######
# old attempts at seeing how PAR and temp are correlated - not enough data to show a trend
# first, hobo data: format the hobo data to match the dates for the par data
KBS_hobo <- KBS
KBS_hobo$month <- format(KBS_hobo$Date_Time,format="%m")
KBS_hobo$year <- format(KBS_hobo$Date_Time,format="%Y")
KBS_hobo$day <- format(KBS_hobo$Date_Time,format="%d")

# create a new dataframe for comparing par + hobo across years
KBS_hobo1 <- KBS_hobo %>%
  select(Date_Time, year, month, day, XH_warmed_air_1m, XH_ambient_air_1m) %>%
  gather(key = "treatment", value = "temp", -year, -month, -day, -Date_Time) %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se_temp = std.error(temp, na.rm = TRUE)) %>%
  filter(year == "2017" & month == "07" & day == "04"|
           year == '2018' & month == '08' & day == '23'|
           year == '2019' & month == '08' & day == '02')

# create a new dataframe for comparing par and hobo for just 2019 (it has multiple par readings - the other years don't)
KBS_hobo2 <- KBS_hobo %>%
  select(Date_Time, year, month, day, XH_warmed_air_1m, XH_ambient_air_1m) %>%
  gather(key = "treatment", value = "temp", -year, -month, -day, -Date_Time) %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se_temp = std.error(temp, na.rm = TRUE)) %>%
  filter(year == "2019" & month == "05" & day == "15"|
           year == '2019' & month == '07' & day == '08'|
           year == '2019' & month == '08' & day == '02'|
           year == '2019' & month == '08' & day == '29'|
           year == '2019' & month == '09' & day == '18')

# second, par data: format the par data for the needed dates
KBS_par$month <- format(KBS_par$Date_Time,format="%m")
KBS_par$year <- format(KBS_par$Date_Time,format="%Y")
KBS_par$day <- format(KBS_par$Date_Time,format="%d")

# new dataframe for comparing par + hobo over the years
KBS_par1 <- KBS_par %>%
  select(Date_Time, Plot, year, month, day, Percent_Sunlight) %>%
  filter(year == "2017" & month == "07" & day == "04"|
           year == '2018' & month == '08' & day == '23'|
           year == '2019' & month == '08' & day == '02') %>%
  filter(Plot == "B6"|Plot == "A6"|Plot == "B3"|Plot == "B4"|Plot == "C1"|Plot == "B1")

# new dataframe for comparing par and hobo for just 2019 (it has multiple par readings - the other years don't)
KBS_par2 <- KBS_par %>%
  select(Date_Time, Plot, year, month, day, Percent_Sunlight) %>%
  filter(year == "2019" & month == "05" & day == "15"|
           year == '2019' & month == '07' & day == '08'|
           year == '2019' & month == '08' & day == '02'|
           year == '2019' & month == '08' & day == '29'|
           year == '2019' & month == '09' & day == '18') %>%
  filter(Plot == "B6"|Plot == "A6"|Plot == "B3"|Plot == "B4"|Plot == "C1"|Plot == "B1")

# new dataframe for par and hobo for 2019 - understory par instead of percent sunlight
KBS_par3 <- KBS_par %>%
  select(Date_Time, Plot, year, month, day, Average_Ground) %>%
  filter(year == "2019" & month == "05" & day == "15"|
           year == '2019' & month == '07' & day == '08'|
           year == '2019' & month == '08' & day == '02'|
           year == '2019' & month == '08' & day == '29'|
           year == '2019' & month == '09' & day == '18') %>%
  filter(Plot == "B6"|Plot == "A6"|Plot == "B3"|Plot == "B4"|Plot == "C1"|Plot == "B1")

# substituting treatment type for plot name
par_names <- c(B6="XH_warmed_air_1m", B3="XH_warmed_air_1m", C1="XH_warmed_air_1m", 
                A6="XH_ambient_air_1m", B4="XH_ambient_air_1m", B1="XH_ambient_air_1m")
KBS_par1$treatment <- as.character(par_names[KBS_par1$Plot])
KBS_par2$treatment <- as.character(par_names[KBS_par2$Plot])
KBS_par3$treatment <- as.character(par_names[KBS_par3$Plot])

# generate averages based on date & treatment
KBS_par1 <- KBS_par1 %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_par = mean(Percent_Sunlight, na.rm = TRUE),
            se_par = std.error(Percent_Sunlight, na.rm = TRUE))

KBS_par2 <- KBS_par2 %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_par = mean(Percent_Sunlight, na.rm = TRUE),
            se_par = std.error(Percent_Sunlight, na.rm = TRUE))

KBS_par3 <- KBS_par3 %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_par = mean(Average_Ground, na.rm = TRUE),
            se_par = std.error(Average_Ground, na.rm = TRUE))

# combine the formatted hobo and par data into one file
KBS_comb <- merge(KBS_hobo1,KBS_par1,by=c("year","month","day","treatment")) # over the years
KBS_comb2 <- merge(KBS_hobo2,KBS_par2,by=c("year","month","day","treatment")) # only 2019 - % sun
KBS_comb3 <- merge(KBS_hobo2,KBS_par3,by=c("year","month","day","treatment")) # only 2019 - understory

# add year/month/day column
ym <- c(paste0(KBS_comb$year,'_',KBS_comb$month,'_',KBS_comb$day))
KBS_comb <- data.frame(KBS_comb,ym)

ym2 <- c(paste0(KBS_comb2$year,'_',KBS_comb2$month,'_',KBS_comb2$day))
KBS_comb_2019 <- data.frame(KBS_comb2,ym2)

ym3 <- c(paste0(KBS_comb3$year,'_',KBS_comb3$month,'_',KBS_comb3$day))
KBS_comb_2019_2 <- data.frame(KBS_comb3,ym3)

# plot the data
# par + hobo over the years with a bar and line graph
ggplot(KBS_comb, aes(x=ym, fill=treatment, color = treatment)) +
  geom_bar( aes(y=average_temp), position = "identity",alpha = 0.5, stat = "identity", color = 'black') + 
  geom_errorbar(aes(ymin = average_temp - se_temp, ymax = average_temp + se_temp), width = 0.05,
                position = "identity", color = 'black') +
  geom_point( aes(y=average_par * 100), size=2) +
  geom_line(aes(y = average_par * 100, group = treatment)) +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~./100, name="PAR")) + 
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue')) +
  scale_color_manual(labels = c("Ambient", "Warmed"), values=c('black','white'))+
  theme_classic()

# par and hobo over the years, both as line graphs
ggplot(KBS_comb, aes(x = ym, color = treatment, shape = treatment)) +
  geom_point(aes(y = average_temp)) +
  geom_point(aes(y = average_par * 100)) +
  geom_line(aes(y = average_temp, group = treatment)) +
  geom_line(aes(y = average_par * 100, group = treatment), linetype = "dashed") +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~./100, name="PAR")) + 
  labs(x = "Year, month, day") +
  scale_color_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c('midnightblue','coral')) +
  scale_shape_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c(17,16)) +
  theme_classic() +
  theme(legend.position="bottom")

# par and hobo for only 2019, percent sunlight
ggplot(KBS_comb_2019, aes(x = ym2, color = treatment, shape = treatment)) +
  geom_point(aes(y = average_temp)) +
  geom_point(aes(y = average_par * 100)) +
  geom_line(aes(y = average_temp, group = treatment)) +
  geom_line(aes(y = average_par * 100, group = treatment), linetype = 'dashed') +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~./100, name="PAR")) + 
  labs(x = "Year, month, day") +
  scale_color_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c('midnightblue','coral')) +
  scale_shape_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c(17,16)) +
  theme_classic()+
  theme(legend.position="bottom")

# par and hobo for only 2019, understory par
# this graph needs work -  axis are wrong
ggplot(KBS_comb_2019_2, aes(x = ym3, color = treatment, shape = treatment)) +
  geom_point(aes(y = average_temp)) +
  geom_point(aes(y = average_par/100)) +
  geom_line(aes(y = average_temp, group = treatment)) +
  geom_line(aes(y = average_par/100, group = treatment), linetype = 'dashed') +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~.*100, name="PAR")) + 
  labs(x = "Year, month, day") +
  scale_color_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c('midnightblue','coral')) +
  scale_shape_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c(17,16)) +
  theme_classic()+
  theme(legend.position="bottom")

# simple linear regression - percent sunlight
KBS_comb_lm <- KBS_comb_2019 %>%
  filter(treatment == "XH_warmed_air_1m")
lm_kbs <- lm(average_temp ~ average_par, data = KBS_comb_lm)
plot(average_temp ~ average_par, data = KBS_comb_lm)
abline(lm_kbs)
summary(lm_kbs)
lm_kbs
# OR
ggplot(KBS_comb_lm, aes(x = average_par, y = average_temp)) + 
  geom_point() +
  stat_smooth(method = "lm", color = 'black') +
  theme_classic() +
  labs(y = 'Average air temperature (°C)', x = 'Average PAR')

# simple linear regression - understory par
KBS_comb_lm2 <- KBS_comb_2019_2 %>%
  filter(treatment == "XH_warmed_air_1m")
lm_kbs2 <- lm(average_temp ~ average_par, data = KBS_comb_lm2)
plot(average_temp ~ average_par, data = KBS_comb_lm2)
abline(lm_kbs2)
summary(lm_kbs2)
lm_kbs2


###### soil moisture ######
# alt plots to the one named "Fig 1" above
# create a new data frame
KBS_season_moist <- KBS
KBS_season_moist$month <- format(KBS_season_moist$Date_Time,format="%m")
KBS_season_moist$year <- format(KBS_season_moist$Date_Time,format="%y")
KBS_season_moist$hour <- format(KBS_season_moist$Date_Time, format="%H")

KBS_season_moist <- KBS_season_moist %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  dplyr::select(Date_Time, year, month, hour, XH_warmed_soil_moisture_5cm, XH_ambient_soil_moisture_5cm)

# average warmed and ambient air temps for each year + standard error
KBS_avg_moist <- KBS_season_moist %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

# plot the data
ggplot(KBS_avg_moist, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", stat = "identity", color = 'black', alpha = 1) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('#a3b4d3','#fb6a4a'))+
  theme_classic() +
  labs(title = "KBS", x="Year", y = "Soil Moisture", fill = "Treatment")

# alternate plot (not stacked)
ggplot(KBS_avg_moist, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('grey90','grey60'))+
  theme_minimal() +
  labs(title = "KBS", x="Year", y = "Average Soil Moisture", fill = "Treatment") +
  theme(legend.position = "none")


###### pendant data ######
# this graph is not in the Rmarkdown hobo plot file

# filter to include only april - august from 7am-7pm
KBS_pend_season <- KBS_pend
KBS_pend_season$month <- format(KBS_pend_season$Date_Time,format="%m")
KBS_pend_season$year <- format(KBS_pend_season$Date_Time,format="%Y")
KBS_pend_season$hour <- format(KBS_pend_season$Date_Time, format="%H")

KBS_pend_season <- KBS_pend_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, Pendant_ID, Temp_F_XP_air_1m)

# boxplot of each pendant by year - not very pretty
ggplot(KBS_pend_season, aes(x = Pendant_ID, y = Temp_F_XP_air_1m)) +
  facet_grid(.~year) +
  geom_boxplot() +
  theme_classic()


#########################################
              # UMBS #
#########################################


###### microstation air/soil temperatures ######

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
UMBS_season <- UMBS
UMBS_season$month <- format(UMBS_season$Date_Time,format="%m")
UMBS_season$year <- format(UMBS_season$Date_Time,format="%Y")
UMBS_season$hour <- format(UMBS_season$Date_Time, format="%H")

UMBS_season_air <- UMBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  dplyr::select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m, sensor)

# another one for both air heights
UMBS_season_air2 <- UMBS_season %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m, XU_warmed_air_10cm, XU_ambient_air_10cm, sensor)

# soil
UMBS_season_soil <- UMBS_season %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm, sensor)

# soil moisture
UMBS_season_soilmo <- UMBS_season %>%
        filter(month > "03") %>%
        filter(month < "09") %>%
        filter(hour > "06") %>%
        filter(hour < "20") %>%
        dplyr::select(Date_Time, year, month, hour, XH_warmed_soil_moisture_5cm, XH_ambient_soil_moisture_5cm, sensor)

# create new dataframes for temperatures averaged by year & averaged by month and year
UMBS_avg_month <- UMBS_season_air %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

UMBS_avg_year <- UMBS_season_air %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

UMBS_avg_year_air <- UMBS_season_air2 %>%
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
        group_by(year, sensor, treatment) %>%
        summarize(average_temp = mean(temp, na.rm = TRUE),
                  se = std.error(temp, na.rm = TRUE))
UMBS_avg_year_air2 <- UMBS_avg_year_air %>%  # summarizing over all sensors 
        group_by(year, treatment) %>%
        summarize(avg = mean(average_temp, na.rm = TRUE),
                  se = std.error(average_temp, na.rm = TRUE))

UMBS_avg_year_soil <- UMBS_season_soil %>%  # soil temp by year
        gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time, -sensor) %>%
        group_by(year, treatment, sensor) %>%
        summarize(count = n(),
                  average = mean(temp, na.rm = TRUE),
                  se = std.error(temp, na.rm = TRUE))
UMBS_avg_year_soil2 <- UMBS_avg_year_soil %>%  # summarizing over all sensors 
        group_by(year, treatment) %>%
        summarize(average_temp = mean(average, na.rm = TRUE),
                  se = std.error(average, na.rm = TRUE))

UMBS_avg_year_soilmo <- UMBS_season_soilmo %>%  # soil moisture by year
        gather(key = "treatment", value = "moisture", -year, -month, -hour, -Date_Time, -sensor) %>%
        group_by(year, treatment, sensor) %>%
        summarize(count = n(),
                  average = mean(moisture, na.rm = TRUE),
                  se = std.error(moisture, na.rm = TRUE))
UMBS_avg_year_soilmo2 <- UMBS_avg_year_soilmo %>%  # summarizing over all sensors 
        group_by(year, treatment) %>%
        summarize(average_moist = mean(average, na.rm = TRUE),
                  se = std.error(average, na.rm = TRUE))

UMBS_soil_merged <- rbind(UMBS_avg_year_soil2, UMBS_avg_year_soilmo2)


# store the data plots - to be combined with KBS plots later in the script
u_month <- ggplot(UMBS_avg_month, aes(x = month, y = average_temp, fill = treatment)) + 
  facet_grid(.~year) +
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(x="Month", y = NULL, fill = "Treatment")

u_year <- ggplot(UMBS_avg_year, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "UMBS", x="Year", y = NULL, fill = "Treatment")

# OTC data plots
Fig1_umbs <- ggplot(UMBS_avg_year_air2, aes(x=year, y=avg, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(ymin = avg - se, ymax = avg + se), size=1, color="black") +
        scale_fill_manual(labels = c("Ambient (1m)", "Warmed (1m)", "Ambient (10cm)", "Warmed (10cm)"), values=c('steelblue3','#fb6a4a','steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient (1m)", "Warmed (1m)", "Ambient (10cm)", "Warmed (10cm)"), values=c(22, 22, 21, 21))+
        labs(title="UMBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_soil_umbs <- ggplot(UMBS_avg_year_soil2, aes(x=year, y=average_temp, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(ymin = average_temp - se, ymax = average_temp + se), size=1, color="black") +
        scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient", "Warmed"), values=c(21, 21))+
        labs(title="UMBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_soil_line_umbs <- ggplot(UMBS_avg_year_soil2, aes(x=year, y=average_temp, group=treatment, color=treatment)) +
        geom_errorbar(aes(ymin=average_temp-se, ymax=average_temp+se), width=.1,color="black",linetype="solid") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient","Warmed")) +
        labs(title=NULL,y=NULL, x=NULL) +
        ylim(14.5, 18.5) +
        theme_bw(14) +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank()) +
        theme(legend.title=element_text(size=15), 
              legend.text=element_text(size=14),
              axis.text.x = element_text(size=13))

Fig1_soil_moist_umbs <- ggplot(UMBS_avg_year_soilmo2, aes(x=year, y=average_moist, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(ymin = average_moist - se, ymax = average_moist + se), size=1, color="black") +
        scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient", "Warmed"), values=c(21, 21))+
        labs(title="UMBS",y=NULL, x=NULL, fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_soil_moist_line_umbs <- ggplot(UMBS_avg_year_soilmo2, aes(x=year, y=average_moist, group=treatment, color=treatment)) +
        geom_errorbar(aes(ymin=average_moist-se, ymax=average_moist+se), width=.1,color="black",linetype="solid") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a"),
                           labels=c("Ambient","Warmed")) +
        labs(title="UMBS",y=NULL, x=NULL) +
        ylim(0.07,0.21) +
        theme_bw(14)

Fig1_soil_umbs_dualy <- ggplot(UMBS_soil_merged, aes(x=year, fill=treatment, shape=treatment)) +
        geom_pointrange(aes(y=average_temp, ymin = average_temp - se, ymax = average_temp + se), size=1, color="black") +
        geom_pointrange(aes(y=average_moist*100, ymin = average_moist*100 - se*100, ymax = average_moist*100 + se*100), size=1, color="black") +
        scale_y_continuous(
                name = NULL,
                sec.axis = sec_axis(~./100, name=NULL)) +
        scale_fill_manual(labels = c("Ambient soil moisture (5cm)", "Warmed soil moisture (5cm)","Ambient soil temperature (5cm)","Warmed soil temperature (5cm)"), values=c('steelblue3','#fb6a4a','steelblue3','#fb6a4a'))+
        scale_shape_manual(labels = c("Ambient soil moisture (5cm)", "Warmed soil moisture (5cm)","Ambient soil temperature (5cm)","Warmed soil temperature (5cm)"), values=c(22,22,21,21))+
        labs(title="UMBS",y=NULL, x="Year", fill="Treatment", shape="Treatment") +
        theme(legend.position="bottom") +
        theme_classic()

Fig1_temp_line_umbs <- ggplot(UMBS_avg_year_air2, aes(x = year, y = avg, group=treatment, color = treatment, linetype=treatment)) +
        geom_errorbar(aes(ymin=avg-se, ymax=avg+se), width=.1,color="black",linetype="solid") +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_manual(name="Treatment",
                           values = c("#a6bddb", "#fb6a4a", "#a6bddb", "#fb6a4a"),
                           labels=c("Ambient \n 1m","Warmed \n 1m","Ambient \n 10cm", "Warmed \n 10cm")) +
        scale_linetype_manual(name="Treatment",
                              values = c("solid", "solid", "dotdash", "dotdash"),
                              labels=c("Ambient \n 1m","Warmed \n 1m","Ambient \n 10cm", "Warmed \n 10cm")) +
        ylim(16,26) +
        labs(title="UMBS",y=NULL, x=NULL) +
        theme_bw(14) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank()) +
        theme(plot.title = element_text(size = 17),
              legend.title=element_text(size=15), 
              legend.text=element_text(size=14))


###### par data ######
# format the hobo data to match the dates for the par data
UMBS_hobo <- UMBS
UMBS_hobo$month <- format(UMBS_hobo$Date_Time,format="%m")
UMBS_hobo$year <- format(UMBS_hobo$Date_Time,format="%Y")
UMBS_hobo$day <- format(UMBS_hobo$Date_Time,format="%d")

# create a new dataframe for comparing par and hobo across years
UMBS_hobo1 <- UMBS_hobo %>%
  select(Date_Time, year, month, day, XH_warmed_air_1m, XH_ambient_air_1m) %>%
  gather(key = "treatment", value = "temp", -year, -month, -day, -Date_Time) %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se_temp = std.error(temp, na.rm = TRUE)) %>%
  filter(year == "2017" & month == "07" & day == "03"|
           year == '2017' & month == '07' & day == '10'|
           year == '2017' & month == '07' & day == '17'|
           year == '2017' & month == '07' & day == '24'|
           year == '2017' & month == '08' & day == '09'|
           year == '2017' & month == '08' & day == '16'|
           year == '2018' & month == '08' & day == '03'|
           year == '2018' & month == '08' & day == '13'|
           year == '2018' & month == '08' & day == '22'|
           year == '2018' & month == '08' & day == '31'|
           year == '2018' & month == '09' & day == '07'|
           year == '2018' & month == '09' & day == '13'|
           year == '2019' & month == '06' & day == '03'|
           year == '2019' & month == '06' & day == '18'|
           year == '2019' & month == '07' & day == '28')

# format the par data for the needed dates
UMBS_par$month <- format(UMBS_par$Date_Time,format="%m")
UMBS_par$year <- format(UMBS_par$Date_Time,format="%Y")
UMBS_par$day <- format(UMBS_par$Date_Time,format="%d")

# new dataframe for comparing par and hobo across years
UMBS_par1 <- UMBS_par %>%
  select(Date_Time, Plot, year, month, day, Percent_Sunlight) %>%
  filter(Plot == "A5"|Plot == "A6"|Plot == "B4"|Plot == "C4"|Plot == "C2"|Plot == "D2")

# substituting treatment type for plot name
par_names2 <- c(A5="XH_warmed_air_1m", C4="XH_warmed_air_1m", D2="XH_warmed_air_1m", 
               A6="XH_ambient_air_1m", B4="XH_ambient_air_1m", C2="XH_ambient_air_1m")
UMBS_par1$treatment <- as.character(par_names2[UMBS_par1$Plot])

# generate averages based on date & treatment
UMBS_par1 <- UMBS_par1 %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_par = mean(Percent_Sunlight, na.rm = TRUE),
            se_par = std.error(Percent_Sunlight, na.rm = TRUE))

# combine the formatted hobo and par data into one file
UMBS_comb <- merge(UMBS_hobo1,UMBS_par1,by=c("year","month","day","treatment")) # over the years

# add year/month/day column
ym3 <- c(paste0(UMBS_comb$year,'_',UMBS_comb$month,'_',UMBS_comb$day))
UMBS_comb <- data.frame(UMBS_comb,ym3)

# plot the data
# par and hobo for all years
ggplot(UMBS_comb, aes(x = ym3, color = treatment, shape = treatment)) +
  geom_point(aes(y = average_temp)) +
  geom_point(aes(y = average_par * 100)) +
  geom_line(aes(y = average_temp, group = treatment)) +
  geom_line(aes(y = average_par * 100, group = treatment), linetype = 'dashed') +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~./100, name="PAR")) + 
  labs(x = "Year, month, day") +
  scale_color_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c('midnightblue','coral')) +
  scale_shape_manual(labels = c("Ambient", "Warmed"), name = "Treatment", values=c(17,16)) +
  theme_classic()+
  theme(legend.position="bottom")

# simple linear regression
UMBS_comb_lm <- UMBS_comb %>%
  filter(treatment == "XH_warmed_air_1m")
lm_umbs <- lm(average_temp ~ average_par, data = UMBS_comb_lm)
plot(average_temp ~ average_par, data = UMBS_comb_lm)
abline(lm_umbs)
summary(lm_umbs)
lm_kbs
# OR
ggplot(KBS_comb_lm, aes(x = average_par, y = average_temp)) + 
  geom_point() +
  stat_smooth(method = "lm", color = 'black') +
  theme_classic() +
  labs(y = 'Average air temperature (°C)', x = 'Average PAR')


###### pendant data ######
# this graph is not in the Rmarkdown hobo plot file

#  filter for april-august from 7am-7pm
UMBS_pend_season <- UMBS_pend
UMBS_pend_season$month <- format(UMBS_pend_season$Date_Time,format="%m")
UMBS_pend_season$year <- format(UMBS_pend_season$Date_Time,format="%Y")
UMBS_pend_season$hour <- format(UMBS_pend_season$Date_Time, format="%H")

UMBS_pend_season <- UMBS_pend_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  na.omit(Temp_F_XP_air_1m) %>%
  select(Date_Time, year, month, hour, Pendant_ID, Temp_F_XP_air_1m)

# boxplot of pendant air temps - not in Rmarkdown hobo plot file
ggplot(UMBS_pend_season, aes(x = Pendant_ID, y = Temp_F_XP_air_1m)) +
  facet_grid(.~year) +
  geom_boxplot() +
  theme_classic()



### final figures ###
# arrange the data to show both KBS and UMBS plots on the same figure
# these two figures below are old
final_year <- ggarrange(k_year, u_year, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_year,
                left = text_grob("Average Air Temperature - 1m (°C)", color = "black", rot = 90))

final_month <- ggarrange(k_month, u_month, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_month,
                left = text_grob("Average Air Temperature - 1m (°C)", color = "black", rot = 90),
                top = text_grob("Year"))

# these figures below are the main ones I've been using
Fig1 <- ggarrange(Fig1_kbs, Fig1_umbs, ncol = 2, common.legend = T, legend = "right")
png("HOBO_plots_L2_air_temps.png", units="in", width=8, height=5, res=300)
annotate_figure(Fig1,
                left = text_grob("Average Air Temperature (°C)", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))
dev.off()

Fig1.1 <- ggarrange(Fig1_soil_kbs, Fig1_soil_umbs, ncol = 2, common.legend = T, legend = "right")
png("HOBO_plots_L2_soil_temps.png", units="in", width=8, height=5, res=300)
annotate_figure(Fig1.1,
                left = text_grob("Average Soil Temperature 5cm (°C)", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))
dev.off()

Fig1.2 <- ggarrange(Fig1_soil_moist_kbs, Fig1_soil_moist_umbs, ncol = 2, common.legend = T, legend = "right")
png("HOBO_plots_L2_soil_moisture.png", units="in", width=8, height=5, res=300)
annotate_figure(Fig1.2,
                left = text_grob("Average Soil Moisture 5cm (%)", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))
dev.off()

# this one below not complete, need to fix the annotation for the separate y-axis
Fig1.3 <- ggarrange(Fig1.1, Fig1.2, nrow = 2, common.legend = F)
annotate_figure(Fig1.2,
                left = text_grob("Average Moisture (%)", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))

# something went wrong with this one too        
Fig1.4 <- ggarrange(Fig1_soil_kbs_dualy, Fig1_soil_umbs_dualy, ncol = 2, common.legend = T, legend = "bottom")
annotate_figure(Fig1.2,
                left = text_grob("Soil Temperature (°C)", color = "black", rot = 90),
                right = text_grob("Soil Moisture", color = "black", rot = 270))

Fig1.5 <- ggarrange(Fig1_temp_line_kbs, Fig1_temp_line_umbs, ncol = 2, common.legend = T, legend = "right")
png("HOBO_plots_L2_air_temps_line.png", units="in", width=10, height=5, res=300)
annotate_figure(Fig1.5,
                left = text_grob("Average Air Temperature (°C)", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))
dev.off()

Fig1.6 <- ggarrange(Fig1_soil_line_kbs, Fig1_soil_line_umbs, ncol = 2, common.legend = T, legend = "right")
png("HOBO_plots_L2_soil_temps_line.png", units="in", width=10, height=5, res=300)
annotate_figure(Fig1.6,
                left = text_grob("Average Soil Temperature (°C)", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))
dev.off()

Fig1.7 <- ggarrange(Fig1_soil_moist_line_kbs, Fig1_soil_moist_line_umbs, ncol = 2, common.legend = T, legend = "right")
png("HOBO_plots_L2_soil_moist_line.png", units="in", width=10, height=5, res=300)
annotate_figure(Fig1.7,
                left = text_grob("Average Soil Moisture", color = "black", rot = 90),
                bottom = text_grob("Year", color = "black"))
dev.off()

Fig1.8 <- ggarrange(Fig1.5,Fig1.6,
                    nrow = 2, common.legend = F)
png("HOBO_plots_L2_air_soil_line.png", units="in", width=10, height=8, res=300)
annotate_figure(Fig1.8,
                left = text_grob("Soil temperature (°C)             Air temperature (°C)", color = "black", rot = 90,size=18),
                bottom = text_grob("Year", color = "black", size=18))
dev.off()

