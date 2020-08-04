# TITLE: OTC data plots
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the HOBO_data folder in the shared Google drive
# DATA OUTPUT: Plots of each graph are in the HOBO_plot.pdf in Github
    ## note: plots are saved for each station and merged into a final figure at the bottom of the script
# PROJECT: warmXtrophic
# DATE: July 2020

# clear all existing data
rm(list=ls())

# load in packages and set working directory
for (package in c("tidyverse", "plotrix", "ggpubr")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# load in the data
KBS_1 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair1_L1.csv")
KBS_2 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair2_L1.csv")
KBS_3 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/KBS/KBS_pair3_L1.csv")
KBS_pend <- read_csv("L1/HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.csv")

UMBS_1 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair1_L1.csv")
UMBS_2 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair2_L1.csv")
UMBS_3 <- read_csv("L1/HOBO_data/HOBO_paired_sensor_data/UMBS/UMBS_pair3_L1.csv")
UMBS_pend <- read_csv("L1/HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.csv")

KBS_par <- read_csv("L1/PAR_data/KBS_PAR_L1.csv")
UMBS_par <- read_csv("L1/PAR_data/UMBS_PAR_L1.csv")




#########################################
              # KBS #
#########################################




###### microstation air temperatures ######
# merge the data
KBS <- rbind(KBS_1, KBS_2, KBS_3)

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
KBS_season <- KBS
KBS_season$month <- format(KBS_season$Date_Time,format="%m")
KBS_season$year <- format(KBS_season$Date_Time,format="%Y")
KBS_season$hour <- format(KBS_season$Date_Time, format="%H")

KBS_season <- KBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

KBS_season2 <- KBS_season %>%
  filter(month == "07") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
KBS_avg_month <- KBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

KBS_avg_year <- KBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(count = n(),
            average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

KBS_avg_july <- KBS_season2 %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

# plot the data - k_month shows the individual monthly averages over time, while k_year shows overall averages per year
k_month <- ggplot(KBS_avg_month, aes(x = month, y = average_temp, fill = treatment)) + 
  facet_grid(.~year) +
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(x = NULL, y = NULL, fill = "Treatment")

k_year <- ggplot(KBS_avg_year, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "KBS", x=NULL, y = NULL, fill = "Treatment")

# alt k_year graph
ggplot(KBS_avg_year, aes(x=year, y=average_temp, fill=treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('grey90','grey60'))+
  theme_minimal() +
  labs(title = "KBS", x="Year", y = "Average Air Temperature - 1m (°C)", fill = "Treatment") +
  theme(legend.position = "bottom")

k_july <- ggplot(KBS_avg_july, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.1,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "KBS — July", x=NULL, y = NULL, fill = "Treatment")




## note to self — how to incorporate pendant temperatures to look at warming over time?
###### pendant air temperatures ######
# format the data 
KBS_pend$year <- format(KBS_pend$Date_Time,format="%Y")

KBS_pend_4P <- KBS_pend %>%
  filter(Pendant_ID == "4P") %>%
  group_by(year) %>%
  summarize(average_temp = mean(Temp_F_XP_air_1m, na.rm = TRUE),
            se = std.error(Temp_F_XP_air_1m, na.rm = TRUE))
KBS_pend_5P <- KBS_pend %>%
  filter(Pendant_ID == "5P") %>%
  group_by(year) %>%
  summarize(average_temp = mean(Temp_F_XP_air_1m, na.rm = TRUE),
            se = std.error(Temp_F_XP_air_1m, na.rm = TRUE))




###### par data ######
# format the hobo data to match the dates for the par data — determine mean temp for the needed dates
KBS <- rbind(KBS_1, KBS_2, KBS_3)

KBS_hobo <- KBS
KBS_hobo$month <- format(KBS_hobo$Date_Time,format="%m")
KBS_hobo$year <- format(KBS_hobo$Date_Time,format="%Y")
KBS_hobo$day <- format(KBS_hobo$Date_Time,format="%d")

# comparing par + hobo across years
KBS_hobo1 <- KBS_hobo %>%
  select(Date_Time, year, month, day, XH_warmed_air_1m, XH_ambient_air_1m) %>%
  gather(key = "treatment", value = "temp", -year, -month, -day, -Date_Time) %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se_temp = std.error(temp, na.rm = TRUE)) %>%
  filter(year == "2017" & month == "07" & day == "04"|
           year == '2018' & month == '08' & day == '23'|
           year == '2019' & month == '08' & day == '02')

# comparing par and hobo for just 2019 (it has multiple par readings - the other years don't)
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

# format the par data for the needed dates
KBS_par$month <- format(KBS_par$Date_Time,format="%m")
KBS_par$year <- format(KBS_par$Date_Time,format="%Y")
KBS_par$day <- format(KBS_par$Date_Time,format="%d")

# comparing par + hobo across years
KBS_par1 <- KBS_par %>%
  select(Date_Time, Plot, year, month, day, Percent_Sunlight) %>%
  filter(year == "2017" & month == "07" & day == "04"|
           year == '2018' & month == '08' & day == '23'|
           year == '2019' & month == '08' & day == '02') %>%
  filter(Plot == "B6"|Plot == "A6"|Plot == "B3"|Plot == "B4"|Plot == "C1"|Plot == "B1")

# comparing par and hobo for just 2019 (it has multiple par readings - the other years don't)
KBS_par2 <- KBS_par %>%
  select(Date_Time, Plot, year, month, day, Percent_Sunlight) %>%
  filter(year == "2019" & month == "05" & day == "15"|
           year == '2019' & month == '07' & day == '08'|
           year == '2019' & month == '08' & day == '02'|
           year == '2019' & month == '08' & day == '29'|
           year == '2019' & month == '09' & day == '18') %>%
  filter(Plot == "B6"|Plot == "A6"|Plot == "B3"|Plot == "B4"|Plot == "C1"|Plot == "B1")

# add treatment types to the plots
par_names <- c(B6="XH_warmed_air_1m", B3="XH_warmed_air_1m", C1="XH_warmed_air_1m", 
                A6="XH_ambient_air_1m", B4="XH_ambient_air_1m", B1="XH_ambient_air_1m")
KBS_par1$treatment <- as.character(par_names[KBS_par1$Plot])
KBS_par2$treatment <- as.character(par_names[KBS_par2$Plot])

# remove plot names and average based on date & treatment
KBS_par1 <- KBS_par1 %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_par = mean(Percent_Sunlight, na.rm = TRUE),
            se_par = std.error(Percent_Sunlight, na.rm = TRUE))

KBS_par2 <- KBS_par2 %>%
  group_by(month, year, day, treatment) %>%
  summarize(average_par = mean(Percent_Sunlight, na.rm = TRUE),
            se_par = std.error(Percent_Sunlight, na.rm = TRUE))

# combine the formatted hobo and par data into one file + add a year/month/day
KBS_comb <- merge(KBS_hobo1,KBS_par1,by=c("year","month","day","treatment"))
KBS_comb2 <- merge(KBS_hobo2,KBS_par2,by=c("year","month","day","treatment"))

ym <- c(paste0(KBS_comb$year,'_',KBS_comb$month,'_',KBS_comb$day))
KBS_comb <- data.frame(KBS_comb,ym)

ym2 <- c(paste0(KBS_comb2$year,'_',KBS_comb2$month,'_',KBS_comb2$day))
KBS_comb_2019 <- data.frame(KBS_comb2,ym2)

# plot the data
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

ggplot(KBS_comb, aes(x = ym, color = treatment, shape = treatment)) +
  geom_point(aes(y = average_temp)) +
  geom_point(aes(y = average_par * 100)) +
  geom_line(aes(y = average_temp, group = treatment)) +
  geom_line(aes(y = average_par * 100, group = treatment)) +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~./100, name="PAR")) + 
  theme_classic()+
  theme(legend.position="bottom")

ggplot(KBS_comb_2019, aes(x = ym2, color = treatment, shape = treatment)) +
  geom_point(aes(y = average_temp)) +
  geom_point(aes(y = average_par * 100)) +
  geom_line(aes(y = average_temp, group = treatment)) +
  geom_line(aes(y = average_par * 100, group = treatment)) +
  scale_y_continuous(
    name = "Air Temperature (°C)",
    sec.axis = sec_axis(~./100, name="PAR")) + 
  theme_classic()+
  theme(legend.position="bottom")





#########################################
              # UMBS #
#########################################




###### air temperature ######
# merge the data
UMBS <- rbind(UMBS_1, UMBS_2, UMBS_3)

# create new dataframe with only data from april - august from 7 AM - 7 PM (growing season during the day)
UMBS_season <- UMBS
UMBS_season$month <- format(UMBS_season$Date_Time,format="%m")
UMBS_season$year <- format(UMBS_season$Date_Time,format="%Y")
UMBS_season$hour <- format(UMBS_season$Date_Time, format="%H")

UMBS_season <- UMBS_season %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

UMBS_season2 <- UMBS_season %>%
  filter(month == "07") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XH_warmed_air_1m, XH_ambient_air_1m)

# create new dataframes for temperatures averaged by year & averaged by month and year
UMBS_avg_month <- UMBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

UMBS_avg_year <- UMBS_season %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

UMBS_avg_july <- UMBS_season2 %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(month, year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

# plot the data - u_month shows the individual monthly averages over time, while u_year shows overall averages per year
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

u_july <- ggplot(UMBS_avg_july, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(position = "identity", alpha = 0.5, stat = "identity", color = 'black') +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.1,
                position = "identity") +
  ylim(0, 30) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('darkblue','lightblue'))+
  theme_classic() +
  labs(title = "UMBS — July", x="Year", y = NULL, fill = "Treatment")



### final figures ###
# arrange the data to show both KBS and UMBS plots on the same figure
final_year <- ggarrange(k_year, u_year, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_year,
                left = text_grob("Average Air Temperature - 1m (°C)", color = "black", rot = 90))

final_month <- ggarrange(k_month, u_month, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_month,
                left = text_grob("Average Air Temperature - 1m (°C)", color = "black", rot = 90),
                top = text_grob("Year"))

final_july <- ggarrange(k_july, u_july, nrow = 2, common.legend = TRUE, legend = "bottom")
annotate_figure(final_july,
                left = text_grob("Average Air Temperature - 1m (°C)", color = "black", rot = 90),
                top = text_grob("Year"))








## soil temperatures ##

# create a new data frame
KBS <- rbind(KBS_1, KBS_2, KBS_3)
KBS_season_soil <- KBS
KBS_season_soil$month <- format(KBS_season_soil$Date_Time,format="%m")
KBS_season_soil$year <- format(KBS_season_soil$Date_Time,format="%y")
KBS_season_soil$hour <- format(KBS_season_soil$Date_Time, format="%H")

KBS_season_soil <- KBS_season_soil %>%
  filter(month > "03") %>%
  filter(month < "09") %>%
  filter(hour > "06") %>%
  filter(hour < "20") %>%
  select(Date_Time, year, month, hour, XU_warmed_soil_temp_5cm, XU_ambient_soil_temp_5cm)

# average warmed and ambient air temps for each year + standard error
KBS_avg_soil <- KBS_season_soil %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

ggplot(KBS_avg_soil, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('grey90','grey60'))+
  theme_minimal() +
  labs(title = "KBS", x="Year", y = "Average Soil Temperature (°C)", fill = "Treatment") +
  theme(legend.position = "bottom")

## soil moisture ##

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
  select(Date_Time, year, month, hour, XH_warmed_soil_moisture_5cm, XH_ambient_soil_moisture_5cm)

# average warmed and ambient air temps for each year + standard error
KBS_avg_moist <- KBS_season_moist %>%
  gather(key = "treatment", value = "temp", -year, -month, -hour, -Date_Time) %>%
  group_by(year, treatment) %>%
  summarize(average_temp = mean(temp, na.rm = TRUE),
            se = std.error(temp, na.rm = TRUE))

ggplot(KBS_avg_moist, aes(x = year, y = average_temp, fill = treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = average_temp - se, ymax = average_temp + se), width = 0.2,
                position = position_dodge(0.9)) +
  scale_fill_manual(labels = c("Ambient", "Warmed"), values=c('grey90','grey60'))+
  theme_minimal() +
  labs(title = "KBS", x="Year", y = "Average Soil Moisture", fill = "Treatment") +
  theme(legend.position = "none")

