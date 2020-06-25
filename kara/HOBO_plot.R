# TITLE: HOBO plots
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: Data from HOBO_paired_sensor_cleanup script
# DATA OUTPUT: Plots for HOBO data
# PROJECT: warmXtrophic
# DATE: June 2020

#Set wd and load in the data
for (package in c("tidyverse")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")
KBS_1 <- read_csv("L0/KBS/sensor_data/KBS_1.csv")
KBS_2 <- read_csv("L0/KBS/sensor_data/KBS_2.csv")
KBS_3 <- read_csv("L0/KBS/sensor_data/KBS_3.csv")

### 1H Plot ###
#Format the dates to be in POSIXlt format
KBS_1$Date_Time <- as.POSIXlt(KBS_1$Date_Time, format = "%m/%d/%y %H:%M")
test_KBS_1 <- KBS_1
test_KBS_1$month <- format(test_KBS_1$Date_Time,format="%m")
test_KBS_1$year <- format(test_KBS_1$Date_Time,format="%y")

#Aggregate data by month and year for temps
mean_monthly_warm1 <- aggregate( XH_warmed_air_1m ~ month + year , test_KBS_1 , mean )
mean_monthly_amb1 <- aggregate( XH_ambient_air_1m ~ month + year, test_KBS_1, mean)
mean_monthly1 <- merge(mean_monthly_amb1, mean_monthly_warm1, by=c("month", "year"))

mean_monthly_gather1 <- mean_monthly1 %>%
  gather(key = "treatment",value="temp", -month, -year)

#Plot the data
ggplot(mean_monthly_gather1, aes(x = as.numeric(month), y = temp)) +
  geom_point(aes(color = treatment, shape = year), size = 2) +
  ylab("Air Temperature (F)") +
  xlab("Month") +
  theme_minimal() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = round(seq(min(mean_monthly_gather1$month), max(mean_monthly_gather1$month), by = 1),1))

p1 <- ggplot(mean_monthly1, aes(x = year, y = XH_warmed_air_1m)) +
  geom_point(aes(color = month), size = 2) +
  ylab("Warmed Air Temperature (F)") +
  ylim(0, 100) +
  xlab("Year") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(mean_monthly1, aes(x = year, y = XH_ambient_air_1m)) +
  geom_point(aes(color = month), size = 2) +
  ylab("Ambient Air Temperature (F)") +
  ylim(0, 100) +
  xlab("Year") +
  theme_minimal()

ggarrange(p1, p2, ncol = 2)

ggplot(mean_monthly_gather1, aes(x = year, y = temp)) +
  geom_point(aes(color = treatment), size = 2) +
  ylab("Air Temperature (F)") +
  ylim(0, 100) +
  xlab("Year") +
  theme_minimal() + 
  labs(colour = "Treatment")

### 2H Plot ###
#Format the dates to be in POSIXlt format
KBS_2$Date_Time <- as.POSIXlt(KBS_2$Date_Time, format = "%m/%d/%y %H:%M")
test_KBS_2 <- KBS_2
test_KBS_2$month <- format(test_KBS_2$Date_Time,format="%m")
test_KBS_2$year <- format(test_KBS_2$Date_Time,format="%y")

#Aggregate data by month and year for temps
mean_monthly_warm2 <- aggregate( XH_warmed_air_1m ~ month + year , test_KBS_2 , mean )
mean_monthly_amb2 <- aggregate( XH_ambient_air_1m ~ month + year, test_KBS_2, mean)
mean_monthly2 <- merge(mean_monthly_amb2, mean_monthly_warm2, by=c("month", "year"))

mean_monthly_gather2 <- mean_monthly2 %>%
  gather(key = "treatment",value="temp", -month, -year)

#Plot the data
ggplot(mean_monthly_gather2, aes(x = as.numeric(month), y = temp)) +
  geom_point(aes(color = treatment, shape = year), size = 2) +
  ylab("Air Temperature (F)") +
  xlab("Month") +
  theme_minimal() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = round(seq(min(mean_monthly_gather2$month), max(mean_monthly_gather2$month), by = 1),1))


### 3H Plot ###
#Format the dates to be in POSIXlt format
KBS_3$Date_Time <- as.POSIXlt(KBS_3$Date_Time, format = "%m/%d/%y %H:%M")
test_KBS_3 <- KBS_3
test_KBS_3$month <- format(test_KBS_3$Date_Time,format="%m")
test_KBS_3$year <- format(test_KBS_3$Date_Time,format="%y")

#Aggregate data by month and year for temps
mean_monthly_warm3 <- aggregate( XH_warmed_air_1m ~ month + year , test_KBS_3 , mean )
mean_monthly_amb3 <- aggregate( XH_ambient_air_1m ~ month + year, test_KBS_3, mean)
mean_monthly3 <- merge(mean_monthly_amb3, mean_monthly_warm3, by=c("month", "year"))

mean_monthly_gather3 <- mean_monthly3 %>%
  gather(key = "treatment",value="temp", -month, -year)

#Plot the data
ggplot(mean_monthly_gather3, aes(x = as.numeric(month), y = temp)) +
  geom_point(aes(color = treatment, shape = year), size = 2) +
  ylab("Air Temperature (F)") +
  xlab("Month") +
  theme_minimal() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = round(seq(min(mean_monthly_gather3$month), max(mean_monthly_gather3$month), by = 1),1))
