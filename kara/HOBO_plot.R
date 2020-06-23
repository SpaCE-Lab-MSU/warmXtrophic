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
KBS_3 <- read_csv("L0/KBS/sensor_data/KBS_3.csv")

#Format the dates to be in POSIXlt format
KBS_3$Date_Time <- as.POSIXlt(KBS_3$Date_Time, format = "%m/%d/%y %H:%M")
test_KBS_3 <- KBS_3
test_KBS_3$Month <- format(test_KBS_3$Date_Time,format="%m")
test_KBS_3$Year <- format(test_KBS_3$Date_Time,format="%y")

#Aggregate data by month and year for temps
mean_monthly_warm <- aggregate( XH_warmed_air_1m ~ Month + Year , test_KBS_3 , mean )
mean_monthly_amb <- aggregate( XH_ambient_air_1m ~ Month + Year, test_KBS_3, mean)
mean_monthly <- merge(mean_monthly_amb, mean_monthly_warm, by=c("Month", "Year"))

mean_monthly_gather <- mean_monthly %>%
  gather(key = "treatment",value="temp", -Month, -Year)

#Plot the data
ggplot(mean_monthly_gather, aes(x = as.numeric(Month), y = temp)) +
  geom_point(aes(color = treatment, shape = Year), size = 2) +
  ylab("Air Temperature (F)") +
  xlab("Month") +
  theme_minimal() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = round(seq(min(mean_monthly_gather$Month), max(mean_monthly_gather$Month), by = 1),1))

ggplot(mean_monthly_gather, aes(x=as.numeric(Month), y=temp, fill=treatment)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylab("Air Temperature (F)") +
  xlab("Month") +
  theme_minimal() + theme(legend.position = "bottom") +
  scale_x_continuous(breaks = round(seq(min(mean_monthly_gather$Month), max(mean_monthly_gather$Month), by = 1),1))
