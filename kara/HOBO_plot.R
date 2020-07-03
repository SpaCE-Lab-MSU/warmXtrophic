# TITLE: HOBO plots
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: CSV files are located in the HOBO_data folder in the shared Google drive
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

###### Paired sensor plots ######
KBS_1 <- read_csv("L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1.csv")
KBS_2 <- read_csv("L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2.csv")
KBS_3 <- read_csv("L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3.csv")

## 1H Plot ##
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

## 2H Plot ##
#Format the dates to be in POSIXlt format
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


## 3H Plot ##
#Format the dates to be in POSIXlt format
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


###### Nina's old plots ######
KBS_combined <- read.csv("L1/HOBO_data/HOBO_pendant_data/KBS/KBS_combined.csv")
UMBS_combined <- read.csv("L1/HOBO_data/HOBO_pendant_data/UMBS/UMBS_combined.csv")
Pend1P_1520k<-read.csv("L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair1_1516.csv", header =T)
Pend2P_1520k<-read.csv("L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair2_1516.csv", header =T)
Pend3P_1520k<-read.csv("L1/HOBO_data/HOBO_U_H_data/KBS/KBS_pair3_1516.csv", header =T)
Pend1P_1520u<-read.csv("L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair1_1516.csv", header =T)
Pend2P_1520u<-read.csv("L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair2_1516.csv", header =T)
Pend3P_1520u<-read.csv("L1/HOBO_data/HOBO_U_H_data/UMBS/UMBS_pair3_1516.csv", header =T)

## From pendant clean up script
ggplot(KBS_combined, aes(x = Date_Time, y = Temp_F_XP_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ .) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  ylim(-100,200)+
  theme_gray() + theme(legend.position = "bottom")

ggplot(UMBS_combined, aes(x = Date_Time, y = Temp_F_XP_air_1m, color = Pendant_ID)) +
  facet_grid(. ~ Pendant_ID) +
  # geom_errorbar(aes(ymin=mean_RFU-sd, ymax=mean_RFU+sd))+
  geom_point(alpha=.5, size = 2) +
  # geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

## KBS average chamber temp per month, by year
test.pendk<-KBS_combined
head(test.pendk)
test.pendk$Month <- months(test.pendk$Date_Time)
test.pendk$Year <- format(test.pendk$Date_Time,format="%y")
mean_monthlyk <- aggregate( Temp_F_XP_air_1m ~ Month + Year , test.pendk , mean )
mean_monthlyk$Month <- factor(mean_monthlyk$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthlyk)
ggplot(mean_monthlyk, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

## KBS mean temp of the plot, per month, per year 
test2.pendk<- KBS_combined
test2.pendk$Month <- months(test2.pendk$Date_Time)
test2.pendk$Year <- format(test2.pendk$Date_Time,format="%y")
mean_monthly_plotk <- aggregate( Temp_F_XP_air_1m ~ Month + Year + Pendant_ID , test2.pendk , mean )
mean_monthly_plotk$Month <- factor(mean_monthly_plotk$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthly_plotk)
ggplot(mean_monthly_plotk, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

## UMBS average chamber temp per month, by year
test.pendu<- UMBS_combined
test.pendu$Month <- months(test.pendu$Date_Time)
test.pendu$Year <- format(test.pendu$Date_Time,format="%y")
mean_monthlyu <- aggregate( Temp_F_XP_air_1m ~ Month + Year , test.pendu , mean )
mean_monthlyu$Month <- factor(mean_monthlyu$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthlyu)
ggplot(mean_monthlyu, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

## UMBS mean temp of the plot, per month, per year 
test2.pendu<- UMBS_combined
test2.pendu$Month <- months(test.pendu$Date_Time)
test2.pendu$Year <- format(test.pendu$Date_Time,format="%y")
mean_monthly_plotu <- aggregate( Temp_F_XP_air_1m ~ Month + Year + Pendant_ID , test2.pendu , mean )
mean_monthly_plotu$Month <- factor(mean_monthly_plotu$Month, levels =c("January","February","March","April","May","June","July","August","September","October","November","December"))
head(mean_monthly_plotu)
ggplot(mean_monthly_plotu, aes(x = Month, y = Temp_F_XP_air_1m, color = Year)) +
  geom_point() +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

## For KBS paired sensors from 2015/2016
# Remove HOBO ID's from column and add 'Pendant_ID'
Pend1P_1520k$Pendant_ID<-"1"
Pend2P_1520k$Pendant_ID<-"2"
Pend3P_1520k$Pendant_ID<-"3"

# Extract only necessary columns
Pend1P_ambient_1520k <-  Pend1P_1520k[,c(2,7,13)]
names(Pend1P_ambient_1520k)[names(Pend1P_ambient_1520k)== "XH_ambient_air_1m"] <- "XH_air_1m"
Pend1P_ambient_1520k$State<-"ambient"
Pend1P_warmed_1520k <-  Pend1P_1520k[,c(2,5,13)]
names(Pend1P_warmed_1520k)[names(Pend1P_warmed_1520k)== "XH_warmed_air_1m"] <- "XH_air_1m"
Pend1P_warmed_1520k$State<-"warmed"
Pend2P_warmed_1520k <-  Pend2P_1520k[,c(2,7,13)]
names(Pend2P_warmed_1520k)[names(Pend2P_warmed_1520k)== "XH_warmed_air_1m"] <- "XH_air_1m"
Pend2P_warmed_1520k$State<-"warmed"
Pend2P_ambient_1520k <-  Pend2P_1520k[,c(2,5,13)]
names(Pend2P_ambient_1520k)[names(Pend2P_ambient_1520k)== "XH_ambient_air_1m"] <- "XH_air_1m"
Pend2P_ambient_1520k$State<-"ambient"
Pend3P_warmed_1520k <-  Pend3P_1520k[,c(2,5,13)]
names(Pend3P_warmed_1520k)[names(Pend3P_warmed_1520k)== "XH_warmed_air_1m"] <- "XH_air_1m"
Pend3P_warmed_1520k$State<-"warmed"
Pend3P_ambient_1520k <-  Pend3P_1520k[,c(2,7,13)]
names(Pend3P_ambient_1520k)[names(Pend3P_ambient_1520k)== "XH_ambient_air_1m"] <- "XH_air_1m"
Pend3P_ambient_1520k$State<-"ambient"

# Merge dataframes
New.Pend1P_1520k<-rbind(Pend1P_warmed_1520k,Pend1P_ambient_1520k)
New.Pend2P_1520k<-rbind(Pend2P_warmed_1520k,Pend2P_ambient_1520k)
New.Pend3P_1520k<-rbind(Pend3P_warmed_1520k,Pend3P_ambient_1520k)
Pend13_1520k<-rbind(New.Pend1P_1520k,New.Pend2P_1520k,New.Pend3P_1520k) 
Pend13_1520k$Site<-"KBS"

ggplot(Pend13_1520k, aes(x = Date_Time, y = XH_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ State) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  ylim(-100,200) +
  theme_gray() + 
  theme(legend.position = "bottom")

ggplot(Pend13_1520k, aes(x = Date_Time, y = XH_air_1m, color = State)) +
  facet_grid(Pendant_ID ~ .) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  ylim(-100,200)+
  theme_gray() + theme(legend.position = "bottom")

## For UMBS paired sensors from 2015/2016
# Remove HOBO ID's from coloumn and add 'Pendant_ID'
Pend1P_1520u$Pendant_ID<-"1"
Pend2P_1520u$Pendant_ID<-"2"
Pend3P_1520u$Pendant_ID<-"3"

# Extract only necessary columns
Pend1P_ambient_1520u <-  Pend1P_1520u[,c(2,7,13)]
names(Pend1P_ambient_1520u)[names(Pend1P_ambient_1520u)== "XH_ambient_air_1m"] <- "XH_air_1m"
Pend1P_ambient_1520u$State<-"ambient"
Pend1P_warmed_1520u <-  Pend1P_1520u[,c(2,5,13)]
names(Pend1P_warmed_1520u)[names(Pend1P_warmed_1520u)== "XH_warmed_air_1m"] <- "XH_air_1m"
Pend1P_warmed_1520u$State<-"warmed"
Pend2P_warmed_1520u <-  Pend2P_1520u[,c(2,7,13)]
names(Pend2P_warmed_1520u)[names(Pend2P_warmed_1520u)== "XH_warmed_air_1m"] <- "XH_air_1m"
Pend2P_warmed_1520u$State<-"warmed"
Pend2P_ambient_1520u <-  Pend2P_1520u[,c(2,5,13)]
names(Pend2P_ambient_1520u)[names(Pend2P_ambient_1520u)== "XH_ambient_air_1m"] <- "XH_air_1m"
Pend2P_ambient_1520u$State<-"ambient"
Pend3P_warmed_1520u <-  Pend3P_1520u[,c(2,5,13)]
names(Pend3P_warmed_1520u)[names(Pend3P_warmed_1520u)== "XH_warmed_air_1m"] <- "XH_air_1m"
Pend3P_warmed_1520u$State<-"warmed"
Pend3P_ambient_1520u <-  Pend3P_1520u[,c(2,7,13)]
names(Pend3P_ambient_1520u)[names(Pend3P_ambient_1520u)== "XH_ambient_air_1m"] <- "XH_air_1m"
Pend3P_ambient_1520u$State<-"ambient"

# Merge dataframes
New.Pend1P_1520u<-rbind(Pend1P_warmed_1520u,Pend1P_ambient_1520u)
New.Pend2P_1520u<-rbind(Pend2P_warmed_1520u,Pend2P_ambient_1520u)
New.Pend3P_1520u<-rbind(Pend3P_warmed_1520u,Pend3P_ambient_1520u)
Pend13_1520u<-rbind(New.Pend1P_1520u,New.Pend2P_1520u,New.Pend3P_1520u) 
Pend13_1520u$Site<-"UMBS"

ggplot(Pend13_1520u, aes(x = Date_Time, y = XH_air_1m, color = Pendant_ID)) +
  facet_grid(Pendant_ID ~ State) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  theme_gray() + theme(legend.position = "bottom")

ggplot(Pend13_1520u, aes(x = Date_Time, y = XH_air_1m, color = State)) +
  facet_grid(Pendant_ID ~ .) +
  geom_point(alpha=.5, size = 2) +
  ylab("Temperature F") +
  theme_gray() + theme(legend.position = "bottom")

# Compare ambient vs warmed
new.Pend13_1520k <- Pend13_1520u %>%  
  +   group_by(Date_Time) %>%  
  +   mutate(Temp.diff=XH_air_1m[State=="warmed"]-XH_air_1m[State=="ambient"])
