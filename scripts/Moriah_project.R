#Moriah's independent work with R to understand and compute the data compiled from the warmxtrophic project
#Moriah Young created 06/14/2019

rm(list = ls())

#Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

library(googledrive)
#this sets the working directory onto the laptop on which you are using... the dataset from drive
#will be downloaded to your local computer and saved in this working directory
setwd("~/Desktop")

#this downloads the desired dataset from google drive onto your local computer and saves it to the working directory above
drive_download("L3dat.csv", path = 'L3dat.csv', type = "csv", overwrite = TRUE)

#this reads in the data into R and allows you to see the data file
data <- read.csv('L3dat.csv')
View(data)

#subsetting data from L3 
ambient_native <- subset(data, Site == "umbs" & state == "ambient" & origin == "Native")
warmed_native <- subset(data, Site == "umbs" & state == "warmed" & origin == "Native")
ambient_exotic <- subset(data, Site == "umbs" & state == "ambient" & origin == "Exotic")
warmed_exotic <- subset (data, Site == "umbs" & state == "warmed" & origin == "Exotic")

#looking at the mean of first flower for NATIVE species in ambient and warmed plots over all years using the subset data from above
mean(ambient_native$First.flower, na.rm=TRUE)
mean(warmed_native$First.flower, na.rm=TRUE)

#looking at the mean of first flower for EXOTIC species in ambient and warmed plots over all years using the subset data from above
mean(ambient_exotic$First.flower, na.rm=TRUE)
mean(ambient_exotic$First.flower, na.rm=TRUE)

##attempting to use Nina's script from data cleaning visualization for plot greenup phenology using the L3 data file
##figuring this out - I haven't gotten this to work yet with the L3 data file

#PLOT GREENUP PHENOLOGY TIME SERIES FOR A SPECIFIC SPECIES
#this is the function
plot_cover_ts <- function (data, site, species, year){
  temp1 <- subset(data, Site == site & variable_name == species & Year == year)
  plot(temp1$Julian_day, temp1$Cover, type = "n", xlim = c(min(temp1$Julian_day), max(temp1$Julian_day)), ylim = c(0,max(temp1$Cover)), xlab = "Julian day", ylab = "Percent cover", main = paste(site, " - ", species," - ", year)) #this makes a blank plot
  
  plot_id <- as.vector(unique(temp1$Plot))
  for(p in 1:length(plot_id)){ #fill in the plot with a line for each plot
    temp2 <- subset(temp1, Plot == plot_id[p])
    temp2 <- temp2[order(temp2$Julian_day),] #make sure dataframe is ordered by date
    color_key <- ifelse(unique(temp2$state)=="warmed", "red", "blue") #color codeaccording to treatment
    lines(temp2$Julian_day, temp2$Cover, col = color_key, type = "o")
  }
  legend("topleft", col = c("blue", "red"), legend = c("ambient", "warmed"), pch = 1, lty=1) #add a legend only once	
}
##maybe this doesn't make sense because L3 data doesn't have a julian day for cover... it's only the avg cover of a species per plot over the entire growing season.

#'call' the function like this:
plot_cover_ts(data, site = "umbs", species = "Cest", year = "2019")

#look at nine most abundant species...
focal.taxa <- c("Cest", "Dasp","Frve","Popr", "Ruac","Hype", "Hiau", "Poco", "Ptaq")
par(mfrow = c(3,3))
for (i in 1:length(focal.taxa)){
  plot_cover_ts(data, site = "umbs", species = focal.taxa[i], year = "2019")
}

