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
# this sets the working directory onto the laptop on which you are using... the dataset from drive
# will be downloaded to your local computer and saved in this working directory
setwd("~/Desktop")

# this downloads the desired dataset from google drive onto your local computer and saves it to the working directory above
drive_download("L3dat.csv", path = 'L3dat.csv', type = "csv", overwrite = TRUE)

# this reads in the data into R
data <- read.csv('L3dat.csv')
View(data)

ambient_native <- subset(data, Site == "umbs" & state == "ambient" & origin == "Native")
warmed_native <- subset(data, Site == "umbs" & state == "warmed" & origin == "Native")
ambient_exotic <- subset(data, Site == "umbs" & state == "ambient" & origin == "Exotic")
warmed_exotic <- subset (data, Site == "umbs" & state == "warmed" & origin == "Exotic")


