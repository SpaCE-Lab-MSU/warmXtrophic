# Title: Working with warmXtrophic data in R practice
# Date: 2020 June 2
# Author: Moriah Young

# Required libraries =======================================
for (package in c('tidyverse', 'googledrive', 'googlesheets4', 'ggplot2')) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package)
                library(package, character.only=T)
        }
}

# Find the file you want to import into R ====================
# From google shared folder
drive_get()
# I'm having the hardest time figuring out a way that makes the most sense and isn't a pain every 
# single time I want to download a file from the google shared folder

file.choose() #this will open up a window on your computer so that you can navigate to the file you 
# wish to download

# Data import and set working directory ==============================================
# I want to look at plant composition data from 2019 at UMBS
PlantComp <- read.csv("/Users/moriahyoung/Downloads/umbs_plantcomp_2019 - Sheet1.csv")

# Data Checks =======================================
str(PlantComp)
summary(PlantComp)
summary(PlantComp$Cover)
head(PlantComp$Cover)
names(PlantComp)

# Convert date column to date class ===============================
PlantComp$Date <- as.Date(PlantComp$Date,
                          format = "%m/%d/%y")

# view R class of data
class(PlantComp$Date)

# view results
head(PlantComp$Date)

# Subset the data ==========================================
# I want to look at the percent cover of the species Cest over one growing season and plot its 
# composition over time

CestB2 <- subset(PlantComp, Species == "Cest" & Plot == "B2")

# Create a plot =========================================
#I want to make a plot that shows the percent cover of Cest in plot B2 over one growing season

ggplot(data = CestB2, aes(x = Date, y = Cover)) +
        geom_bar(stat = "identity", fill = "purple") +
        labs(title = "Cest Composition in Plot B2 at UMBS",
             subtitle = "2019",
             x = "Date", y = "Percent Cover")

#Creating a function for this plot ==========================


# Now I will create a new column  called "treatment" for "warming" and "ambient"

warming <- c("A2", "A4", "A5", "B2", "B3", "B6", "C1", "C4", "C6", "D2", "D3", "D6")
ambient <- c("A1", "A3", "A6", "B1", "B4", "B5", "C2", "C3", "C5", "D1", "D4", "D5")

