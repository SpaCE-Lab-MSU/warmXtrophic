# Title: Working with warmXtrophic data in R practice
# Date: 2020 June 2
# Author: Moriah Young

# Required libraries =======================================
for (package in c('tidyverse', 'googledrive', 'googlesheets4', 'tinytex')) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package)
                library(package, character.only=T)
        }
}

# Find the file you want to import into R ==============================================
# Ideally I would do this directly from Google FileStream (don't have on computer) or from the
# shared folder online, but for now I am downloading the desired files from the shared folder onto
# my desktop to be called into R

file.choose() #this will open up a window on your computer so that you can navigate to the file you 
# wish to download and it will give you the path to that file in the console. Copy and paste this 
# into the read.csv() command

# Data import and set working directory ===================================================
# I want to look at plant composition data from 2019 at UMBS
PC <- read.csv("/Users/moriahyoung/Downloads/umbs_plantcomp_2019 - Sheet1.csv")

# I'm also going to read in the plot.csv table so that I can add new columns in the plant comp data frame
# to add further plot information
PlotKey <- read.csv("/Users/moriahyoung/Downloads/plot.csv")

# Data Checks ==================================================================================
str(PC)
names(PC)

str(PlotKey)
names(PlotKey)

# Join the two data frames together and create a new data frame called "PlantComp" ===================

PlantComp <- full_join(PC, PlotKey, by = c("Plot" = "plot"), na.rm = TRUE)

# Convert date column to date class ============================================================
PlantComp$Date <- as.Date(PlantComp$Date,
                          format = "%m/%d/%y")

# view R class of data
class(PlantComp$Date)

# view results and other desired data checks
head(PlantComp$Date)
names(PlantComp)

# This was my first attempt when I subsetted the data and had a string of literals =====================
# I want to look at the percent cover of the species Cest in plot B2 over one growing season and plot its 
# composition over time

CestB2 <- subset(PlantComp, Species == "Cest" & Plot == "B2")

ggplot(data = CestB2, aes(x = Date, y = Cover)) +
        geom_point(stat = "identity", fill = "purple") +
        labs(title = "Cest Composition in Plot B2 at UMBS",
             subtitle = "2019",
             x = "Date", y = "Percent Cover")

# Let's create a function ========================================
# This function will select a certain species in a single plot and graph it's cover over time
cover <- PlantComp$Cover #not sure if I need to do this for anything - I don't use it in the function bellow

perc_cover_plot <- function(Species, Plot) {
        PlantComp$Species == "Species"
        PlantComp$Plot == "Plot"
        return(plot("Date", "Cover", type = "p"))
}
# this function doesn't work

# Try creating a plot using ggplot =========================================

ggplot(data = PlantComp %>% 
               filter(PlantComp, Species == "Species", Plot == "Plot"), 
        aes(x = Date, y = Cover)) +
        geom_point(stat = "identity", fill = "purple") +
        labs(title = "Cest Composition in Plot B2 at UMBS",
             subtitle = "2019",
             x = "Date", y = "Percent Cover")
# this doesn't work...
