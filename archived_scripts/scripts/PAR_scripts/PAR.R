# Title: Looking at PAR data in relation to plant composition
# Date: 2020 June 19
# Author: Moriah Young

# Required libraries ===================================================================================
for (package in c('tidyverse', 'googledrive', 'googlesheets4', 'tinytex')) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package)
                library(package, character.only=T)
        }
}

# Find the file you want to import into R ==============================================================
# Ideally I would do this directly from Google FileStream (don't have on my computer) or from the
# shared folder online, but for now I am downloading the desired files from the shared folder onto
# my desktop to be called into R

file.choose() #this will open up a window on your computer so that you can navigate to the file you 
# wish to download and it will give you the path to that file in the console. Copy and paste this 
# into the read.csv() command

# Start by looking at 2019 UMBS data so we will need to download Plant Comp and PAR data files 
# PAR was collected on 6/03, 6/18, 7/28
# Plant Comp collected on 6/04, 6/19, 7/12 - these dates are the closest dates to the PAR measurements taken
# from 2019


# Import data files and set working directory =============================================================

# Read in the PAR data
PAR2019 <- read.csv("/Users/moriahyoung/Downloads/umbs_par_2019.csv",stringsAsFactors = FALSE)

# Read in the Plant Composition data
PC <- read.csv("/Users/moriahyoung/Downloads/umbs_plantcomp_2019.csv",stringsAsFactors = FALSE)

# Read in the plot key csv
PlotKey <- read.csv("/Users/moriahyoung/Downloads/plot.csv",stringsAsFactors = FALSE)

# Get these data frames ready to work with ===============================================================
# For some reason when the this dataset is read in, there are extra columns - get rid of these:
PAR <- PAR2019[,c(1:9)]

# Convert date columns to date class 
PC$Date <- as.Date(PC$Date,
                   format = "%m/%d/%Y")
class(PC$Date) # check 

PAR$Date <- as.Date(PAR$Date,
                    format = "%m/%d/%Y")
class(PAR$Date) # check

# Filter through the PC data to select the dates that correspond with the dates that PAR was recorded to 
# create a new plant comp data frame with only those dates

PlantComp <- filter(PC, Date == c("2019-06-04", "2019-06-19", "2019-07-12"))

# Compute percent cover data as absolute ================================================================

# Remove non-plant taxa
PlantDat <- PlantComp %>%
        dplyr::filter(Species != "Brown",  # Remove the non-taxa codes
                      Species != "Bare_Ground",
                      Species != "Unknown",
                      Species != "Litter",
                      Species != "Animal_Disturbance",
                      Species != "Vert_Litter",
                      Species != "Herbicide")

# Calculate absolute cover for each plot per each date and create a new data frame with this information
AbsoluteCover <- PlantDat %>%
        group_by(Plot, Date) %>%
        dplyr::summarize(Cover = sum(Cover, na.omit=T))

View(AbsoluteCover)
# It worked!!!!!! Although the totals don't add up to me... look into this more later

# Change the dates from the AbsoluteCover to match the corresponding PAR dates ===========================
# so that we can combine the cover and PAR data frames by date

AbsoluteCover[which(AbsoluteCover$Date == as.Date('2019-06-04')), 'Date'] = as.Date('2019-06-03')
AbsoluteCover[which(AbsoluteCover$Date == as.Date('2019-06-19')), 'Date'] = as.Date('2019-06-18')
AbsoluteCover[which(AbsoluteCover$Date == as.Date('2019-07-12')), 'Date'] = as.Date('2019-07-28')

View(AbsoluteCover)

# Combine the new data frame "AC" with the "PAR" data frame to create a new data fram "PARpc" ============

PARpc <- full_join(PAR, AbsoluteCover, by = c("Date", "Plot"))
View(PARpc)

PAR_PC <- full_join(PARpc, PlotKey, by = c("Plot" = "plot"), na.rm = TRUE)
View(PAR_PC)

# Plot PAR vs. Absolute Cover

par(mar = (c(4, 4, 4, 4))) 
for(i in as.list(unique(PAR_PC$Date))){
        p <- subset(PAR_PC, Date == i)
        plot(p$Average_Ground ~ p$Cover, 
             xlab = "Absolute Percent Cover", ylab = "Average Ground PAR",
             main = i,
             col = "blue", 
             xlim = c(0, 100),
             ylim = c(0, 2000),
             pch = 19)
        text(p$Average_Ground ~ p$Cover, labels = Plot, data = p, cex = 0.6, font = 2, pos = 3)
}
                       
# Next steps: I want to visually look at the differences btw warming and ambient plot and PAR 
# measurements when graphing. Also need to figure out why the actual date isn't showing up 
# in the title of the plot. Look at KBS 2019 PAR and plant composition data as well and compare that to
# UMBS. 
