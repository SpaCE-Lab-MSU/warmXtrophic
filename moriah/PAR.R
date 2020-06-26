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
# Ideally I would do this directly from Google FileStream (don't have on computer) or from the
# shared folder online, but for now I am downloading the desired files from the shared folder onto
# my desktop to be called into R

file.choose() #this will open up a window on your computer so that you can navigate to the file you 
# wish to download and it will give you the path to that file in the console. Copy and paste this 
# into the read.csv() command

# Start by looking at 2019 UMBS data so we will need to download Plant Comp and PAR data files 
# PAR was collected on 6/03, 6/18, 7/28
# Plant Comp collected on 6/4, 6/19, 7/12 - these dates are the closest dates to the PAR measurements taken
# from 2019


# Import data files and set working directory =============================================================
# Read in the PAR data
PAR2019 <- read.csv("/Users/moriahyoung/Downloads/umbs_par_2019.csv")
# Read in the Plant Composition data
PC <- read.csv("/Users/moriahyoung/Downloads/umbs_plantcomp_2019.csv")

# Get these data frames ready to work with ===============================================================
# For some reason when the this dataset is read in, there are extra columns - get rid of these:
PAR <- PAR2019[,c(1:9)]

# Convert date columns to date class 
PC$Date <- as.Date(PC$Date,
                   format = "%m/%d/%y")
class(PC$Date) # check 

PAR$Date <- as.Date(PAR$Date,
                    format = "%m/%d/%y")
class(PAR$Date) # check

# Filter through the PC data to select the dates that correspond with the dates that PAR was recorded to 
# create a new plant comp data frame with only those dates

PlantComp <- filter(PC, Date == c("2020-06-04", "2020-06-19", "2020-07-12"))

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
# It worked!!!!!! Although the totals don't add up to me... look into this more

# Combine the new data frame "AbsoluteCover" with the "PAR2019" data frame ==============================
# How do you do this if the dates aren't the same?

PARpc <- full_join(PAR, AbsoluteCover, by = "Date")
View(PARpc)
# this combines the two data frames but bc the dates aren't the same they don't combine as I would like

