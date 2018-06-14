## TITLE:         MI Trophic Warming Field Experiment 2015-2018 Data Prep
## AUTHORS:       Phoebe Zarnetske (PLZ), Kathryn Schmidt (KS) 
## COLLABORATORS: Mark Hammond, Nina Lany
## DATA:          MI Warming x Herbivory Open Top Chamber experiment 2015-2018
## PROJECT:       "Direct and Indirect Effects of Climate Warming on Plant Communities"
## OUTPUT:        creates cleaned up data for 2015-2018 for response variables
## DATE:          June 14, 2018

## This script reads in raw data compiled from 2015-2018 experiment data.
## This script then combines all years into 1 dataframe per response variable. 
## Add additional notes here...

## **** Makes the following datasets:
## green1618:     Greenup data for 2016, 2107, 2018 across KBS and UMBS  
## flower1518:    Flowering data for 2015, 2016, 2017, 2018 across KBS and UMBS
## seed1518:      Seed set data for 2015, 2016, 2017, 2018 across KBS and UMBS
## spcomp1518:    Plant community composition data for 2015, 2016, 2017, 2018 across KBS and UMBS
## herb1518:      Herbivory data for 2015, 2016, 2017, 2018 across KBS and UMBS

# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# set working directory (if you're not PLZ, change this to the correct path for your
# Google Drive directory. It should point to where we have /final_data
setwd("/Volumes/GoogleDrive/My Drive/Research/TrophicWarming/final_data")

## Edit below for any packages you'll be using
for (package in c("ggplot2", "splines", "lme4", "plyr", "RLRsim","lmerTest")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90),
             axis.text.y = element_text(size = 10))

# function to calculate standard errors
stderr <- function(x) sqrt(var(x)/length(x))

#************************
####** DATA IMPORT **####
#************************

# Uncomment this if you want to read in the saved workspace
#load("otc_data_prep.RData")

## Occasionally add this line to save your workspace.
save.image("otc_data_prep.RData")

## Read in all the data files (both sites, all years) below - I put in a few as examples
# below I'm using shorthand given that we already set the working directory to
# the final_data folder. A single period "./" means go down one directory level
# A double period "../" means go up one directory level. This is going up 2 directory
# levels: "../../"

# you may need to edit the exact filenames
green16k<-read.csv("../raw_data/KBS/2016_Plant_Data/KBS_Greenup_2016.csv")
green17k<-read.csv("../raw_data/KBS/2017_Plant_Data/KBS_Greenup_2017.csv")
green18k<-read.csv("../raw_data/KBS/2018_Plant_Data/KBS_Greenup_2018.csv")

## Clean up each dataframe so column headings are the same and 
# structure is the same (ie all columns are numeric or factor or categorical)
# Check them here.. use unique(data$variable to see where the errors are;
# If there are typos, manually change them here in script; don't change them 
# in the raw data file). Then use "as.numeric()", "as.factor()" to change the 
# data column formats (see the data codings file)
str(green16k)
str(green17k)
str(green18k)

greenk<-cbind(green16k,green17k,green18k)
greenk$site<-"KBS"

# Do the same for UMBS
green16u<-read.csv("../raw_data/UMBS/2016_Plant_Data/UMBS_Greenup_2016.csv")

# etc.
greenu<-cbind(green16u,green17u,green18u)
greenu$site<-"UMBS"



# Then merge all greenup
green1618<-cbind(greenk, greenu)
# output greenup into the final_data folder
write.csv("green1618.csv", row.names=FALSE)

# Repeat for other response variables

# End of script:
save.image("otc_data_prep.RData")
