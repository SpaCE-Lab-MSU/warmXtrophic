## TITLE:         MI Trophic Warming Field Experiment 2015-2018 Data Prep
## AUTHORS:       Phoebe Zarnetske (PLZ), Kathryn Schmidt (KS) 
## COLLABORATORS: Mark Hammond, Nina Lany
## DATA:          MI Warming x Herbivory Open Top Chamber experiment 2015-2018
## PROJECT:       "Direct and Indirect Effects of Climate Warming on Plant Communities"
## OUTPUT:        creates species composition data per plot and year
## DATE:          July 27 PLZ
## LAST RUN:      

## This script reads in the species composition data and generates plots
## Uses following dataset
## spcomp1518:    Plant community composition data for 2015, 2016, 2017, 2018 across KBS and UMBS

# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# set working directory (if you're not PLZ, change this to the correct path for your
# Google Drive directory. It should point to where we have /final_data
# setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/final_data/")
setwd("/Volumes/GoogleDrive/My Drive/Research/TrophicWarming/TrophicWarming_Experiment/MIWarmHerb_FieldExperiment/data/final_data/")

## Edit below for any packages you'll be using
for (package in c("ggplot2","dplyr","tidyr")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
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
#load("spcomp_2015_2018.RData")

## Occasionally add this line to save your workspace.
save.image("spcomp_2015_2018.RData")

# read in data
spcomp<-read.csv("spcomp1518.csv")

#look at data
unique(sort(spcomp$species))

#Edit species names ... Kathryn: can you do this? standardize so there is 1
# name per species. Reference "plant_codes.csv" in the /data/raw_data/ folder
spcomp$species <- as.character(spcomp$species)
spcomp$species[spcomp$species == "Hisp"] <- "Hipr"
spcomp$species[spcomp$species == "Rufl"] <- "Rual"


#convert cover to relative abundance 
#first get summed cover for all plants per plot
spcomp$date<-NULL
cov.sum = aggregate(cover ~ plot*year*julian*site, data=spcomp, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="cover"]<-"cov.sum"
head(cov.sum)
spcomp1<-merge(spcomp,cov.sum, by=c("plot","julian","year","site"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
spcomp1$relab<-spcomp1$cover/spcomp1$cov.sum
summary(spcomp1)
# check that above code worked... haven't run w new 2018 data

#merge in species trait info - if some species have no entry, add to the original
#trait lookup tables here. Use USDA plants/ Grime classification for growth forms
ktrait<-read.csv("../raw_data/KBS_Species_Traits_Comp.csv")
utrait<-read.csv("../raw_data/UMBS_Species_Traits_Comp.csv")
str(utrait)
str(ktrait)

spcomp1<-merge(spcomp1, ktrait, by=c("species"), all.x=T, all.y=T)
spcomp1<-merge(spcomp1, utrait, by=c("species"), all.x=T, all.y=T)

str(spcomp1)

save.image("spcomp_2015_2018.RData")

#merge in plot info (if not already included)
trt<-read.csv("../raw_data/Treatment_key_updated.csv", header = TRUE)
str(trt)
spcomp2<-merge(spcomp1, trt, by=c("plot"), all.x=T, all.y=T)


## Feel free to make some plots by summarizing relab by year, species, site.
## Plot is usually experimental unit, so when plotting take mean across plots (if there is 
# one value per plot).
# The script, "2016_SpComp_finaldata_by_site.R" has several analyses and 
# relative abundance plots that could be added below and run here to look at 
# differences over years.
# 
