## TITLE:         MI Trophic Warming Field Experiment Shrub Exclusion or Inclusion
## AUTHORS:       Phoebe Zarnetske (PLZ), Kathryn Schmidt (KS) 
## COLLABORATORS: Mark Hammond, Nina Lany
## DATA:          MI Warming x Herbivory Open Top Chamber experiment 2015-2018
## PROJECT:       "Direct and Indirect Effects of Climate Warming on Plant Communities"
## OUTPUT:        summarizes data and detects outliers across response variables
## DATE:          July 2, 2018

## This script reads in raw data compiled from 2015-2018 experiment data for each response
## This script summarizes the response variable and detects outliers

# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# set working directory 
# Google Drive directory. It should point to where we have /final_data
setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/final_data/")

## Edit below for any packages you'll be using
for (package in c("ggplot2", "splines", "lme4", "plyr", "RLRsim","lmerTest", "lubridate")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

# Read in combined raw data (output of script titled: "data_prep_2015_2018") for each response
green1618<-read.csv("green1618.csv")
flower1518<-read.csv("flower1518.csv")
seed1518<-read.csv("seed1518.csv")
spcomp1518<-read.csv("spcomp1518.csv")
herb1518<-read.csv("herb1518.csv")

# Pull out data for UMBS site
greenu<-green1618[(green1618$site == "UMBS"),]
floweru<-flower1518[(flower1518$site == "UMBS"),]
seedu<-seed1518[(seed1518$site == "UMBS"),]
spcompu<-spcomp1518[(spcomp1518$site == "UMBS"),]
herbu<-herb1518[(herb1518$site == "UMBS"),]

# fix problem with plot levles in data
greenu$plot<-revalue(greenu$plot, c("A5 "="A5"))
levels(greenu$plot)

## Greenup  (emergence) would be something like median greenup of the plot (take the median julian date for the entire plot)
# same fore flowering, and seed set
greenup <- aggregate( julian~plot, greenu , median )
flowering<- aggregate( julian~plot, floweru, median)
seedset<- aggregate (julian~plot, seedu, median)

## species composition: mean % cover of the plot 
#PLZ: see Combined_2015_Plantcomp_rawdata.R for some code to modify to generate relative abundance at the plot level, per species, then take the mean
#average sub-quadrats for plots
#KS: taking the mean % cover per plot, per species
head(spcompu)
quad.mn = aggregate(cover ~ plot*species, data=spcompu, FUN=mean, na.rm=T)
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"
head(quad.mn)

#convert cover to relative abundance 
#first get summed cover for all plants per plot
cov.sum = aggregate(quad.mn ~ plot, data=quad.mn, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="quad.mn"]<-"cov.sum"
head(cov.sum)
k2<-merge(quad.mn,cov.sum, by=c("plot"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
k2$relab<-k2$quad.mn/k2$cov.sum
summary(k2)


# species diversity: richness of plot (total number of species per plot)
# remove "Bare Ground" and "Brown"
spcompu <- subset(spcompu, species != "Brown")
spcompu <- subset(spcompu, species != "Bare Ground")
spcompu <- subset(spcompu, species != "Bareground")

# levels of species per plot
lvlspcompu<-spcompu
library(data.table)
setDT(lvlspcompu)[, count := uniqueN(species), by = plot]
head(lvlspcompu)

# pull out only necessary columns
lvlspcompu <-  lvlspcompu[,c(2,5,6,7,9)]
head(lvlspcompu)

# delete reps
lvlspcompu1<-unique( lvlspcompu[ , 1:5 ] )
tail(lvlspcompu1)

# save max 'count' per plot, per year
#lvlspcompu1[which.max(lvlspcompu1$),]
lvlspcompu2 <- aggregate(count ~ plot*year, lvlspcompu1, max)
tail(lvlspcompu2)

# series of linear models
M1<lm(greenup~warming*insecticide)


