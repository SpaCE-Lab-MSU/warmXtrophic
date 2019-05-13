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
for (package in c("ggplot2", "splines", "plyr", "lubridate")) {
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
greenup <- aggregate( julian~plot, greenu , median ) ##not seperated by year
#greenup <- aggregate( julian~year+plot, greenu, median) ## seperated by year
flowering<- aggregate( julian~plot, floweru, median)
seedset<- aggregate (julian~plot, seedu, median)

## species composition: mean % cover of the plot 
#PLZ: see Combined_2015_Plantcomp_rawdata.R for some code to modify to generate relative abundance at the plot level, per species, then take the mean
#KS: taking the mean % cover per plot, per species
# Remove 'Bare Ground', 'Ground', etc. 
spcompu$species <- factor(spcompu$species)
spcompu<-spcompu[!(spcompu$species=="Bare Ground"),]
spcompu<-spcompu[!(spcompu$species=="Bare Ground "),]
spcompu<-spcompu[!(spcompu$species=="Bareground"),]
spcompu<-spcompu[!(spcompu$species=="Bare"),]
spcompu<-spcompu[!(spcompu$species=="Bare Groud"),]
spcompu<-spcompu[!(spcompu$species=="Bare Groud "),]
spcompu<-spcompu[!(spcompu$species=="Brown"),]
spcompu<-spcompu[!(spcompu$species=="Brown "),]
spcompu$species<-revalue(spcompu$species, c("Anspp"="Ansp"))
spcompu$species <- factor(spcompu$species)
levels(spcompu$species)

# mean % cover per species, per plot 
quad.mn = aggregate(cover ~ plot*species, data=spcompu, FUN=mean, na.rm=T)
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"
head(quad.mn)
View(quad.mn)

#convert cover to relative abundance 
#first get summed cover for all plants per plot
cov.sum = aggregate(quad.mn ~ plot, data=quad.mn, FUN=sum, na.rm=T) ### Greater than 100, due to averaged percent cover over the year
names(cov.sum)[names(cov.sum)=="quad.mn"]<-"cov.sum"
head(cov.sum)
rel.spabundances<-merge(quad.mn,cov.sum, by=c("plot"))
head(rel.spabundances)
#calculate relative percent cover per species in each plot (="relative abundance")
rel.spabundances$relab<-rel.spabundances$quad.mn/k2$cov.sum
summary(rel.spabundances)

# species diversity: richness of plot (total number of species per plot)
# across all years
library(data.table)
lvlspcompu<-spcompu
setDT(lvlspcompu)[, count := uniqueN(species), by = plot]
head(lvlspcompu)

# pull out only necessary columns
#lvlspcompu <-  lvlspcompu[,c(2,5,6,7,9)]
lvlspcompu <-  lvlspcompu[,c(2,8,9)]
head(lvlspcompu)

# delete reps
#lvlspcompu1<-unique( lvlspcompu[ , 1:5 ] )
sprichness<-unique(lvlspcompu[ ,1:3])
tail(sprichness)
#class(lvlspcompu1$year)
#lvlspcompu1$year<-as.character(lvlspcompu1$year)

# # save max 'count' per plot, per year
# #lvlspcompu1[which.max(lvlspcompu1$),]
# lvlspcompu2 <- aggregate(count ~ plot*year, lvlspcompu1, max)
# tail(lvlspcompu2)
# View(lvlspcompu2)

# plot it to see trends
ggplot(sprichness, aes(x = plot, y = count)) +
  #facet_grid(. ~ plot) +
  #geom_errorbar(aes(ymin=mean_RFU-sd, ymax=mean_RFU+sd))+
  geom_point(alpha=.5, size = 2) +
  #   geom_line(aes(group = sensitivity)) +
  ylab("Temperature F") +
  theme_minimal() + theme(legend.position = "bottom")

#### Merge plot information (treatments) with response variable outputs ####
treatment.key<-read.csv("../raw_data/Treatment_key_updated.csv")
greenup.test<-merge(greenup,treatment.key, by=c("plot"))
head(greenup)
flowering.test<-merge(flowering,treatment.key, by=c("plot"))
seedset.test<-merge(seedset,treatment.key, by=c("plot"))
rel.spabundances.test<-merge(rel.spabundances,treatment.key, by=c("plot"))
sprichness.test<-merge(sprichness,treatment.key, by=c("plot"))

# series of linear models
attach(greenup.test)
M1<-lm(julian~state*insecticide)
plot(julian~state*insecticide)
 M1
summary(M1)
t.test(data=greenup.test,julian~state)
