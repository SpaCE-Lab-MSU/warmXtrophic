## TITLE:         MI Trophic Warming Field Experiment 2015-2018 Data Prep
## AUTHORS:       Phoebe Zarnetske (PLZ), Kathryn Schmidt (KS) 
## COLLABORATORS: Mark Hammond, Nina Lany
## DATA:          MI Warming x Herbivory Open Top Chamber experiment 2015-2018
## PROJECT:       "Direct and Indirect Effects of Climate Warming on Plant Communities"
## OUTPUT:        creates cleaned up data for 2015-2018 for response variables
## DATE:          June 14, 2018; July 26 PLZ added a few edits, and noted KBS 2018 is not entered online - will check w Mark

## LAST RUN:      July 26, 2018 by PLZ

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
setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/final_data/")
# setwd("/Volumes/GoogleDrive/My Drive/Research/TrophicWarming/TrophicWarming_Experiment/MIWarmHerb_FieldExperiment/data/final_data/")

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
#load("otc_data_prep.RData")

## Occasionally add this line to save your workspace.
save.image("otc_data_prep.RData")

## Read in all the data files (both sites, all years) below - I put in a few as examples
# below I'm using shorthand given that we already set the working directory to
# the final_data folder. A single period "./" means go down one directory level
# A double period "../" means go up one directory level. This is going up 2 directory
# levels: "../../"

# you may need to edit the exact filenames
# # **Note that the Google Sheets have the first line or two with example data from 
# # a previous year - these must be deleted when exporting to CSV!!
# # PLZ did this for 2017 and 2018 data at KBS and UMBS as of July 26, 2018 
# # This involved removing example rows from 2016.
# Read in KBS green-up data from all years
green16k<-read.csv("../raw_data/KBS/2016_Plant_Data/KBS_Greenup_2016.csv")
green17k<-read.csv("../raw_data/KBS/2017_Plant_Data/kbs_greenup_2017.csv")
#green18k<-read.csv("../raw_data/KBS/2018_Plant_Data/KBS_Greenup_2018.csv") ###data not available


## Clean up each dataframe so column headings are the same and 
# structure is the same (ie all columns are numeric or factor or categorical)
# Check them here.. use unique(data$variable to see where the errors are;
# If there are typos, manually change them here in script; don't change them 
# in the raw data file). Then use "as.numeric()", "as.factor()" to change the 
# data column formats (see the data codings file)
str(green16k)
str(green17k)
#str(green18k)

#remove column "Site", "Notes", and "X" from dataframe green17k
green17k$Notes<-NULL
green17k$Site<-NULL
green17k$X<-NULL

# change column headings to lower case
names(green16k)[1:5] <- tolower(names(green16k)[1:5])
names(green17k)[1:5] <- tolower(names(green17k)[1:5])
#names(green18k)[1:5] <- tolower(names(green18k)[1:5])

# double check that these files contain just that year's data
unique(green16k$date) # ok
unique(green17k$date) # ok
#unique(green18k$date)

# Add column "year"
green16k$year<-"2016"
green17k$year<-"2017"
#green18k$year<-"2018"

# Re-order columns in data.frame
green16k <- green16k[, c("plot", "species", "cover", "date", "julian", "year")]
green17k <- green17k[, c("plot", "species", "cover", "date", "julian", "year")]
#green18k <- green18k[, c("plot", "species", "cover", "date", "julian", "year")]

# Change date from factor to date: note that the date format differs
green16k$date <- as.Date(green16k$date,format="%m/%d/%Y")
green17k$date <- as.Date(green17k$date,format="%m/%d/%y") ## Stopped edits here 7/26
#green18k$date <- as.Date(green18k$date,format="%m/%d/%Y")

# Combine green-up data from KBS from years 2016, 2017, and 2018
greenk<-rbind(green16k,green17k) ##add green18k later
greenk$site<-"KBS"

# Do the same for UMBS
green16u<-read.csv("../raw_data/UMBS/2016_Plant_Data/UMBS_Greenup_2016.csv")
green17u<-read.csv("../raw_data/UMBS/2017_Plant_Data/umbs_greenup_2017.csv")
green18u<-read.csv("../raw_data/UMBS/2018_Plant_Data/umbs_greenup_2018.csv")
str(green16u)
str(green17u)
str(green18u)


#remove column "Site", "Notes", and "X" from dataframe green17u and green18u
green17u$Site<-NULL
green17u$Notes<-NULL
green18u$Site<-NULL
green18u$Notes<-NULL
green18u$X<-NULL

# change column headings to lower case
names(green16u)[1:5] <- tolower(names(green16u)[1:5])
names(green17u)[1:5] <- tolower(names(green17u)[1:5])
names(green18u)[1:5] <- tolower(names(green18u)[1:5])

# double check that these files contain just that year's data
unique(green16u$date) # ok
unique(green17u$date) # ok
unique(green18u$date) # ok

# Add column "year"
green16u$year<-"2016"
green17u$year<-"2017"
green18u$year<-"2018"

# Re-order columns in data.frame
green16u <- green16u[, c("plot", "species", "cover", "date", "julian", "year")]
green17u <- green17u[, c("plot", "species", "cover", "date", "julian", "year")]
green18u <- green18u[, c("plot", "species", "cover", "date", "julian", "year")]

# Change date from factor to date - note some are different date format
green16u$date <- as.Date(green16u$date,format="%m/%d/%Y")
green17u$date <- as.Date(green17u$date,format="%m/%d/%y")
green18u$date <- as.Date(green18u$date,format="%m/%d/%y")

# Combine green-up data from UMBS from years 2016, 2017, and 2018
greenu<-rbind(green16u,green17u,green18u)
greenu$site<-"UMBS"

# Then merge all greenup
green1618<-rbind(greenk, greenu)
# output greenup into the final_data folder
write.csv(green1618, file="green1618.csv",row.names=F)

### Pull in flowering data ###
# Read in KBS phenology data from all years
phenology15k<-read.csv("../raw_data/KBS/2015_Plant_Data/KBS_Phenology_2015.csv")
phenology16k<-read.csv("../raw_data/KBS/2016_Plant_Data/KBS_Flwr_Sdst_2016.csv")
phenology17k<-read.csv("../raw_data/KBS/2017_Plant_Data/kbs_flwr_sd_2017.csv")
#phenology18k<-read.csv("../raw_data/KBS/2018_Plant_Data/kbs_flwr_sd_2018.csv")

# Create new file including only the phenological event 'flower'
flower15k<-phenology15k[(phenology15k$event=="flower"),]
flower16k<-phenology16k[(phenology16k$action=="flower"),]
flower17k<-phenology17k[(phenology17k$Action=="flower"),]
#flower18k<-phenology18k[(phenology18k$Action=="flower"),]
str(flower15k)
str(flower16k)
str(flower17k)
#str(flower18k)

# remove unnecessary columns and rename synonym columns to compile data later
flower15k$site<-NULL
flower15k$collector<-NULL
names(flower15k)[names(flower15k)=="event"] <- "action"
flower17k$Site<-NULL
flower17k$Notes<-NULL

# change column headings to lower case
names(flower15k)[1:5] <- tolower(names(flower15k)[1:5])
names(flower16k)[1:5] <- tolower(names(flower16k)[1:5])
names(flower17k)[1:5] <- tolower(names(flower17k)[1:5])
#names(flower18k)[1:5] <- tolower(names(flower18k)[1:5])

# double check that these files contain just that year's data
unique(flower15k$date) # ok
unique(flower16k$date) # ok
unique(flower17k$date) # ok
#unique(flower18k$date) # ok

# Add column "year"
flower15k$year<-"2015"
flower16k$year<-"2016"
flower17k$year<-"2017"
#flower18k$year<-"2018"

# Re-order columns in data.frame
flower15k <- flower15k[, c("plot", "species", "action", "date", "julian", "year")]
flower16k <- flower16k[, c("plot", "species", "action", "date", "julian", "year")]
flower17k <- flower17k[, c("plot", "species", "action", "date", "julian", "year")]
#flower18k <- flower18k[, c("plot", "species", "action", "date", "julian", "year")]

# Change date from factor to 'Date' - note some are different format
flower15k$date <- as.Date(flower15k$date,format="%m/%d/%Y")
flower16k$date <- as.Date(flower16k$date,format="%m/%d/%Y")
flower17k$date <- as.Date(flower17k$date,format="%m/%d/%y")
#flower18k$date <- as.Date(flower18k$date,format="%m/%d/%Y")

# Combine flowering data from KBS from years 2016, 2017, and 2018
flowerk<-rbind(flower15k,flower16k,flower17k) ##add flower18k later
flowerk$site<-"KBS"

# Do the same for UMBS
phenology15u<-read.csv("../raw_data/UMBS/2015_Plant_Data/UMBS_Phenology_2015.csv")
head(phenology15u)
phenology16u<-read.csv("../raw_data/UMBS/2016_Plant_Data/UMBS_Flwr_Sdst_2016.csv")
phenology17u<-read.csv("../raw_data/UMBS/2017_Plant_Data/umbs_flwr_sd_2017.csv")
#phenology18u<-read.csv("../raw_data/UMBS/2018_Plant_Data/umbs_flwr_sd_2018.csv")
flower15u<-phenology15u[(phenology15u$event=="flower"),]
flower16u<-phenology16u[(phenology16u$Action=="flower"),]
flower17u<-phenology17u[(phenology17u$Action=="flower"),]
#flower18k<-phenology18k[(phenology18k$Action=="flower"),]
str(flower15u)
str(flower16u)
str(flower17u)
#str(flower18u)

# remove unnecessary columns and rename synonym columns to compile data later
flower15u$collector<-NULL
flower15u$site<-NULL
names(flower15u)[names(flower15u)=="event"] <- "action"
flower17u$Site<-NULL
flower17u$Notes<-NULL

# change column headings to lower case
names(flower15u)[1:5] <- tolower(names(flower15u)[1:5])
names(flower16u)[1:5] <- tolower(names(flower16u)[1:5])
names(flower17u)[1:5] <- tolower(names(flower17u)[1:5])
#names(flower18u)[1:5] <- tolower(names(flower18u)[1:5])

# double check that these files contain just that year's data
unique(flower15u$date) # ok
unique(flower16u$date) # ok
unique(flower17u$date) # ok
#unique(flower18k$date) # ok

# Add column "year"
flower15u$year<-"2015"
flower16u$year<-"2016"
flower17u$year<-"2017"
#flower18u$year<-"2018"

# Re-order columns in data.frame
flower15u <- flower15u[, c("plot", "species", "action", "date", "julian", "year")]
flower16u <- flower16u[, c("plot", "species", "action", "date", "julian", "year")]
flower17u <- flower17u[, c("plot", "species", "action", "date", "julian", "year")]
#flower18u <- flower18u[, c("plot", "species", "action", "date", "julian", "year")]

# Change date from factor to 'Date' - note some have different format
flower15u$date <- as.Date(flower15u$date,format="%m/%d/%Y")
flower16u$date <- as.Date(flower16u$date,format="%m/%d/%Y")
flower17u$date <- as.Date(flower17u$date,format="%m/%d/%y")
#flower18u$date <- as.Date(flower18u$date,format="%m/%d/%Y")

# Combine flowering data from UMBS from years 2016, 2017, and 2018
floweru<-rbind(flower15u,flower16u,flower17u) ## add flower18u
floweru$site<-"UMBS"

# Then merge all flowering data
flower1518<-rbind(flowerk, floweru)
# output flowering data into the final_data folder
write.csv(flower1518, file="flower1518.csv",row.names=F)


### Pull out seed data ###
# Create new file including only the phenological event 'seed'
seed15k<-phenology15k[(phenology15k$event=="seed"),]
seed16k<-phenology16k[(phenology16k$action=="seed"),]
seed17k<-phenology17k[(phenology17k$Action=="seed"),]
#seed18k<-phenology18k[(phenology18k$Action=="seed"),]
str(seed15k)
str(seed16k)
str(seed17k)
#str(seed18k)

# remove unnecessary columns and rename synonym columns to compile data later
seed15k$site<-NULL
seed15k$collector<-NULL
names(seed15k)[names(seed15k)=="event"] <- "action"
seed17k$Site<-NULL
seed17k$Notes<-NULL

# change column headings to lower case
names(seed15k)[1:5] <- tolower(names(seed15k)[1:5])
names(seed16k)[1:5] <- tolower(names(seed16k)[1:5])
names(seed17k)[1:5] <- tolower(names(seed17k)[1:5])
#names(seed18k)[1:5] <- tolower(names(seed18k)[1:5])

# Add column "year"
seed15k$year<-"2015"
seed16k$year<-"2016"
seed17k$year<-"2017"
#seed18k$year<-"2018"

# Re-order columns in data.frame
seed15k <- seed15k[, c("plot", "species", "action", "date", "julian", "year")]
seed16k <- seed16k[, c("plot", "species", "action", "date", "julian", "year")]
seed17k <- seed17k[, c("plot", "species", "action", "date", "julian", "year")]
#seed18k <- seed18k[, c("plot", "species", "action", "date", "julian", "year")]

# Change date from factor to 'Date' - note some have different format
seed15k$date <- as.Date(seed15k$date,format="%m/%d/%Y")
seed16k$date <- as.Date(seed16k$date,format="%m/%d/%Y")
seed17k$date <- as.Date(seed17k$date,format="%m/%d/%y")
#seed18k$date <- as.Date(seed18k$date,format="%m/%d/%Y")

# Combine seed data from KBS from years 2016, 2017, and 2018
seedk<-rbind(seed15k,seed16k,seed17k) ##add seed18k later
seedk$site<-"KBS"

# Do the same for UMBS
# Create new file including only the phenological event 'seed'
seed15u<-phenology15u[(phenology15u$event=="seed"),]
seed16u<-phenology16u[(phenology16u$Action=="seed"),]
seed17u<-phenology17u[(phenology17u$Action=="seed"),]
#seed18k<-phenology18k[(phenology18k$Action=="seed"),]
str(seed15u)
str(seed16u)
str(seed17u)
#str(seed18u)

# remove unnecessary columns and rename synonym columns to compile data later
seed15u$collector<-NULL
seed15u$site<-NULL
names(seed15u)[names(seed15u)=="event"] <- "action"
seed17u$Site<-NULL
seed17u$Notes<-NULL

# change column headings to lower case
names(seed15u)[1:5] <- tolower(names(seed15u)[1:5])
names(seed16u)[1:5] <- tolower(names(seed16u)[1:5])
names(seed17u)[1:5] <- tolower(names(seed17u)[1:5])
#names(seed18u)[1:5] <- tolower(names(seed18u)[1:5])

# Add column "year"
seed15u$year<-"2015"
seed16u$year<-"2016"
seed17u$year<-"2017"
#seed18u$year<-"2018"

# Re-order columns in data.frame
seed15u <- seed15u[, c("plot", "species", "action", "date", "julian", "year")]
seed16u <- seed16u[, c("plot", "species", "action", "date", "julian", "year")]
seed17u <- seed17u[, c("plot", "species", "action", "date", "julian", "year")]
#seed18u <- seed18u[, c("plot", "species", "action", "date", "julian", "year")]

# Change date from factor to 'Date' - note some have different format
seed15u$date <- as.Date(seed15u$date,format="%m/%d/%Y")
seed16u$date <- as.Date(seed16u$date,format="%m/%d/%Y")
seed17u$date <- as.Date(seed17u$date,format="%m/%d/%y")
#seed18u$date <- as.Date(seed18u$date,format="%m/%d/%Y")

# Combine seed data from UMBS from years 2016, 2017, and 2018
seedu<-rbind(seed15u,seed16u,seed17u) ## add seed18u
seedu$site<-"UMBS"

# Then merge all seed data
seed1518<-rbind(seedk, seedu)
# output phenology into the final_data folder
write.csv(seed1518, file="seed1518.csv",row.names=F)


### Pull in plant community composition data###
# Read in KBS sp. composition data from all years
spcomp15k<-read.csv("../raw_data/KBS/2015_Plant_Data/KBS_Plantcomp_2015.csv")
spcomp16k<-read.csv("../raw_data/KBS/2016_Plant_Data/KBS_Plantcomp_2016.csv")
spcomp17k<-read.csv("../raw_data/KBS/2017_Plant_Data/kbs_plant_comp_2017.csv")
#spcomp18k<-read.csv("../raw_data/KBS/2018_Plant_Data/KBS_Greenup_2018.csv") ###data not available
str(spcomp15k)
str(spcomp16k)
str(spcomp17k)
#str(spcomp18k)

# compile 2015 quadrat data to have one explanatory variable 'cover' per plot
spcomp15k$Cover.25 <- spcomp15k$Cover * .25
spcomp15kcover<-aggregate(Cover.25~Date*Plot*Species, spcomp15k, sum)
names(spcomp15kcover)[names(spcomp15kcover)=="Cover.25"] <- "new.cover"
spcomp15k<- merge(spcomp15k, spcomp15kcover)

# remove unnecessary columns and rename synonym columns to compile data later
spcomp15k$Site<-NULL
spcomp15k$Quadrat<-NULL ### quadrat data only collected in 2015
spcomp15k$Cover<-NULL
names(spcomp15k)[names(spcomp15k)=="new.cover"] <- "cover"
spcomp17k$Site<-NULL
spcomp17k$Notes<-NULL

# change column headings to lower case
names(spcomp15k)[1:5] <- tolower(names(spcomp15k)[1:5])
names(spcomp16k)[1:5] <- tolower(names(spcomp16k)[1:5])
names(spcomp17k)[1:5] <- tolower(names(spcomp17k)[1:5])
#names(spcomp18k)[1:5] <- tolower(names(spcomp18k)[1:5])

# Add column "year"
spcomp15k$year<-"2015"
spcomp16k$year<-"2016"
spcomp17k$year<-"2017"
#spcomp18k$year<-"2018"

# Re-order columns in data.frame
spcomp15k <- spcomp15k[, c("plot", "species", "cover", "date", "julian", "year")]
spcomp16k <- spcomp16k[, c("plot", "species", "cover", "date", "julian", "year")]
spcomp17k <- spcomp17k[, c("plot", "species", "cover", "date", "julian", "year")]
#spcomp18k <- spcomp18k[, c("plot", "species", "action", "date", "julian", "year")]

# Change date from factor to 'Date' - note some have different format
spcomp15k$date <- as.Date(spcomp15k$date,format="%m/%d/%Y")
spcomp16k$date <- as.Date(spcomp16k$date,format="%m/%d/%Y")
spcomp17k$date <- as.Date(spcomp17k$date,format="%m/%d/%y")
#spcomp18k$date <- as.Date(spcomp18k$date,format="%m/%d/%Y")

# Delete repeat 2015 data due to quadrats
spcomp15k<-unique( spcomp15k[ , 1:6 ] )

# Combine spcomp data from KBS from years 2016, 2017, and 2018
spcompk<-rbind(spcomp15k,spcomp16k,spcomp17k) ##add spcomp18k later
spcompk$site<-"KBS"

# Do the same for UMBS
# Read in UMBS sp. composition data from all years
spcomp15u<-read.csv("../raw_data/UMBS/2015_Plant_Data/UMBS_Plantcomp_2015.csv")
spcomp16u<-read.csv("../raw_data/UMBS/2016_Plant_Data/UMBS_Plantcomp_2016.csv")
spcomp17u<-read.csv("../raw_data/UMBS/2017_Plant_Data/umbs_plant_comp_2017.csv")
#spcomp18u<-read.csv("../raw_data/UMBS/2018_Plant_Data/umbs_greenup_2018.csv")
str(spcomp15u)
str(spcomp16u)
str(spcomp17u)
#str(spcomp18u)

# compile 2015 quadrat data to have one explanatory variable 'cover' per plot
spcomp15u$Cover.25 <- spcomp15u$Cover * .25
spcomp15ucover<-aggregate(Cover.25~Date*Plot*Species, spcomp15u, sum)
names(spcomp15ucover)[names(spcomp15ucover)=="Cover.25"] <- "new.cover"
spcomp15u<- merge(spcomp15u, spcomp15ucover)

# remove unnecessary columns and rename synonym columns to compile data later
spcomp15u$Site<-NULL
spcomp15u$Quadrat<-NULL ### quadrat data only recorded in 2015
spcomp15u$Cover<-NULL
names(spcomp15u)[names(spcomp15u)=="new.cover"] <- "cover"
names(spcomp16u)[names(spcomp16u)=="Julian_Day"] <- "Julian"
names(spcomp16u)[names(spcomp16u)=="Percent_Cover"] <- "Cover"
spcomp17u$Site<-NULL
spcomp17u$Notes<-NULL

# change column headings to lower case
names(spcomp15u)[1:5] <- tolower(names(spcomp15u)[1:5])
names(spcomp16u)[1:5] <- tolower(names(spcomp16u)[1:5])
names(spcomp17u)[1:5] <- tolower(names(spcomp17u)[1:5])
#names(spcomp18u)[1:5] <- tolower(names(spcomp18u)[1:5])

# Add column "year"
spcomp15u$year<-"2015"
spcomp16u$year<-"2016"
spcomp17u$year<-"2017"
#spcomp18u$year<-"2018"

# Re-order columns in data.frame
spcomp15u <- spcomp15u[, c("plot", "species", "cover", "date", "julian", "year")]
spcomp16u <- spcomp16u[, c("plot", "species", "cover", "date", "julian", "year")]
spcomp17u <- spcomp17u[, c("plot", "species", "cover", "date", "julian", "year")]
#spcomp18u <- spcomp18u[, c("plot", "species", "cover", "date", "julian", "year")]

# Check that it's just 1 yr per dataframe
unique(spcomp15u$date) # ok
unique(spcomp16u$date) # ok
unique(spcomp17u$date) # ok

# Change date from factor to 'Date'
spcomp15u$date <- as.Date(spcomp15u$date,format="%m/%d/%Y")
spcomp16u$date <- as.Date(spcomp16u$date,format="%m/%d/%Y")
spcomp17u$date <- as.Date(spcomp17u$date,format="%m/%d/%y")
#spcomp18u$date <- as.Date(spcomp18u$date,format="%m/%d/%Y")

# Delete repeat 2015 data due to quadrats
spcomp15u<-unique( spcomp15u[ , 1:6 ] )

# Combine spcomp data from UMBS from years 2016, 2017, and 2018
spcompu<-rbind(spcomp15u,spcomp16u,spcomp17u) ## add spcomp18u
spcompu$site<-"UMBS"

# Then merge all spcomp data
spcomp1518<-rbind(spcompk, spcompu)
# output spcomp into the final_data folder
write.csv(spcomp1518, file="spcomp1518.csv",row.names=F)


### Pull in plant herbivory data###
# Read in KBS herbivory data from all years
herb15k<-read.csv("../raw_data/KBS/2015_Plant_Data/KBS_Herbivory_2015.csv")
herb16k<-read.csv("../raw_data/KBS/2016_Plant_Data/KBS_Leaf_Herbivory_2016.csv")
herb17k<-read.csv("../raw_data/KBS/2017_Plant_Data/kbs_leaf_herbivory_2017.csv")
#herb18k<-read.csv("../raw_data/KBS/2018_Plant_Data/KBS_Greenup_2018.csv") ###data not available
str(herb15k)
str(herb16k)
str(herb17k)
#str(herb18k)

# split column "ID" into appropriate data column
herb15k <- separate(data=herb15k, col = ID, into =c("Ste","date1","date2","date3","plt","plant_number"))%>%
  unite('Date',c('date1','date2','date3'))

# remove unnecessary columns to compile data later
herb15k$site<-NULL
herb15k$Ste<-NULL
herb15k$plt<-NULL
herb15k$total<-NULL
herb15k$julian <- as.POSIXlt(herb15k$Date,format="%Y_%m_%d")
herb15k$julian <- (herb15k$julian)$yday
names(herb15k)[names(herb15k)=="damage"] <- "p_damage"
names(herb15k)[names(herb15k)=="eaten"] <- "p_eaten"
herb17k$Site<-NULL
herb17k$Notes<-NULL

# change column headings to lower case
names(herb15k)[1:7] <- tolower(names(herb15k)[1:7])
names(herb16k)[1:7] <- tolower(names(herb16k)[1:7])
names(herb17k)[1:7] <- tolower(names(herb17k)[1:7])
#names(herb18k)[1:7] <- tolower(names(herb18k)[1:7])

# check for single yr per dataframe
unique(herb15k$date)
unique(herb16k$date)
unique(herb17k$date)
#unique(herb18k$date)

# Add column "year"
herb15k$year<-"2015"
herb16k$year<-"2016"
herb17k$year<-"2017"
#herb18k$year<-"2018"

# Re-order columns in data.frame
herb15k <- herb15k[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]
herb16k <- herb16k[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]
herb17k <- herb17k[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]
#herb18k <- herb18k[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]

# Change date from factor to 'Date'
herb15k$date <- as.Date(herb15k$date,format="%Y_%m_%d")
herb16k$date <- as.Date(herb16k$date,format="%m/%d/%Y")
herb17k$date <- as.Date(herb17k$date,format="%m/%d/%y")
#herb18k$date <- as.Date(herb18k$date,format="%m/%d/%Y")

# Combine herb data from KBS from years 2016, 2017, and 2018
herbk<-rbind(herb15k,herb16k,herb17k) ##add herb18k later
herbk$site<-"KBS"

# Do the same for UMBS
# Read in UMBS sp. composition data from all years
herb15u<-read.csv("../raw_data/UMBS/2015_Plant_Data/UMBS_Herbivory_2015.csv")
herb16u<-read.csv("../raw_data/UMBS/2016_Plant_Data/UMBS_Leaf_Herbivory_2016.csv")
herb17u<-read.csv("../raw_data/UMBS/2017_Plant_Data/umbs_leaf_herbivory_2017.csv")
#herb18u<-read.csv("../raw_data/UMBS/2018_Plant_Data/umbs__2018.csv")
str(herb15u)
str(herb16u)
str(herb17u)
#str(herb18u)

# split column "ID" into appropriate data column
herb15u <- separate(data=herb15u, col = ID, into =c("Ste","date1","date2","date3","plt","plant_number"))%>%
  unite('Date',c('date1','date2','date3'))

# remove unnecessary columns to compile data later
herb15u$site<-NULL
herb15u$Ste<-NULL
herb15u$plt<-NULL
herb15u$total<-NULL
herb15u$julian <- as.POSIXlt(herb15u$Date,format="%Y_%m_%d")
herb15u$julian <- (herb15u$julian)$yday
names(herb15u)[names(herb15u)=="damage"] <- "p_damage"
names(herb15u)[names(herb15u)=="eaten"] <- "p_eaten"
herb17u$Site<-NULL
herb17u$Notes<-NULL

# change column headings to lower case
names(herb15u)[1:7] <- tolower(names(herb15u)[1:7])
names(herb16u)[1:7] <- tolower(names(herb16u)[1:7])
names(herb17u)[1:7] <- tolower(names(herb17u)[1:7])
#names(herb18u)[1:7] <- tolower(names(herb18u)[1:7])

# check for single yr per dataframe
unique(herb15u$date)
unique(herb16u$date)
unique(herb17u$date)
#unique(herb18k$date)

# Add column "year"
herb15u$year<-"2015"
herb16u$year<-"2016"
herb17u$year<-"2017"
#herb18u$year<-"2018"

# Re-order columns in data.frame
herb15u <- herb15u[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]
herb16u <- herb16u[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]
herb17u <- herb17u[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]
#herb18u <- herb18u[, c("plot", "species", "p_eaten", "p_damage", "date", "julian", "year")]

# Change date from factor to 'Date'
herb15u$date <- as.Date(herb15u$date,format="%Y_%m_%d")
herb16u$date <- as.Date(herb16u$date,format="%m/%d/%Y")
herb17u$date <- as.Date(herb17u$date,format="%m/%d/%Y")
#herb18u$date <- as.Date(herb18u$date,format="%m/%d/%Y")

# Combine herb data from UMBS from years 2016, 2017, and 2018
herbu<-rbind(herb15u,herb16u,herb17u) ## add herb18u
herbu$site<-"UMBS"

# Then merge all herb data
herb1518<-rbind(herbk, herbu)
# output herb into the final_data folder
write.csv(herb1518, file="herb1518.csv",row.names=F)

# End of script:
save.image("otc_data_prep.RData")
