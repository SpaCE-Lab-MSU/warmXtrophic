###################################
#2. create L2 plant composition data
#created by Nina Lany 08-May-2019
###################################

#this script reads in the L1 plant composition data (one csv for each site-year in warmXtrophic/data/L1/plant_composition) and creates one L2 dataset in the warmXtrophic/data/L2 directory on Google Drive.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#Check to make sure working directory is set to the github repo
if(basename(getwd())!="warmXtrophic"){cat("Plz change your working directory. It should be 'warmXtrophic'")}
#source script with useful functions
setwd("~/Documents/warmXtrophic")
source("scripts/functions.R")

#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
gs_ls()
drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L1_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#if you will be using Google Drive File Stream or Back Up and Sync (not the 'googledrive' and 'googlesheets' packages), set your working directory to the warmXtrophic folder on the shared Google Drive.
#setwd("~/Google Drive File Stream/My Drive/warmXtrophic")
#Check to make sure working directory is correct
if(basename(getwd())!="warmXtrophic"){cat("Plz change your working directory. It should be 'warmXtrophic'")}

#load the plot and taxon tables for reference
taxa <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/taxon.csv", stringsAsFactors=F)
plots <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/plot.csv", stringsAsFactors=F)
events <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/event.csv", stringsAsFactors=F)


######################
# read in L1 data files and rbind them together
setwd("~/Google Drive File Stream/My Drive/warmXtrophic/data/L1/plant_composition")
file_list <- list.files(pattern = ".csv")
newdat <- do.call(rbind,lapply(file_list, read.csv, header=T, stringsAsFactors=F))

#some data checks. If you don't get any error messages, the data passed!
data_checks(newdat)


#convert to observation-variable-value form:
newdat$observation_id <- paste(newdat$Site,newdat$Date,newdat$Plot,newdat$Species, sep = "_")
colnames(newdat)[colnames(newdat)=="Species"] <- "variable_name"
colnames(newdat)[colnames(newdat)=="Cover"] <- "value"
newdat$Julian_day <- strptime(newdat$Date, format = "%j") #convert the Date to a decimal number 001-36 for day of the year 

#write out L2 species-level observation table
write.csv(newdat, file = "~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/observation_species.csv", row.names = F)
