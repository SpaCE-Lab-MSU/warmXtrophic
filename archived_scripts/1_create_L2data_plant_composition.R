###################################
#1. create L2 plant composition data
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

#set wd to github repo
setwd("~/Documents/warmXtrophic")
#Check to make sure working directory is set to the github repo
if(basename(getwd())!="warmXtrophic"){cat("Plz change your working directory. It should be 'warmXtrophic'")}
#source script with useful functions
source("scripts/functions.R")

#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
gs_ls()
drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L1_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#if you will be using Google Drive File Stream or Back Up and Sync (not the 'googledrive' and 'googlesheets' packages), set your working directory to the warmXtrophic folder on the shared Google Drive.
#setwd("~/Google Drive File Stream/My Drive/warmXtrophic")


#######################################
#load the taxon, plot and event lookup tables for reference:
#get google_id for taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('taxon.csv',name))%>%
		 select(id) %>% 
		 unlist()
taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#taxa <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/taxon.csv", stringsAsFactors=F)

#get google_id for plot table and load:
google_id <- L2_data_dir %>% filter(grepl('plot.csv',name))%>%
		 select(id) %>% 
		 unlist()
plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#plots <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/plot.csv", stringsAsFactors=F)

#get google_id for event table and load:
google_id <- L2_data_dir %>% filter(grepl('event',name))%>%
		 select(id) %>% 
		 unlist()
events <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
events <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

######################
# read in all of the L1 plant composition data files and rbind them together
setwd("~/Google Drive File Stream/My Drive/warmXtrophic/data/L1/plant_composition")
file_list <- list.files(pattern = ".csv")
newdat <- do.call(rbind,lapply(file_list, read.csv, header=T, stringsAsFactors=F))
newdat$Date = as.Date(newdat$Date)
#some data checks. If you don't get any error messages, the data passed!
data_checks(newdat)


#convert to observation-variable-value form:
newdat$observation_id <- paste(newdat$Site,newdat$Date,newdat$Plot, sep = "_")
colnames(newdat)[colnames(newdat)=="Species"] <- "variable_name"
colnames(newdat)[colnames(newdat)=="Cover"] <- "value"
newdat$Julian_day <- format(newdat$Date,"%j") #convert the Date to a decimal number 001-36 for day of the year 

L2dat <- newdat[,c("observation_id", "Site", "Year", "Date", "Julian_day", "Plot", "variable_name", "value")]


#write out L2 species-level observation table
write.csv(L2dat, file = "~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/observation_species.csv", row.names = F)

