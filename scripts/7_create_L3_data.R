###################################
#7. create L3 data
#created by Nina Lany 12-Jun-2019
###################################

#this script reads in the L2 data, summarizes it to annual values for each plot, and writes it out as a csv in the L3 directory on Google Drive.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

setwd("~/Documents/warmXtrophic")
#Check to make sure working directory is set to the github repo
if(basename(getwd())!="warmXtrophic"){cat("Plz change your working directory. It should be 'warmXtrophic'")}
#source script with useful functions
source("scripts/functions.R")

#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
#gs_ls()
#drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#if you will be using Google Drive File Stream or Back Up and Sync (not the 'googledrive' and 'googlesheets' packages), set your working directory to the warmXtrophic folder on the shared Google Drive.
setwd("~/Google Drive File Stream/My Drive/warmXtrophic")


#######################################
#load the L2 data:
#get google_id for taxon table and load:
#google_id <- L2_data_dir %>% filter(grepl('taxon.csv',name))%>%
#		 select(id) %>% 
#		 unlist()
#taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
taxa <- read.csv("data/L2/taxon.csv")

#get google_id for plot table and load:
#google_id <- L2_data_dir %>% filter(grepl('plot.csv',name))%>%
#		 select(id) %>% 
#		 unlist()
#plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
plots <- read.csv("data/L2/plot.csv")

#get google_id for event table and load:
#google_id <- L2_data_dir %>% filter(grepl('event',name))%>%
#		 select(id) %>% 
#		 unlist()
#events <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
events <- read.csv("data/L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

pc <- read.csv("data/L2/observation_species.csv", stringsAsFactors=F)
phen <- read.csv("data/L2/observation_reproductive_phenology.csv", stringsAsFactors=F)


#random explorations
umbs <- subset(pc, Site == "umbs")
unique(umbs$variable_name)
umbs2 <- subset(phen, Site == "umbs")
unique(umbs2$variable_name)

kbs <- subset(pc, Site == "kbs")
unique(kbs$variable_name)
kbs2 <- subset(phen, Site == "kbs")
unique(kbs2$variable_name)

