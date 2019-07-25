#Moriah's independent work with R to understand and compute the data compiled from the warmxtrophic project
#Moriah Young created 06/14/2019

rm(list = ls())

#Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


###These steps set you up to read gsheets into R directly from the internet (Google Drive):
#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
gs_ls()
drive_find(n_max=10)
#import metadata on files in relevant Google Drive directories
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0_data_entry")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#load the taxon and plot lookup tables to we can cross-reference with them:
#get google_id for taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('taxon.csv',name))%>%
  select(id) %>% 
  unlist()
taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#taxa <- read.csv("data/L2/taxon.csv")

#get google_id for plot table and load:
google_id <- L2_data_dir %>% filter(grepl('plot.csv',name))%>%
  select(id) %>% 
  unlist()
plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)

#get google_id for event table and load:
google_id <- L2_data_dir %>% filter(grepl('event',name))%>%
  select(id) %>% 
  unlist()
events <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
events <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

#read in the gsheet for plant composition
#cleaning of 2019 plant composition data
data <- gs_title("umbs_plantcomp_2019")
dat <- as.data.frame(gs_read(data))

#look at the dataframe:
str(dat)
unique(dat$Site)
unique(dat$Date)
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y") #format the date
#are all sampling events on the event lookup table?
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")
dat$Julian_day <- as.numeric(format(dat$Date,"%j")) #convert the Date to a decimal number 001-36 for day of the year. This helps with plotting, especially when combining different years.
dat$Year <- 2019

#look at plot codes:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot)) 

#compare species codes with taxon lookup table
unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code)) #these are the codes in the gsheet that are not on the taxon table.

#read in the gsheet for phenology events
#cleaning of 2019 phenology data
data_phenology <- gs_title("umbs_flwr_sd_2019")
dat_phen <- as.data.frame(gs_read(data_phenology))

#look at the dataframe:
str(dat_phen)
unique(dat_phen$Site)
unique(dat_phen$Date)
dat_phen$Date <- as.Date(dat_phen$Date, format = "%m/%d/%Y") #format the date
#are all sampling events on the event lookup table?
as.Date(setdiff(unique(dat_phen$Date), unique(events$Date)), origin = "1970-01-01")
dat_phen$Julian_day <- as.numeric(format(dat_phen$Date,"%j")) #convert the Date to a decimal number 001-36 for day of the year. This helps with plotting, especially when combining different years.
dat_phen$Year <- 2019

#look at plot codes:
unique(dat_phen$Plot)
setdiff(unique(dat_phen$Plot), unique(plots$plot)) 

#compare species codes with taxon lookup table
unique(dat_phen$Species)
setdiff(unique(dat_phen$Species), unique(taxa$code)) #these are the codes in the gsheet that are not on the taxon table.

#looking at 2019 phenology data for single species over the season


#Below is trying to work with the L3 data file
library(googledrive)

#this sets the working directory onto the laptop on which you are using
setwd("~/Desktop")

#this downloads the desired dataset from google drive onto your local computer and saves it to the working directory above
drive_download("L3dat.csv", path = 'L3dat.csv', type = "csv", overwrite = TRUE)

#this reads in the data to R and allows you to see the data file
data <- read.csv('L3dat.csv')
View(data)

#subsetting data from L3 
ambient_native <- subset(data, Site == "umbs" & state == "ambient" & origin == "Native")
warmed_native <- subset(data, Site == "umbs" & state == "warmed" & origin == "Native")
ambient_exotic <- subset(data, Site == "umbs" & state == "ambient" & origin == "Exotic")
warmed_exotic <- subset (data, Site == "umbs" & state == "warmed" & origin == "Exotic")

#looking at the mean of first flower for NATIVE species in ambient and warmed plots over all years using the subset data from above
mean(ambient_native$First.flower, na.rm=TRUE)
mean(warmed_native$First.flower, na.rm=TRUE)

#looking at the mean of first flower for EXOTIC species in ambient and warmed plots over all years using the subset data from above
mean(ambient_exotic$First.flower, na.rm=TRUE)
mean(warmed_exotic$First.flower, na.rm=TRUE)



