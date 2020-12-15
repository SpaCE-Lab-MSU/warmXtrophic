###################################
#1. clean reproductive phenology data
#created by Nina Lany 11-Jun-2019
###################################

#this script reads in the L0 gsheet for each site-year one at a time, cleans the data, and writes it out as a csv in the L1/reproductive_phenology directory on Google Drive.

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
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0_data_entry")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#if you will be using Google Drive File Stream or Back Up and Sync (not the 'googledrive' and 'googlesheets' packages), set your working directory to the warmXtrophic folder on the shared Google Drive.
#setwd("~/Google Drive File Stream/My Drive/warmXtrophic")


#######################################
#load the plot, taxon, and event lookup tables:
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
#plots <- read.csv("data/L2/plot.csv")

#get google_id for event table and load:
google_id <- L2_data_dir %>% filter(grepl('event',name))%>%
		 select(id) %>% 
		 unlist()
events <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#events <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

#####################################
#             KBS                   #
#####################################
###2015
data <- gs_title("kbs_flwr_sd_2015")
dat <- as.data.frame(gs_read(data))

colnames(dat) <- c("Date", "Julian", "Collector", "Plot", "Site", "Species", "Event")
dat$Site <- tolower(dat$Site)
unique(dat$Site)
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")

#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code))

unique(dat$Event)
dat$Year <- 2015
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
drive_upload("kbs_reproductive_phen_2015.csv", 
             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
             name = "kbs_reproductive_phen_2015.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/kbs_reproductive_phen_2015.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))


###2016
data <- gs_title("kbs_flwr_sd_2016")
dat <- as.data.frame(gs_read(data))

colnames(dat) <- c("Date", "Julian", "Plot", "Species", "Event")
dat$Site <- 'kbs'

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")

#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
dat$Species[dat$Species=="rual"] <- "Rual" 
dat$Species[dat$Species=="HIsp"] <- "Hisp" 
dat$Species[dat$Species=="PHpr"] <- "Phpr" 
dat$Species[dat$Species=="Popre"] <- "Popr"
#note: Sila was flowering on D3 but is not present at all in the percent cover dataset. Remove it
dat <- dat[-(which(dat$Species == 'Sila')),]
setdiff(unique(dat$Species), unique(taxa$code))

unique(dat$Event)

dat$Year <- 2016
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
readr::write_csv(dat, "kbs_reproductive_phen_2016.csv")
drive_rm("kbs_reproductive_phen_2016.csv") #delete old version
drive_upload("kbs_reproductive_phen_2016.csv", 
             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
             name = "kbs_reproductive_phen_2016.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("kbs_reproductive_phen_2016.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("kbs_reproductive_phen_2016.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/kbs_reproductive_phen_2016.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))


###2017
data <- gs_title("kbs_flwr_sd_2017")
dat <- as.data.frame(gs_read(data))

colnames(dat)[which(names(dat) == "Action")] <- "Event"
unique(dat$Site)
dat <- dat[dat$Date != "4/30/2016",] #remove random single row from 2016
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")

#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
dat$Species[dat$Species=="Hipr"] <- "Hica" #name change 
dat$Species[dat$Species=="unknown"] <- "Unknown" 
dat$Species[dat$Species=="Dafl"] <- "Dagl"  #probably a typo
dat$Species[dat$Species=="Drin"] <- "Brin"  #probably a typo
dat$Species[dat$Species=="Dlre"] <- "Elre"  #probably a typo
dat$Species[dat$Species=="Dach"] <- "Daca"  #probably a typo
dat$Species[dat$Species=="Aca"] <- "Daca"  #probably a typo 
dat$Species[dat$Species=="Aspi"] <- "Sypi"  #name chgange
dat$Species[dat$Species=="Sogr"] <- "Eugr"  #name chgange
dat$Species[dat$Species=="Agre"] <- "Elre"  #name change
setdiff(unique(dat$Species), unique(taxa$code))

dat$Event[dat$Event=="speed"] <- "seed"
dat$Event[dat$Event=="floer"] <- "flower"
dat$Event[dat$Event=="flwoer"] <- "flower" 
unique(dat$Event)

dat$Year <- 2017
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
readr::write_csv(dat, "kbs_reproductive_phen_2017.csv")
drive_rm("kbs_reproductive_phen_2017.csv") #delete old version
drive_upload("kbs_reproductive_phen_2017.csv", 
             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
             name = "kbs_reproductive_phen_2017.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("kbs_reproductive_phen_2017.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("kbs_reproductive_phen_2017.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/kbs_reproductive_phen_2017.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))





#####################################
#             UMBS                   #
#####################################
###2016 (no data from 2015)
data <- gs_title("umbs_flwr_sd_2016")
dat <- as.data.frame(gs_read(data))


colnames(dat)[which(names(dat) == "Action")] <- "Event"
dat$Site <- 'umbs'

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")

#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
dat$Species[dat$Species=="ruac"] <- "Ruac"
dat$Species[dat$Species=="Ruace"] <- "Ruac"
dat$Species[dat$Species=="Piau"] <- "Hiau" #name change  
setdiff(unique(dat$Species), unique(taxa$code))

unique(dat$Event)
dat$Year <- 2016
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
readr::write_csv(dat, "umbs_reproductive_phen_2016.csv")
drive_rm("umbs_reproductive_phen_2016.csv") #delete old version
drive_upload("umbs_reproductive_phen_2016.csv", 
             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
             name = "umbs_reproductive_phen_2016.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_reproductive_phen_2016.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_reproductive_phen_2016.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/umbs_reproductive_phen_2016.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))



###2017
data <- gs_title("umbs_flwr_sd_2017")
dat <- as.data.frame(gs_read(data))


colnames(dat)[which(names(dat) == "Action")] <- "Event"
unique(dat$Site)
dat <- dat[dat$Date != "4/30/2016",] #remove random single row from 2016
dat$Date[dat$Date=="5/13/0201"] <- "5/13/2017"
dat$Date[dat$Date=="6/1/1017"] <- "6/1/2017"
dat$Date[dat$Date=="6/23/3017"] <- "6/23/2017"
dat$Date[dat$Date=="6/30/3017"] <- "6/30/2017"
dat$Date[dat$Date=="7/11/1017"] <- "7/11/2017"
dat$Date[dat$Date=="7/14/20117"] <- "7/14/2017"
dat$Date[dat$Date=="7/23/3017"] <- "7/23/2017"
dat$Date[dat$Date=="8/9/0207"] <- "8/9/2017"
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)

as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")

#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
dat$Species[dat$Species=="Piau"] <- "Hiau" #name change  
dat$Species[dat$Species=="cape"] <- "Cape"
dat$Species[dat$Species=="Porp"] <- "Popr"
setdiff(unique(dat$Species), unique(taxa$code))

unique(dat$Event)
dat$Year <- 2017
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
readr::write_csv(dat, "umbs_reproductive_phen_2017.csv")
drive_rm("umbs_reproductive_phen_2017.csv") #delete old version
drive_upload("umbs_reproductive_phen_2017.csv", 
             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
             name = "umbs_reproductive_phen_2017.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_reproductive_phen_2017.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_reproductive_phen_2017.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/umbs_reproductive_phen_2017.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))

###2018
data <- gs_title("umbs_flwr_sd_2018")
dat <- as.data.frame(gs_read(data))


colnames(dat)[which(names(dat) == "Action")] <- "Event"
unique(dat$Site)
dat <- dat[dat$Date != "4/30/2016",] #remove random single row from 2016
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)

as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")

#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
dat$Species[dat$Species=="Piau"] <- "Hiau" #name change  
dat$Species[dat$Species=="cape"] <- "Cape"
dat$Species[dat$Species=="Porp"] <- "Popr"
setdiff(unique(dat$Species), unique(taxa$code))

dat$Event[dat$Event=="fruit"] <- "seed"
unique(dat$Event)
dat$Year <- 2018
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
readr::write_csv(dat, "umbs_reproductive_phen_2018.csv")
drive_rm("umbs_reproductive_phen_2018.csv") #delete old version
drive_upload("umbs_reproductive_phen_2018.csv", 
             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
             name = "umbs_reproductive_phen_2018.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_reproductive_phen_2018.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_reproductive_phen_2018.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/umbs_reproductive_phen_2018.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))
