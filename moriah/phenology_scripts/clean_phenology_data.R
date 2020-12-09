###################################
#1. clean reproductive phenology data
#created by Nina Lany 11-Jun-2019
#updated by Moriah Young 2020-12
###################################

#this script reads in the L0 csv for each site-year one at a time, cleans the data, and writes it out as a csv in the L1/reproductive_phenology directory on Google Drive.

rm(list = ls())

# Check for and install required packages
library(tidyverse)

# Set working directory to file stream
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

#source script with useful functions


#######################################
#load the plot, taxon, and event lookup tables:

# read in the taxon list 
taxa <- read.csv("L2/taxon_uptodate.csv", stringsAsFactors=F)

# read in the plot look up table
plots <- read.csv("L2/plot.csv", stringsAsFactors=F)

# read in events table
events <- read.csv("L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

#####################################
#             KBS                   #
#####################################
###2015
data <- read.csv("L0/KBS/2015/kbs_flwr_sd_2015.csv")
dat <- as.data.frame(data)

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
#googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
#readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
#drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
#drive_upload("kbs_reproductive_phen_2015.csv", 
#             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
#             name = "kbs_reproductive_phen_2015.csv", 
#             type = NULL,
#             verbose = TRUE)
#drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
#file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
write.csv(dat, "L1/reproductive_phenology/kbs_reproductive_phen_2015.csv", row.names=F)
file.remove("L1/reproductive_phenology/kbs_reproductive_phen_2015.csv")

#remove data from workspace
rm(list = c('dat', 'data'))


###2016
data <- read.csv("L0/KBS/2016/kbs_flwr_sd_2016.csv")
dat <- as.data.frame(data)

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
data <- read.csv("L0/KBS/2017/kbs_flwr_sd_2017.csv")
dat <- as.data.frame(data)

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
dat$Species[dat$Species=="Aspi"] <- "Sypi"  #name change
dat$Species[dat$Species=="Sogr"] <- "Eugr"  #name change
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
write_csv(dat, "kbs_reproductive_phen_2017.csv")

#remove local file
file.remove("kbs_reproductive_phen_2017.csv")

#alternately:
#write.csv(dat, "data/L1/reproductive_phenology/kbs_reproductive_phen_2017.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))

###2018
data <- read.csv("L0/KBS/2018/kbs_flwr_sd_2018.csv")
dat <- as.data.frame(data)

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
dat$Year <- 2018
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
#googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
#readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
#drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
#drive_upload("kbs_reproductive_phen_2015.csv", 
#             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
#             name = "kbs_reproductive_phen_2015.csv", 
#             type = NULL,
#             verbose = TRUE)
#drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
#file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
write.csv(dat, "L1/reproductive_phenology/kbs_reproductive_phen_2018.csv", row.names=F)
file.remove("L1/reproductive_phenology/kbs_reproductive_phen_2018.csv")

#remove data from workspace
rm(list = c('dat', 'data'))

###2019
data <- read.csv("L0/KBS/2019/kbs_flwr_sd_2019.csv")
dat <- as.data.frame(data)

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
dat$Year <- 2019
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
#googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
#readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
#drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
#drive_upload("kbs_reproductive_phen_2015.csv", 
#             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
#             name = "kbs_reproductive_phen_2015.csv", 
#             type = NULL,
#             verbose = TRUE)
#drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
#file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
write.csv(dat, "L1/reproductive_phenology/kbs_reproductive_phen_2015.csv", row.names=F)
file.remove("L1/reproductive_phenology/kbs_reproductive_phen_2015.csv")

#remove data from workspace
rm(list = c('dat', 'data'))

###2020
data <- read.csv("L0/KBS/2020/kbs_flwr_sd_2020.csv")
dat <- as.data.frame(data)

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
dat$Year <- 2020
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
#googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
#readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
#drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
#drive_upload("kbs_reproductive_phen_2015.csv", 
#             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
#             name = "kbs_reproductive_phen_2015.csv", 
#             type = NULL,
#             verbose = TRUE)
#drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
#file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
write.csv(dat, "L1/reproductive_phenology/kbs_reproductive_phen_2020.csv", row.names=F)
file.remove("L1/reproductive_phenology/kbs_reproductive_phen_2020.csv")

#remove data from workspace
rm(list = c('dat', 'data'))


#####################################
#             UMBS                   #
#####################################
###2016 (no data from 2015)
data <- read.csv("L0/UMBS/2016/umbs_flwr_sd_2016.csv")
dat <- as.data.frame(data)

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
data <- read.csv("L0/UMBS/2017/umbs_flwr_sd_2017.csv")
dat <- as.data.frame(data)

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
data <- read.csv("L0/UMBS/2018/umbs_flwr_sd_2018.csv")
dat <- as.data.frame(data)

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

###2019
data <- read.csv("L0/UMBS/2019/umbs_flwr_sd_2019.csv")
dat <- as.data.frame(data)

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
dat$Year <- 2019
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
#googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
#readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
#drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
#drive_upload("kbs_reproductive_phen_2015.csv", 
#             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
#             name = "kbs_reproductive_phen_2015.csv", 
#             type = NULL,
#             verbose = TRUE)
#drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
#file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
write.csv(dat, "L1/reproductive_phenology/umbs_reproductive_phen_2019.csv", row.names=F)
file.remove("L1/reproductive_phenology/umbs_reproductive_phen_2019.csv")

#remove data from workspace
rm(list = c('dat', 'data'))

###2020
data <- read.csv("L0/UMBS/2020/umbs_flwr_sd_2020.csv")
dat <- as.data.frame(data)

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
dat$Year <- 2020
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Event")]

str(dat)

#Save the L1 data file to googledrive
#googledrive::drive_ls("~/warmXtrophic/data/L1/reproductive_phenology")
# temp write local
#readr::write_csv(dat, "kbs_reproductive_phen_2015.csv")
#drive_rm("kbs_reproductive_phen_2015.csv") #delete old version
#drive_upload("kbs_reproductive_phen_2015.csv", 
#             path = "~/warmXtrophic/data/L1/reproductive_phenology", 
#             name = "kbs_reproductive_phen_2015.csv", 
#             type = NULL,
#             verbose = TRUE)
#drive_share("kbs_reproductive_phen_2015.csv", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
#file.remove("kbs_reproductive_phen_2015.csv")

#alternately:
write.csv(dat, "L1/reproductive_phenology/umbs_reproductive_phen_2020.csv", row.names=F)
file.remove("L1/reproductive_phenology/umbs_reproductive_phen_2020.csv")

#remove data from workspace
rm(list = c('dat', 'data'))