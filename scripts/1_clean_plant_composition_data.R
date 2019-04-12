###################################
#1. clean plant composition data
#created by Nina Lany 11-Apr-2019
###################################

#this script reads in the L0 gsheet for each site-year one at a time, cleans the data, and writes it out as a csv in the L1/plant_composition directory on Google Drive.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#authenticate with Google Drive. If this is your first time using the 'googledrive' or 'googlesheets' packages to access Google Drive from R, a browser window will prompt you to log in to Google Drive.
gs_ls()
drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#######################################
#load the plot and taxon lookup tables:
#get google_id for taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('taxon',name))%>%
		 select(id) %>% 
		 unlist()
taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)

#get google_id for plot table and load:
google_id <- L2_data_dir %>% filter(grepl('plot',name))%>%
		 select(id) %>% 
		 unlist()
plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)


#####################################
#             KBS                   #
#####################################
###2015
data <- gs_title("kbs_plant_comp_2015")
dat <- as.data.frame(gs_read(data))
dat$Site <- tolower(dat$Site)
unique(dat$Site)
unique(dat$Date)
#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code))
#[1] "Phpr" "Erci"
#in 2015 only, percent cover was measured separately on four quadrants of each quadrat(plot). Aggregate to get percent cover of each quadrat (plot):
dat$Cover <- dat$Cover * .25
dat <- aggregate(Cover~Site+Date+Plot+Species, data = dat, FUN = sum)
hist(dat$Cover)
summary(dat$Cover)
dat$Year <- 2015
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]
str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
# temp write local
readr::write_csv(dat, "kbs_plant_comp_2015.csv")
drive_upload("kbs_plant_comp_2015.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "kbs_plant_comp_2015.csv", 
             type = NULL,
             verbose = TRUE)
#remove local file
file.remove("kbs_plant_comp_2015.csv")
#remove data from workspace
rm(list = c('dat', 'data'))

###2016


###2017
data <- gs_title("kbs_plant_comp_2017")
dat <- as.data.frame(gs_read(data))
unique(dat$Site)
unique(dat$Date)
dat <- dat[dat$Date != "5/10/2016",] #remove random single row from 2016
unique(dat$Plot)
#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
#clean some taxa codes
dat$Species[dat$Species=="Bown"] <- "Brown" #typo
dat$Species[dat$Species=="Bare Ground"] <- "Bare_Ground"
dat$Species[dat$Species=="Hipr"] <- "Hica" #Hieracium pratense is a synonym for Hieracium caespitosum
#dat$Species[dat$Species=="Ramu"] <- "Romu" #RAMU is not in the USDA plants databse. ROMU is Rosa multiflora. 
#compare species codes with taxon lookup table
setdiff(unique(dat$Species), unique(taxa$code))
#[1] "Phpr" "Cahi" "Ramu" "Brin"
hist(dat$Cover)
summary(dat$Cover)
dat$Year <- 2017
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]
str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
# temp write local
readr::write_csv(dat, "kbs_plant_comp_2017.csv")
drive_upload("kbs_plant_comp_2017.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "kbs_plant_comp_2017.csv", 
             type = NULL,
             verbose = TRUE)
#remove local file
file.remove("kbs_plant_comp_2017.csv")
#remove data from workspace
rm(list = c('dat', 'data'))

###2018



###add future years here...

#####################################
#             UMBS                  #
#####################################