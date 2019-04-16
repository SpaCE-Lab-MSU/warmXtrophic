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


#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
gs_ls()
drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0_data_entry")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#if you will be using Google Drive File Stream or Back Up and Sync (not the 'googledrive' and 'googlesheets' packages), set your working directory to the warmXtrophic folder on the shared Google Drive.
#setwd("~/Google Drive File Stream/My Drive/warmXtrophic")
#Check to make sure working directory is correct
if(basename(getwd())!="warmXtrophic"){cat("Plz change your working directory. It should be 'warmXtrophic'")}

#######################################
#load the plot and taxon lookup tables:
#get google_id for taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('taxon',name))%>%
		 select(id) %>% 
		 unlist()
taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#taxa <- read.csv("data/L2/taxon.csv")

#get google_id for plot table and load:
google_id <- L2_data_dir %>% filter(grepl('plot',name))%>%
		 select(id) %>% 
		 unlist()
plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#plots <- read.csv("data/L2/plot.csv")

#####################################
#             KBS                   #
#####################################
###2015
data <- gs_title("kbs_plant_comp_2015")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/KBS/2015/kbs_plant_comp_2015.csv", stringsAsFactors=F)
dat$Site <- tolower(dat$Site)
unique(dat$Site)
unique(dat$Date)
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code))
#[1] "Erci"
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
drive_rm("kbs_plant_comp_2015.csv") #delete old version
drive_upload("kbs_plant_comp_2015.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "kbs_plant_comp_2015.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("kbs_plant_comp_2015", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("kbs_plant_comp_2015.csv")

#alternately:
#write.csv(dat, "data/L1/plant_composition/kbs_plant_comp_2015.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))

###2016
data <- gs_title("kbs_plant_comp_2016")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/KBS/2016/kbs_plant_comp_2016.csv", stringsAsFactors=F)
dat$Site <- "kbs"
unique(dat$Date)
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
#compare plot codes with plot lookup table:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
dat$Species[dat$Species=="Elrre"] <- "Elre" #typo
setdiff(unique(dat$Species), unique(taxa$code))
dat$Year <- 2016
hist(dat$Cover)
summary(dat$Cover)
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]
str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
# temp write local
readr::write_csv(dat, "kbs_plant_comp_2016.csv")
drive_rm("kbs_plant_comp_2016.csv") #delete old version
drive_upload("kbs_plant_comp_2016.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "kbs_plant_comp_2016.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("kbs_plant_comp_2016", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("kbs_plant_comp_2016.csv")

#alternately:
#write.csv(dat, "data/L1/plant_composition/kbs_plant_comp_2016.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))


###2017
data <- gs_title("kbs_plant_comp_2017")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/KBS/2017/kbs_plant_comp_2017.csv", stringsAsFactors=F)
unique(dat$Site)
unique(dat$Date)  ## Is 3/2/2017 reasonable?
dat <- dat[dat$Date != "5/10/2016",] #remove random single row from 2016
#convert date to common format:
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Plot)
#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
#clean some taxa codes
dat$Species[dat$Species=="Bown"] <- "Brown" #typo
dat$Species[dat$Species=="Bare Ground"] <- "Bare_Ground"
dat$Species[dat$Species=="Hipr"] <- "Hica" #Hieracium pratense is a synonym for Hieracium caespitosum
#dat$Species[dat$Species=="Ramu"] <- "Romu" #RAMU is not in the USDA plants databse. ROMU is Rosa multiflora. This change was in Phoebe's notes without explanation.
#compare species codes with taxon lookup table
setdiff(unique(dat$Species), unique(taxa$code))
#[1]"Ramu" "Brin"
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
drive_rm("kbs_plant_comp_2017.csv") #delete old version
drive_upload("kbs_plant_comp_2017.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "kbs_plant_comp_2017.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("kbs_plant_comp_2017", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("kbs_plant_comp_2017.csv")

#alternately:
#write.csv(dat, "data/L1/plant_composition/kbs_plant_comp_2017.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))

###2018



###add future years here...

#####################################
#             UMBS                  #
#####################################
#2015
data <- gs_title("umbs_plant_comp_2015")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/UMBS/2015/umbs_plant_comp_2015.csv", stringsAsFactors=F)
dat$Site <- tolower(dat$Site)
unique(dat$Site)
unique(dat$Date)  
#convert date to common format:
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Plot)
#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
dat$Species[dat$Species=="Piau"] <- "Hiau" #USDA accepted name for Pilosella aurantica is Hieracium auranticum
setdiff(unique(dat$Species), unique(taxa$code))
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
readr::write_csv(dat, "umbs_plant_comp_2015.csv")
drive_rm("umbs_plant_comp_2015.csv") #delete old version
drive_upload("umbs_plant_comp_2015.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "umbs_plant_comp_2015.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_plant_comp_2015", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_plant_comp_2015.csv")

#alternately:
#write.csv(dat, "data/L1/plant_composition/umbs_plant_comp_2015.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))

#2016
data <- gs_title("umbs_plant_comp_2016")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/UMBS/2016/umbs_plant_comp_2016.csv", stringsAsFactors=F)
dat$Site <- 'umbs'
unique(dat$Date)  
#convert date to common format:
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Plot)
#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
dat$Species[dat$Species=="Bareground"] <- "Bare_Ground"
dat$Species[dat$Species=="Anspp"] <- "Ansp" #probably a typo
dat$Species[dat$Species=="Piau"] <- "Hiau" #USDA accepted name for Pilosella aurantica is Hieracium auranticum
setdiff(unique(dat$Species), unique(taxa$code))
colnames(dat)[which(names(dat) == "Percent_Cover")] <- "Cover"
hist(dat$Cover)
summary(dat$Cover)
dat$Year <- 2016
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]
str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
# temp write local
readr::write_csv(dat, "umbs_plant_comp_2016.csv")
drive_rm("umbs_plant_comp_2016.csv") #delete old version
drive_upload("umbs_plant_comp_2016.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "umbs_plant_comp_2016.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_plant_comp_2016", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_plant_comp_2016.csv")

#alternately:
#write.csv(dat, "data/L1/plant_composition/umbs_plant_comp_2016.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))


#2017
data <- gs_title("umbs_plant_comp_2017")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/UMBS/2017/umbs_plant_comp_2017.csv", stringsAsFactors=F)
unique(dat$Site)
dat <- dat[dat$Date != "5/10/2016",] #remove random single row from 2016
unique(dat$Date)  
#convert date to common format:
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Plot)
#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
dat$Species[dat$Species=="Bare Ground"] <- "Bare_Ground"
dat$Species[dat$Species=="Bare Groud"] <- "Bare_Ground"
dat$Species[dat$Species=="Bare"] <- "Bare_Ground"
dat$Species[dat$Species=="Unknown 5"] <- "Unknown"
dat$Species[dat$Species=="Vesp"] <- "Vear" #from Phoebe's cleaning script
dat$Species[dat$Species=="Piau"] <- "Hiau" #USDA accepted name for Pilosella aurantica is Hieracium auranticum
setdiff(unique(dat$Species), unique(taxa$code))
hist(dat$Cover)
summary(dat$Cover)
dat$Year <- 2017
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]
str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
# temp write local
readr::write_csv(dat, "umbs_plant_comp_2017.csv")
#drive_rm("umbs_plant_comp_2017.csv") #delete old version
drive_upload("umbs_plant_comp_2017.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "umbs_plant_comp_2017.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_plant_comp_2017", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_plant_comp_2017.csv")

#alternately:
#write.csv(dat, "data/L1/plant_composition/umbs_plant_comp_2017.csv", row.names=F)

#remove data from workspace
rm(list = c('dat', 'data'))

#2018
data <- gs_title("umbs_plant_comp_2018")
dat <- as.data.frame(gs_read(data))
#dat <- read.csv("data/L0/UMBS/2018/umbs_plant_comp_2018.csv", stringsAsFactors=F)
unique(dat$Site)
unique(dat$Date)  
#convert date to common format:
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Plot)
#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
dat$Species[dat$Species=="UNKNOWN1"] <- "Unknown"
dat$Species[dat$Species=="UKNOWN_sp"] <- "Unknown"
dat$Species[dat$Species=="UNKNOWN_GRASS3"] <- "Posp"
dat$Species[dat$Species=="UKNOWN_Grass"] <- "Posp"
dat$Species[dat$Species=="Unknown_shrub"] <- "Unknown"
dat$Species[dat$Species=="UNKNOWN_PLANT1"] <- "Unknown"
dat$Species[dat$Species=="WILD_RASP?"] <- "Unknown"
dat$Species[dat$Species=="UNKNOWN_GRASS2"] <- "Posp"
dat$Species[dat$Species=="UNKNOWN 5"] <- "Unknown"
dat$Species[dat$Species=="UNKNOWN_7"] <- "Unknown"
dat$Species[dat$Species=="UNKNOWN_GRASS"] <- "Posp"
dat$Species[dat$Species=="Unknown_grass"] <- "Posp"
dat$Species[dat$Species=="unknown_grass"] <- "Posp"
dat$Species[dat$Species=="Unknown."] <- "Unknown"
dat$Species[dat$Species=="Piau"] <- "Hiau" #USDA accepted name for Pilosella aurantica is Hieracium auranticum
setdiff(unique(dat$Species), unique(taxa$code))
#[1] "Prse" "Amla" "Ptsp" "Apan"
hist(dat$Cover)
summary(dat$Cover)
dat$Year <- 2018
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]
str(dat)

#Save the L1 data file to googledrive
googledrive::drive_ls("~/warmXtrophic/data/L1/plant_composition")
# temp write local
readr::write_csv(dat, "umbs_plant_comp_2018.csv")
drive_rm("umbs_plant_comp_2018.csv") #delete old version
drive_upload("umbs_plant_comp_2018.csv", 
             path = "~/warmXtrophic/data/L1/plant_composition", 
             name = "umbs_plant_comp_2018.csv", 
             type = NULL,
             verbose = TRUE)
drive_share("umbs_plant_comp_2018", role = "write", type = "anyone") #change permissions to 'anyone with the link can edit'
#remove local file
file.remove("umbs_plant_comp_2018.csv")
#Alternately, write locally.
#dat <- read.csv("data/L0/UMBS/2018/umbs_plant_comp_2018.csv", stringsAsFactors=F)

#remove data from workspace
rm(list = c('dat', 'data'))