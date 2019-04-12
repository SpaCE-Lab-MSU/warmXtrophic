###################################
#1. clean plant composition data
#created by Nina Lany 11-Apr-2019
###################################

#this script reads in the L0 gsheet for each site-year one at a time, cleans the data, and writes it out as a csv in the L1/plant_composition directory.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#authenticate with googledrive. If this is your first time using 'googledrive' or 'googlesheets' to access GoogleDrive from R, a browser window will promt you to log in to GoogleDrive.
gs_ls()
drive_find(n_max=10)

#get contents of relevant google drive directories for R
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#######################################
#load the plot and taxon lookup tables:
#get google_id for Taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('taxon',name))%>%
		 select(id) %>% 
		 unlist()
taxon <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)

#get google_id for Taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('plot',name))%>%
		 select(id) %>% 
		 unlist()
plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)


##############
#     KBS    #
##############

###2015


###2016


###2017
data <- gs_title("kbs_plant_comp_2017")
dat <- as.data.frame(gs_read(data))
str(dat)
unique(dat$Site)
unique(dat$Date)
dat <- dat[dat$Date != "5/10/2016",] #remove random single row from 2016
unique(dat$Plot)
#compare entries with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
unique(dat$Species)
#clean some taxa codes
dat$Species[dat$Species=="Bown"] <- "Brown" #typo
dat$Species[dat$Species=="Bare Ground"] <- "Bare_Ground"
dat$Species[dat$Species=="Hipr"] <- "Hica" #Hieracium pratense is a synonym for Hieracium caespitosum
#dat$Species[dat$Species=="Ramu"] <- "Romu" #RAMU is not in the USDA plants databse. ROMU is Rosa multiflora. 
#compare species codes with taxon lookup table
setdiff(unique(dat$Species), unique(taxon$code))
#[1] "Phpr" "Cahi" "Ramu" "Brin"
hist(dat$Cover)
summary(dat$Cover)
dat$Year <- 2017
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Plot", "Species", "Cover")]

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
rm(dat)