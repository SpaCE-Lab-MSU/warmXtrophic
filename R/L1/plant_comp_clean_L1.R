# TITLE:          warmXtrophic Plant composition cleanup
# AUTHORS:        Kara Dobson, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing plant comp data for all sites & years is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Source in needed functions from the github repo
source("~/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R")
#source("~/DATA/git/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R") # PLZ's location

# Set working directory
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L0_dir)

# Read in data (only need columns 1-7 for the umbs files)
meta <- read.csv(file.path(L0_dir, "plot.csv"))
taxon <- read.csv(file.path(L0_dir,"taxon.csv"))
kbs_2015 <- read.csv(file.path(L0_dir,"KBS/2015/kbs_plant_comp_2015.csv"))
kbs_2016 <- read.csv(file.path(L0_dir,"KBS/2016/kbs_plant_comp_2016.csv"))
kbs_2017 <- read.csv(file.path(L0_dir,"KBS/2017/kbs_plant_comp_2017.csv"))
kbs_2018 <- read.csv(file.path(L0_dir,"KBS/2018/kbs_plant_comp_2018.csv"))
kbs_2019 <- read.csv(file.path(L0_dir,"KBS/2019/kbs_plant_comp_2019.csv"))
kbs_2020 <- read.csv(file.path(L0_dir,"KBS/2020/kbs_plant_comp_2020.csv"))
umbs_2015 <- read.csv(file.path(L0_dir,"UMBS/2015/umbs_plant_comp_2015.csv"))
umbs_2016 <- read.csv(file.path(L0_dir,"UMBS/2016/umbs_plant_comp_2016.csv"))
umbs_2017 <- read.csv(file.path(L0_dir,"UMBS/2017/umbs_plant_comp_2017.csv"))
umbs_2018 <- read.csv(file.path(L0_dir,"UMBS/2018/umbs_plant_comp_2018.csv"))
umbs_2019 <- read.csv(file.path(L0_dir,"UMBS/2019/umbs_plantcomp_2019.csv"))[,1:7]
umbs_2020 <- read.csv(file.path(L0_dir,"UMBS/2020/umbs_plantcomp_2020.csv"))[,1:7]

# remove all empty rows for umbs_2019
umbs_2019[c(5046:6024),]
umbs_2019 <- umbs_2019[-c(5047:6024), ]

# add site column for kbs & umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column name for umbs 2016
colnames(umbs_2016) <- sub("Percent_Cover", "Cover", colnames(umbs_2016))

# Could make a function for these below, TBD
# remove a species from KBS (and not UMBS)
kbs_2015 <- kbs_2015[!grepl("Desp",kbs_2015$Species),]
kbs_2016 <- kbs_2016[!grepl("Desp",kbs_2016$Species),]
kbs_2017 <- kbs_2017[!grepl("Desp",kbs_2017$Species),]
kbs_2018 <- kbs_2018[!grepl("Desp",kbs_2018$Species),]
kbs_2019 <- kbs_2019[!grepl("Desp",kbs_2019$Species),]
kbs_2020 <- kbs_2020[!grepl("Desp",kbs_2020$Species),]
kbs_2020 <- kbs_2020[!grepl("Des",kbs_2020$Species),]

# remove species from UMBS (and not KBS)
umbs_2015 <- umbs_2015[!grepl("Vear",umbs_2015$Species),]
umbs_2016 <- umbs_2016[!grepl("Vear",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Vear",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Vear",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Vear",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Vear",umbs_2020$Species),]

umbs_2015 <- umbs_2015[!grepl("Elre",umbs_2015$Species),]
umbs_2016 <- umbs_2016[!grepl("Elre",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Elre",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Elre",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Elre",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Elre",umbs_2020$Species),]

umbs_2015 <- umbs_2015[!grepl("Rusp",umbs_2015$Species),]
umbs_2016 <- umbs_2016[!grepl("Rusp",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Rusp",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Rusp",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Rusp",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Rusp",umbs_2020$Species),]

umbs_2015 <- umbs_2015[!grepl("Ruag",umbs_2015$Species),]
umbs_2016 <- umbs_2016[!grepl("Ruag",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Ruag",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Ruag",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Ruag",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Ruag",umbs_2020$Species),]

umbs_2015 <- umbs_2015[!grepl("Rual",umbs_2015$Species),]
umbs_2016 <- umbs_2016[!grepl("Rual",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Rual",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Rual",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Rual",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Rual",umbs_2020$Species),]

# Add dataframes into a list so that needed functions can be applied 
comp_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
comp_list <- lapply(comp_list, remove_col, name=c("Julian", "Notes", "Quadrat", "Julian_Day"))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names
lapply(comp_list, site_name) # need to make these all the same for each site

# Fixing species names
comp_list <- lapply(comp_list, change_spp)
lapply(comp_list, spp_name)

# Remove species
comp_list <- lapply(comp_list, remove_spp)
lapply(comp_list, spp_name)

# Fixing site name
comp_list <- lapply(comp_list, change_site)
lapply(comp_list, site_name)

# Merge final data
comp_merge <- rbind(comp_list$kbs_2015, comp_list$kbs_2016, comp_list$kbs_2017, comp_list$kbs_2018, comp_list$kbs_2019, comp_list$kbs_2020, 
                    comp_list$umbs_2015, comp_list$umbs_2016, comp_list$umbs_2017, comp_list$umbs_2018, comp_list$umbs_2019, comp_list$umbs_2020)
str(comp_merge)
sort(unique(comp_merge$Species))

# Change column names to lowercase so they can be merged with metadata
names(comp_merge) <- tolower(names(comp_merge))

# Make year, month, and julian date columns
comp_merge$year <- format(comp_merge$date,format="%Y")
comp_merge$month <- format(comp_merge$date,format="%m")
comp_merge$julian <- format(comp_merge$date, "%j")
head(comp_merge)

# Make julian column numeric
unique(comp_merge$julian)
comp_merge$julian <- as.numeric(comp_merge$julian)
unique(comp_merge$julian)

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"

# Merge meta-data with plant comp data 
# Make sure the plot IDs are the same in each, and there aren't any missing
sort(unique(comp_merge$plot))
plant_comp_merge <- left_join(meta, comp_merge, by = "plot")
sort(unique(plant_comp_merge$species))

# taxon contains "site" which is the site where the species is found on our meta-data table, but those data exist in our plant_comp_merge dataset already.
# Delete "site" from taxon so it doesn't accidentally get merged in.
taxon$site<-NULL
plant_comp_merge2 <- left_join(taxon, plant_comp_merge, by = "species")
sort(unique(plant_comp_merge2$species))

# remove species not found in our dataset
plant_comp_merge3 <- plant_comp_merge2[!is.na(plant_comp_merge2$cover),]
sort(unique(plant_comp_merge3$species))

# remove unnecessary columns
plant_comp_merge3$old_code <- NULL
plant_comp_merge3$old_name <- NULL
plant_comp_merge3$resolution <- NULL

# Upload clean data csv to google drive without the index column
write.csv(plant_comp_merge3, file="L1/plant_composition/final_plantcomp_L1.csv", row.names=FALSE)

