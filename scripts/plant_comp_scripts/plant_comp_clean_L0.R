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

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data (only need columns 1-7 for the umbs files)
meta <- read.csv("L0/plot.csv")
taxon <- read.csv("L0/taxon.csv")
kbs_2015 <- read.csv("L0/KBS/2015/kbs_plant_comp_2015.csv")
kbs_2016 <- read.csv("L0/KBS/2016/kbs_plant_comp_2016.csv")
kbs_2017 <- read.csv("L0/KBS/2017/kbs_plant_comp_2017.csv")
kbs_2018 <- read.csv("L0/KBS/2018/kbs_plant_comp_2018.csv")
kbs_2019 <- read.csv("L0/KBS/2019/kbs_plant_comp_2019.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_plant_comp_2020.csv")
umbs_2015 <- read.csv("L0/UMBS/2015/umbs_plant_comp_2015.csv")
umbs_2016 <- read.csv("L0/UMBS/2016/umbs_plant_comp_2016.csv")
umbs_2017 <- read.csv("L0/UMBS/2017/umbs_plant_comp_2017.csv")
umbs_2018 <- read.csv("L0/UMBS/2018/umbs_plant_comp_2018.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_plantcomp_2019.csv")[,1:7]
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_plantcomp_2020.csv")[,1:7]

# remove all empty rows for umbs_2019
umbs_2019[c(5046:6024),]
umbs_2019 <- umbs_2019[-c(5047:6024), ]

# add site column for kbs & umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column name for umbs 2016
colnames(umbs_2016) <- sub("Percent_Cover", "Cover", colnames(umbs_2016))

# Add dataframes into a list so that needed functions can be applied 
comp_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
comp_list <- lapply(comp_list, remove_col, name=c("Julian", "Notes", "Quadrat", "Julian_Day"))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names
lapply(comp_list, site_name) # need to make these all the same for each site

# Fixing species names
# Ruag (Rual), Smooth_oat (Arre? Arel?), Cofo (?)
comp_list <- lapply(comp_list, change_spp)
lapply(comp_list, spp_name) # looks good

# Fixing site name
comp_list <- lapply(comp_list, change_site)
lapply(comp_list, site_name) # looks good

# Merge final data
comp_merge <- rbind(comp_list$kbs_2015, comp_list$kbs_2016, comp_list$kbs_2017, comp_list$kbs_2018, comp_list$kbs_2019, comp_list$kbs_2020, 
                    comp_list$umbs_2015, comp_list$umbs_2016, comp_list$umbs_2017, comp_list$umbs_2018, comp_list$umbs_2019, comp_list$umbs_2020)
str(comp_merge)

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
sort(unique(comp_merge$plot))
plant_comp_merge <- left_join(meta, comp_merge, by = "plot")

# taxon contains "site" which is the site where the species is found on our meta-data table, but those data exist in our plant_comp_merge dataset already.
# Delete "site" from taxon so it doesn't accidentally get merged in.
taxon$site<-NULL
plant_comp_merge2 <- left_join(taxon, plant_comp_merge, by = "species")

# remove unnecessary columns
plant_comp_merge2$old_code <- NULL
plant_comp_merge2$old_name <- NULL
plant_comp_merge2$resolution <- NULL

# Upload clean data csv to google drive without the index column
write.csv(plant_comp_merge2, file="L1/plant_composition/final_plantcomp_L1.csv", row.names=FALSE)

