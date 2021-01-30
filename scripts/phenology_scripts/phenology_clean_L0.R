# TITLE:          Phenology Data Cleanup
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder - all phenology data
#                 for each site and every year 
# DATA OUTPUT:    A csv file containing phenology data for all sites & years is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
# Pat said he doesn't setwd --> figure out .renviron
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Source in needed functions from phenology_functions.R
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/scripts/phenology_scripts/phenology_functions.R")

### Read in meta data files
# Read in species list
taxa <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L0/taxon.csv", stringsAsFactors=F)
# Change column name for from "code" to "Species" to match cleaned phenology data
colnames(taxa) <- sub("code", "species", colnames(taxa))

# Read in plot list
plot_info <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L0/plot.csv")

# Read in phenology data
kbs_2015 <- read.csv("L0/KBS/2015/kbs_flwr_sd_2015.csv")
kbs_2016 <- read.csv("L0/KBS/2016/kbs_flwr_sd_2016.csv")
kbs_2017 <- read.csv("L0/KBS/2017/kbs_flwr_sd_2017.csv")
kbs_2018 <- read.csv("L0/KBS/2018/kbs_flwr_sd_2018.csv")
kbs_2019 <- read.csv("L0/KBS/2019/kbs_flwr_sd_2019.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_flwr_sd_2020.csv")
umbs_2015 <- read.csv("L0/UMBS/2015/umbs_flwr_sd_2015.csv")
umbs_2016 <- read.csv("L0/UMBS/2016/umbs_flwr_sd_2016.csv")
umbs_2017 <- read.csv("L0/UMBS/2017/umbs_flwr_sd_2017.csv")
umbs_2018 <- read.csv("L0/UMBS/2018/umbs_flwr_sd_2018.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_flwr_sd_2019.csv")
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_flwr_sd_2020.csv")

# Add "Site" column for kbs and umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column name for kbs 2015
colnames(kbs_2015) <- sub("event", "Action", colnames(kbs_2015))
colnames(umbs_2015) <- sub("event", "Action", colnames(umbs_2015))

# Add dataframes into a list so that needed functions can be applied 
phen_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
phen_list <- lapply(phen_list, change_colnames) 
phen_list <- lapply(phen_list, remove_col, name=c("Julian", "Notes", "collector", "julian", "notes"))
#phen_list <- lapply(phen_list, colnames_ordered)
phen_list <- lapply(phen_list, change_date)
lapply(phen_list, spp_name) # look over species code to see what needs to be fixed
lapply(phen_list, site_name) # need to make these all the same for each site
lapply(phen_list, date_check) # see if any dates were entered incorrectly, like the year

# Fixing species names
phen_list <- lapply(phen_list, change_spp)
lapply(phen_list, spp_name) # looks good

# Would be great to write some code where you can compare the species codes in the data with
# our species list
#spp_name <- function(df){
#        spp <- unique(sort(df[["Species"]]))
#        return(spp)
#}
#unique(dat$Species)
#setdiff(unique(dat$Species), unique(taxa$code))

# Fixing site names
phen_list <- lapply(phen_list, change_site)
lapply(phen_list, site_name) # looks good

# Fixing site dates
phen_list <- lapply(phen_list, change_date)
lapply(phen_list, date_check) # looks good

# Merge final data (make a single data frame from all of the lists)
phen_merge <- rbind(phen_list$kbs_2015, phen_list$kbs_2016, phen_list$kbs_2017, phen_list$kbs_2018, phen_list$kbs_2019, phen_list$kbs_2020,
                    phen_list$umbs_2016, phen_list$umbs_2017, phen_list$umbs_2018, phen_list$umbs_2019, phen_list$umbs_2020)
str(phen_merge)

# Change column names to lowercase
names(phen_merge) <- tolower(names(phen_merge))

# Fix date column & add column for the year and julian day
phen_merge$date <- as.Date(phen_merge$date, format="%m/%d/%Y")
str(phen_merge)
phen_merge$year <- format(phen_merge$date,format="%Y")
phen_merge$julian <- format(phen_merge$date, "%j")

# merge cleaned data with the plot and species level information
phen_merge2 <- merge(phen_merge, plot_info, by = "plot")
phen_merge3 <- merge(phen_merge2, taxa, by = "species")

colnames(phen_data) <- sub("site.x", "site", colnames(phen_data))

# Change column names to lowercase (again)
names(phen_data) <- tolower(names(phen_data))
str(phen_data)

# Make julian column numeric
phen_data$julian <- as.numeric(phen_data$julian)
str(phen_data)

phen_data <- phen_data[, c("site", "plot", "species", "action", "date", "julian", "year", "treatment_key", "state",
                          "insecticide", "scientific_name", "common_name", "usda_species", "lter_species",
                          "origin", "group", "family", "duration", "growth_habit")]

str(phen_data)

# write a new cvs with the cleaned and merge data and upload to the shared google drive
write.csv(phen_data, file="L1/phenology/final_flw_sd_L1.csv")
