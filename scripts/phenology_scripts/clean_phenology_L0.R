# TITLE:          Phenology Data Cleanup
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing phenology data for all sites & years is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Source in needed functions from Kara's plant_comp_functions.R
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/kara/plant_response_scripts/plant_comp_functions.R")

# Read in species list
taxa <- read.csv("L2/taxon_uptodate.csv", stringsAsFactors=F)

# Read in data
kbs_2015 <- read.csv("L0/KBS/2015/kbs_flwr_sd_2015.csv")
kbs_2016 <- read.csv("L0/KBS/2016/kbs_flwr_sd_2016.csv")
kbs_2017 <- read.csv("L0/KBS/2017/kbs_flwr_sd_2017.csv")
kbs_2018 <- read.csv("L0/KBS/2018/kbs_flwr_sd_2018.csv")
kbs_2019 <- read.csv("L0/KBS/2019/kbs_flwr_sd_2019.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_flwr_sd_2020.csv")
umbs_2016 <- read.csv("L0/UMBS/2016/umbs_flwr_sd_2016.csv")
umbs_2017 <- read.csv("L0/UMBS/2017/umbs_flwr_sd_2017.csv")
umbs_2018 <- read.csv("L0/UMBS/2018/umbs_flwr_sd_2018.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_flwr_sd_2019.csv")
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_flwr_sd_2020.csv")

# Add "Site" column for kbs and umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Add a "Year" column for kbs and umbs for all years
kbs_2015$Year <- 2015
kbs_2016$Year <- 2016
kbs_2017$Year <- 2017
kbs_2018$Year <- 2018
kbs_2019$Year <- 2019
kbs_2020$Year <- 2020
umbs_2016$Year <- 2016
umbs_2017$Year <- 2017
umbs_2018$Year <- 2018
umbs_2019$Year <- 2019
umbs_2020$Year <- 2020


# Change column name for kbs 2015
colnames(kbs_2015) <- sub("event", "Action", colnames(kbs_2015))

#function to change column names
change_colnames <- function(df){
        names(df)[names(df) == "site"] <- "Site"
        names(df)[names(df) == "date"] <- "Date"
        names(df)[names(df) == "plot"] <- "Plot"
        names(df)[names(df) == "species"] <- "Species"
        names(df)[names(df) == "action"] <- "Action"
        return(df)
}

# change species names
change_spp_phen <- function(df){
        df$Species[df$Species == "Aspi "] <- "Aspi"
        df$Species[df$Species == "Daca "] <- "Daca"
        df$Species[df$Species == "Hype "] <- "Hype"
        df$Species[df$Species == "Soca "] <- "Soca"
        df$Species[df$Species == "Acmi "] <- "Acmi"
        df$Species[df$Species == "ruac"] <- "Ruac"
        df$Species[df$Species == "Ruace"] <- "Ruac"
        df$Species[df$Species == "Cape "] <- "Cape"
        df$Species[df$Species == "cape"] <- "Cape"
        df$Species[df$Species == "Vaan "] <- "Vaan"
        df$Species[df$Species == "Popr "] <- "Popr"
        df$Species[df$Species == "Popre"] <- "Popr"
        df$Species[df$Species == "Porp"] <- "Popr"
        df$Species[df$Species == "PHpr"] <- "Phpr"
        df$Species[df$Species == "Piau"] <- "Hiau"
        df$Species[df$Species == "Des"] <- "Desp"
        df$Species[df$Species == "rual"] <- "Rual"
        df$Species[df$Species == "HIsp"] <- "Hisp"
        df$Species[df$Species == "Hipr"] <- "Hica"
        df$Species[df$Species == "Dach"] <- "Daca"
        df$Species[df$Species == "Drin"] <- "Brin"
        df$Species[df$Species == "Dafl"] <- "Dagl"
        df$Species[df$Species == "unknown"] <- "Unknown"
        return(df)
}

change_site_phen <- function(df){
        df$Site[df$Site == "KBS"] <- "kbs"
        df$Site[df$Site == "umbs "] <- "umbs"
        df$Site[df$Site == "kbs "] <- "kbs"
        return(df)
}

#order_colnames <- c("Site", "Year", "Date", "Plot", "Species", "Action")
#colnames_ordered <- function(df){
#        df <- df[,order_colnames]
#        return(df)
#} # this won't work bc "undefined columns selected"

# Add dataframes into a list so that needed functions can be applied 
phen_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
phen_list <- lapply(phen_list, change_colnames) 
phen_list <- lapply(phen_list, remove_col, name=c("Julian", "Notes", "collector", "julian", "notes"))
#phen_list <- lapply(phen_list, colnames_ordered)
phen_list <- lapply(phen_list, change_date)
lapply(phen_list, spp_name) # look over species code to see what needs to be fixed
lapply(phen_list, site_name) # need to make these all the same for each site

# Fixing species names
phen_list <- lapply(phen_list, change_spp_phen)
lapply(phen_list, spp_name) # looks good


spp_name <- function(df){
        spp <- unique(sort(df[["Species"]]))
        return(spp)
}

unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code))

# Fixing site names
phen_list <- lapply(phen_list, change_site_phen)
lapply(phen_list, site_name) # looks good

# Merge final data
phen_merge <- rbind(phen_list$kbs_2015, phen_list$kbs_2016, phen_list$kbs_2017, phen_list$kbs_2018, phen_list$kbs_2019, phen_list$kbs_2020,
                    phen_list$umbs_2016, phen_list$umbs_2017, phen_list$umbs_2018, phen_list$umbs_2019, phen_list$umbs_2020)
str(phen_merge)
write.csv(phen_merge, file="L1/reproductive_phenology/final_flw_sd_L1.csv")
