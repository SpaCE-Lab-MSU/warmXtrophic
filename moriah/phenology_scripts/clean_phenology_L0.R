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

# Source in needed functions
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
#umbs_2015 <- read.csv("L0/UMBS/2015/umbs_flwr_sd_2015.csv")
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

order_colnames <- c("Site", "Year", "Date", "Plot", "Species", "Action")
colnames_ordered <- function(df){
        df <- df[,order_colnames]
        return(df)
} # this won't work bc "undefined columns selected"

# Add dataframes into a list so that needed functions can be applied 
phen_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
phen_list <- lapply(phen_list, change_colnames) # this did not work for kbs_2015 & kbs_2016
phen_list <- lapply(phen_list, remove_col, name=c("Julian", "Notes", "collector", "julian", "notes"))
phen_list <- lapply(phen_list, colnames_ordered)
phen_list <- lapply(phen_list, change_date)
lapply(phen_list, spp_name)

unique(phen_list$Species) #this doesn't work
setdiff(unique(phen_list$Species), unique(taxa$code)) #this doesn't work

# Merge final data
#phen_merge <- rbind(phen_list$kbs_2015, phen_list$kbs_2016, phen_list$kbs_2017, phen_list$kbs_2018, phen_list$kbs_2019, phen_list$kbs_2020,
#                    phen_list$umbs_2016, phen_list$umbs_2017, phen_list$umbs_2018, phen_list$umbs_2019, phen_list$umbs_2020)
#str(phen_merge)
#write.csv(phen_merge, file="L1/reproductive_phenology/final_flw_sd_L1.csv")
