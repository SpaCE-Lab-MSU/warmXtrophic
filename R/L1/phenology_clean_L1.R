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
# setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Source in needed functions
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/phenology_functions_L1.R")

# Read in data
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
list.files(L0_dir)
meta <- read.csv(file.path(L0_dir, "plot.csv"))
taxon <- read.csv(file.path(L0_dir, "taxon.csv"))
colnames(taxon) <- sub("code", "species", colnames(taxon))

### Read in meta data files
# Read in species list
#taxa <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L0/taxon.csv", stringsAsFactors=F)
# Change column name for from "code" to "Species" to match cleaned phenology data
#colnames(taxon) <- sub("code", "species", colnames(taxon))

# Read in plot list
#plot_info <- read.csv("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L0/plot.csv")

# Read in phenology data
kbs_2015 <- read.csv(file.path(L0_dir, "KBS/2015/kbs_flwr_sd_2015.csv"))
kbs_2016 <- read.csv(file.path(L0_dir, "KBS/2016/kbs_flwr_sd_2016.csv"))
kbs_2017 <- read.csv(file.path(L0_dir, "KBS/2017/kbs_flwr_sd_2017.csv"))
kbs_2018 <- read.csv(file.path(L0_dir, "KBS/2018/kbs_flwr_sd_2018.csv"))
kbs_2019 <- read.csv(file.path(L0_dir, "KBS/2019/kbs_flwr_sd_2019.csv"))
kbs_2020 <- read.csv(file.path(L0_dir, "KBS/2020/kbs_flwr_sd_2020.csv"))
umbs_2015 <- read.csv(file.path(L0_dir, "UMBS/2015/umbs_flwr_sd_2015.csv"))
umbs_2016 <- read.csv(file.path(L0_dir, "UMBS/2016/umbs_flwr_sd_2016.csv"))
umbs_2017 <- read.csv(file.path(L0_dir, "UMBS/2017/umbs_flwr_sd_2017.csv"))
umbs_2018 <- read.csv(file.path(L0_dir, "UMBS/2018/umbs_flwr_sd_2018.csv"))
umbs_2019 <- read.csv(file.path(L0_dir, "UMBS/2019/umbs_flwr_sd_2019.csv"))
umbs_2020 <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_flwr_sd_2020.csv"))

# Add "Site" column for kbs and umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column name for kbs 2015
colnames(kbs_2015) <- sub("event", "Action", colnames(kbs_2015))
colnames(umbs_2015) <- sub("event", "Action", colnames(umbs_2015))

# remove species from UMBS (and not KBS)
#remove_spp <- function(df, site, species){
#        vec <- 
#        df = df[!grepl("species",df$Species),]
#        return(df)
#}

umbs_2016 <- umbs_2016[!grepl("Vear",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Vear",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Vear",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Vear",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Vear",umbs_2020$Species),]

umbs_2016 <- umbs_2016[!grepl("Elre",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Elre",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Elre",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Elre",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Elre",umbs_2020$Species),]

umbs_2016 <- umbs_2016[!grepl("Rusp",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Rusp",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Rusp",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Rusp",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Rusp",umbs_2020$Species),]

umbs_2016 <- umbs_2016[!grepl("Ruag",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Ruag",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Ruag",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Ruag",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Ruag",umbs_2020$Species),]

umbs_2016 <- umbs_2016[!grepl("Rual",umbs_2016$Species),]
umbs_2017 <- umbs_2017[!grepl("Rual",umbs_2017$Species),]
umbs_2018 <- umbs_2018[!grepl("Rual",umbs_2018$Species),]
umbs_2019 <- umbs_2019[!grepl("Rual",umbs_2019$Species),]
umbs_2020 <- umbs_2020[!grepl("Rual",umbs_2020$Species),]

# Add dataframes into a list so that needed functions can be applied 
phen_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)

# Apply functions
phen_list <- lapply(phen_list, change_colnames) 
phen_list <- lapply(phen_list, remove_col, name=c("Julian", "Notes", "collector", "julian", "notes"))
phen_list <- lapply(phen_list, change_date)
phen_list <- lapply(phen_list, change_plotID)

# Look over the contents of species, site, and dates after applying functions
lapply(phen_list, spp_name) # look over species code to see what needs to be fixed
lapply(phen_list, site_name) # need to make these all the same for each site
lapply(phen_list, date_check) # see if any dates were entered incorrectly, like the year
lapply(phen_list, plot_check) # see if there are any repeat plot IDs

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

# Removing species
phen_list <- lapply(phen_list, remove_spp)
lapply(phen_list, spp_name) 

# Combining some species codes
phen_list <- lapply(phen_list, change_spp_2)
lapply(phen_list, spp_name)

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
sort(unique(phen_merge$Species))

# Change column names to lowercase
names(phen_merge) <- tolower(names(phen_merge))

# Fix date column & add column for the year, month, and julian day
phen_merge$date <- as.Date(phen_merge$date, format="%m/%d/%Y")
phen_merge$year <- format(phen_merge$date,format="%Y")
phen_merge$month <- format(phen_merge$date,format="%m")
phen_merge$julian <- format(phen_merge$date, "%j")
head(phen_merge)
str(phen_merge)

# Make julian column numeric
phen_merge$julian <- as.numeric(phen_merge$julian)
str(phen_merge)

# Merge meta data with phenology data - merge cleaned data with the plot and species level information
phen_merge2 <- merge(phen_merge, plot_info, by = "plot")

sort(unique(phen_merge$plot))
phen_merge2 <- left_join(plot_info, phen_merge, by = "plot")
sort(unique(phen_merge2$species))

# taxon contains "site" which is the site where the species is found on our meta-data table, but those data exist in our plant_comp_merge dataset already.
# Delete "site" from taxon so it doesn't accidentally get merged in.
taxa$site<-NULL

# merge taxon meta data with the phenology data
phen_data <- left_join(phen_merge2, taxa, by = "species")
sort(unique(phen_data$species))

# remove unnecessary columns
phen_data$old_code <- NULL
phen_data$old_name <- NULL
phen_data$resolution <- NULL
phen_data$old_species <- NULL

# Change column names to lowercase (again)
names(phen_data) <- tolower(names(phen_data))
str(phen_data)

phen_data <- phen_data[, c("site", "plot", "species", "action", "date", "julian", "year", "month", "treatment_key", "state",
                          "insecticide", "scientific_name", "common_name", "usda_species", "lter_species",
                          "origin", "group", "family", "duration", "growth_habit")]

str(phen_data)

# make a column that breaks down years as 1, 2, 3, 4, 5, 6 and into factors
phen_data$year_factor <- 
        ifelse(phen_data$year == 2015, "1",
               ifelse(phen_data$year == 2016, "2",
                      ifelse(phen_data$year == 2017, "3",
                             ifelse(phen_data$year == 2018, "4",
                                    ifelse(phen_data$year == 2019, "5",
                                           ifelse(phen_data$year == 2020, "6", NA))))))

# Order warm and ambient so that warm shows up first in plotting (and is default is red = warm; blue = ambient). First make it a factor
phen_data$state <- as.factor(phen_data$state)
levels(phen_data$state)
# [1] "ambient" "warmed" 
phen_data$state <- factor(phen_data$state, levels(phen_data$state)[c(2,1)])
levels(phen_data$state)
# [1] "warmed"  "ambient"

# write a new csv with the cleaned and merge data and upload to the shared google drive
write.csv(phen_data, file="L1/phenology/final_flwr_sd_L1.csv")

# Create separate data frames for flowering and seeding
phen_flwr <- subset(phen_data, action == "flower")
phen_sd <- subset(phen_data, action == "seed")

#### FLOWERING ####
### Create a data frame at the SPECIES LEVEL that includes median date of flower and first flower date
# First Flower by SPECIES LEVEL - filter data to contain the date of first flower for each species at each plot
FirstFlwr_spp <- phen_flwr %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, treatment_key, year_factor) %>%
        summarize(julian_min = min(julian, na.rm=T))

# Median Flower Date by sPECIES LEVEL - filter data to contain the median date of flower for each species at each plot
MedianFlwr_spp <- phen_flwr %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, treatment_key, year_factor) %>%
        summarize(julian_median = median(julian, na.rm=T))

# Merge the two data frames above so that you have one data frame that includes median date of flower and first date
# of flower at SPECIES LEVEL
phen_flwr_spp <- merge(FirstFlwr_spp, MedianFlwr_spp)

# write a new csv with flowering data at the SPECIES LEVEL and upload to the shared google drive
write.csv(phen_flwr_spp, file="L1/phenology/final_flwr_species_L1.csv")

### Create a data frame at the PLOT LEVEL that includes median date of flower and first flower
# First Flower Date by PLOT LEVEL
FirstFlwr_plot <- phen_flwr %>%
        group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor) %>%
        summarize(julian_min = min(julian, na.rm=T))

# Median Flower Date by PLOT LEVEL
MedianFlwr_plot <- phen_flwr %>%
        group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor) %>%
        summarize(julian_median = median(julian, na.rm=T))

# Merge the two data frames above so that you have one data frame that includes median date of flower and first date
# of flower at PLOT LEVEL
phen_flwr_plot <- merge(FirstFlwr_plot, MedianFlwr_plot)

# write a new csv with flowering data at the PLOT LEVEL and upload to the shared google drive
write.csv(phen_flwr_plot, file="L1/phenology/final_flwr_plot_L1.csv")

#### SEED SET ####
### Create a data frame at the SPECIES LEVEL that includes first date of seed
# First Seed by SPECIES LEVEL - filter data to contain the date of first seed for each species at each plot
FirstSd_spp <- phen_sd %>%
        group_by(plot, year, species, state, site, action, origin, insecticide, treatment_key, year_factor) %>%
        summarize(julian_min = min(julian, na.rm=T))

# write a new csv with first seed date at the SPECIES LEVEL and upload to the shared google drive
write.csv(FirstSd_spp, file="L1/phenology/final_sd_species_L1.csv")

### Create a data frame at the PLOT LEVEL that includes first date of seed
# First Flower Date by PLOT LEVEL
FirstSd_plot <- phen_sd %>%
        group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor) %>%
        summarize(julian_min = min(julian, na.rm=T))

# write a new csv with first seed date at the PLOT LEVEL and upload to the shared google drive
write.csv(FirstSd_plot, file="L1/phenology/final_sd_plot_L1.csv")
