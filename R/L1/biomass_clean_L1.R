# TITLE:          ANPP Biomass Data Cleanup
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing biomass data for both kbs and umbs sites is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           January, 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory 
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L0_dir)

# Read in csv files

# read in taxon meta data 
taxon <- read.csv(file.path(L0_dir,"taxon.csv"))

# Source in needed functions
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/biomass_functions_L1.R") 
# need to figure out how this works with the .environ?

## KBS
kbs_biomass <- read.csv(file.path(L0_dir, "KBS/2020/kbs_ancillary_biomass_2020.csv"))
kbs_plant_comp <- read.csv(file.path(L0_dir, "KBS/2020/kbs_ancillary_plantcomp_2020.csv"))

# UMBS
umbs_biomass <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_ancillary_ANPP_2020.csv"))
umbs_plant_comp <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_ancillary_plantcomp_2020.csv"))

# Clean data
## KBS
View(kbs_biomass)
kbs_biomass <- kbs_biomass %>% 
        select(-dry_weight_g, -bag, -notes, -bag_size, -bag_code, -weight, -n_bags, -bag_weight, -date)
kbs_biomass <- lapply(kbs_biomass, remove_col, name=c("dry_weight_g", "notes", "bag", "bag_size", "bag_code", "weight",
                                                      "n_bags", "bag_weight", "date"))
str(kbs_biomass)

View(kbs_plant_comp)
# get rid of unnecessary columns
kbs_plant_comp <- kbs_plant_comp %>% 
        select(-Julian, -Notes, Date)
kbs_plant_comp <- lapply(kbs_plant_comp, remove_col, name=c("Julian", "Notes", "Date"))
str(kbs_plant_comp)

# Change column names to lowercase so that we can merge with the biomass file
names(kbs_plant_comp) <- tolower(names(kbs_plant_comp))
str(kbs_plant_comp)

#kbs_ANPP <- merge(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))
kbs_ANPP <- full_join(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))
View(kbs_ANPP)

kbs_ANPP <- kbs_ANPP %>% 
        select(-site.y)
colnames(kbs_ANPP) <- sub("site.x", "site", colnames(kbs_ANPP))
#colnames(kbs_ANPP) <- sub("date.y", "date", colnames(kbs_ANPP))
colnames(kbs_ANPP) <- sub("final_biomass_g", "biomass", colnames(kbs_ANPP))

kbs_ANPP$year <- "2020"
kbs_ANPP$site <- "kbs"

kbs_ANPP <- kbs_ANPP[, c("site", "year", "plot", "species", "cover", "biomass")]
View(kbs_ANPP)

## UMBS
View(umbs_biomass)
umbs_biomass <- umbs_biomass %>% 
        select(-dry_weight..g., -X, -dry_weight_kbs, -X.1, -dried.bag.weight, -bag, -notes, -X.2, -date) # get ride of
# unwanted columns
umbs_biomass <- umbs_biomass[-c(93, 94, 95),] # get ride of unwanted rows

View(umbs_plant_comp)
umbs_plant_comp <- umbs_plant_comp %>% 
        select(-Julian, -Notes, -Date) # get ride of unwanted columns
str(umbs_plant_comp)

# Change column names to lowercase so that we can merge with the biomass file
names(umbs_plant_comp) <- tolower(names(umbs_plant_comp))

#umbs_ANPP <- merge(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))
umbs_ANPP <- full_join(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))
View(umbs_ANPP)

# get rid of site.y column bc there are two site columns currently after the join
umbs_ANPP <- umbs_ANPP %>% 
        select(-site.y)
colnames(umbs_ANPP) <- sub("site.x", "site", colnames(umbs_ANPP))
#colnames(umbs_ANPP) <- sub("date.y", "date", colnames(umbs_ANPP))
colnames(umbs_ANPP) <- sub("weight_g", "biomass", colnames(umbs_ANPP))

umbs_ANPP$year <- "2020"
umbs_ANPP$site <- "umbs"

umbs_ANPP <- umbs_ANPP[, c("site", "year", "plot", "species", "cover", "biomass")]
View(umbs_ANPP)

# Merge the kbs and umbs files together
#final_ANPP <- merge(kbs_ANPP, umbs_ANPP, by = c("site", "year", 'plot", "species"))
final_ANPP <- full_join(kbs_ANPP, umbs_ANPP)
View(final_ANPP)

# Now that the two sites are merged, now the species list needs to be cleaned like the other scripts we have i.e
# phenology and plant comp

lapply(final_ANPP, spp_name) # need to fix a few species names
lapply(final_ANPP, site_name) # need to make these all the same for each site



# Need to calculate total biomass for each plot and put zeros where appropriate

# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final_ANPP, file = "L1/final_ANPP.csv")  
