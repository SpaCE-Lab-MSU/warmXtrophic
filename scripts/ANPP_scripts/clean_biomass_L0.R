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

# Set working directory to Google Drive
# Pat said he doesn't setwd --> figure out .renviron
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in csv files
## KBS
kbs_biomass <- read.csv("L0/KBS/2020/kbs_ancillary_biomass_2020.csv")
kbs_plant_comp <- read.csv("L0/KBS/2020/kbs_ancillary_plantcomp_2020.csv")

# UMBS

# Clean data
## KBS
View(kbs_biomass)
kbs_biomass <- kbs_biomass %>% 
        select(-dry_weight_g, -bag, -notes, -bag_size, -bag_code, -weight, -n_bags, -bag_weight)

View(kbs_plant_comp)
kbs_plant_comp <- kbs_plant_comp %>% 
        select(-Julian, -Notes)
str(kbs_plant_comp)

# Change column names to lowercase so that we can merge with the biomass file
names(kbs_plant_comp) <- tolower(names(kbs_plant_comp))
str(kbs_plant_comp)

kbs_ANPP <- merge(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))
kbs_ANPP <- kbs_ANPP %>% 
        select(-site.y, -date.x, )
colnames(kbs_ANPP) <- sub("site.x", "site", colnames(kbs_ANPP))
colnames(kbs_ANPP) <- sub("date.y", "date", colnames(kbs_ANPP))

colnames(kbs_ANPP) <- sub("final_biomass_g", "biomass", colnames(kbs_ANPP))
View(kbs_ANPP)

kbs_ANPP <- kbs_ANPP[, c("site", "date", "plot", "species", "cover", "biomass")]

## UMBS

# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(kbs_ANPP, file="L1/final_ANPP.csv")                     