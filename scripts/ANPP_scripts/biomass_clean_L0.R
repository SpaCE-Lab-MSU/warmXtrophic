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
umbs_biomass <- read.csv("L0/UMBS/2020/umbs_ancillary_ANPP_2020.csv")
umbs_plant_comp <- read.csv("L0/UMBS/2020/umbs_ancillary_plantcomp_2020.csv")

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

#kbs_ANPP <- merge(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))
kbs_ANPP <- full_join(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))

kbs_ANPP <- kbs_ANPP %>% 
        select(-site.y, -date.x)
colnames(kbs_ANPP) <- sub("site.x", "site", colnames(kbs_ANPP))
colnames(kbs_ANPP) <- sub("date.y", "date", colnames(kbs_ANPP))

colnames(kbs_ANPP) <- sub("final_biomass_g", "biomass", colnames(kbs_ANPP))

kbs_ANPP <- kbs_ANPP[, c("site", "date", "plot", "species", "cover", "biomass")]
View(kbs_ANPP)

## UMBS
View(umbs_biomass)
umbs_biomass <- umbs_biomass %>% 
        select(-dry_weight..g., -X, -dry_weight_kbs, -X.1, -dried.bag.weight, -bag, -notes, -X.2)

umbs_biomass <- umbs_biomass[-c(93, 94, 95),]

umbs_plant_comp <- umbs_plant_comp %>% 
        select(-Julian, -Notes)
str(umbs_plant_comp)

# Change column names to lowercase so that we can merge with the biomass file
names(umbs_plant_comp) <- tolower(names(umbs_plant_comp))
str(umbs_plant_comp)

#umbs_ANPP <- merge(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))
umbs_ANPP <- full_join(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))

umbs_ANPP <- umbs_ANPP %>% 
        select(-site.y, -date.x)
colnames(umbs_ANPP) <- sub("site.x", "site", colnames(umbs_ANPP))
colnames(umbs_ANPP) <- sub("date.y", "date", colnames(umbs_ANPP))

colnames(umbs_ANPP) <- sub("weight_g", "biomass", colnames(umbs_ANPP))

umbs_ANPP <- umbs_ANPP[, c("site", "date", "plot", "species", "cover", "biomass")]
View(umbs_ANPP)

# Merge the kbs and umbs files together
final_ANPP <- merge(kbs_ANPP, umbs_ANPP, by = c("site", "plot", "species"))

# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final_ANPP, file="L1/final_ANPP.csv")                     