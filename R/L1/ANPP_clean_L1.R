# TITLE:          warmXtrophic ANPP biomass and plant composition cleanup
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing ANPP biomass and plant composition data for both kbs and umbs sites is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           October, 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory 
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")

################################################################################
# Read in csv files
taxon <- read.csv(file.path(L0_dir,"taxon.csv")) # taxon meta data 

# Source in needed functions
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/ANPP_functions_L1.R") 
# need to figure out how this works with the .environ?

## KBS
kbs_biomass_20 <- read.csv(file.path(L0_dir, "KBS/2020/kbs_ancillary_biomass_2020.csv"))
kbs_plantcomp_20 <- read.csv(file.path(L0_dir, "KBS/2020/kbs_ancillary_plantcomp_2020.csv"))

#kbs_biomass_21 <- read.csv(file.path(L0_dir, "KBS/2021/kbs_ANPP_biomass_2021.csv"))
#kbs_plantcomp_21 <- read.csv(file.path(L0_dir, "KBS/2021/kbs_ANPP_plantcomp_2021.csv"))

# UMBS
umbs_biomass_20 <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_ancillary_ANPP_2020.csv"))
umbs_plantcomp_20 <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_ancillary_plantcomp_2020.csv"))

umbs_biomass_21 <- read.csv(file.path(L0_dir, "UMBS/2021/umbs_ANPP_biomass_2021.csv"))
umbs_plantcomp_21 <- read.csv(file.path(L0_dir, "UMBS/2021/umbs_ANPP_plantcomp_2021.csv"))

################################################################################
# Clean data - 2020
## KBS

# Biomass data
View(kbs_biomass_20)
# get rid of unnecessary columns
kbs_biomass_20 <- remove_col(kbs_biomass_20, name=c("dry_weight_g", "notes", "bag", "bag_size", "bag_code", "weight",
                                                      "n_bags", "bag_weight", "date"))
str(kbs_biomass_20)

# Plant Comp data
View(kbs_plantcomp_20)
# get rid of unnecessary columns
kbs_plantcomp_20 <- remove_col(kbs_plantcomp_20, name=c("Julian", "Notes", "Date"))
str(kbs_plantcomp_20)

# Change column names to lowercase so that we can merge with the biomass file
names(kbs_plantcomp_20) <- tolower(names(kbs_plantcomp_20))
str(kbs_plantcomp_20)

# Merge KBS biomass and plant comp data together
#kbs_ANPP <- merge(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))
kbs_ANPP_20 <- full_join(kbs_biomass_20, kbs_plantcomp_20, by = c("plot", "species", "site"))
View(kbs_ANPP_20)

colnames(kbs_ANPP_20) <- sub("final_biomass_g", "weight_g", colnames(kbs_ANPP_20)) # change column name to biomass

kbs_ANPP_20$year <- "2020" # add year to data frame

kbs_ANPP_20 <- kbs_ANPP_20[, c("site", "year", "plot", "species", "cover", "weight_g")] # reorganize column order
View(kbs_ANPP_20)

## UMBS

# Biomass data
View(umbs_biomass_20)
# get ride of unwanted columns
umbs_biomass_20 <- remove_col(umbs_biomass_20, name=c("dry_weight..g.", "X", "dry_weight_kbs", "X.1", "dried.bag.weight", 
                                                "bag", "notes", "X.2", "date"))
umbs_biomass_20 <- umbs_biomass_20[-c(93, 94, 95),] # get ride of unwanted rows

# Plant Comp data
View(umbs_plantcomp_20)
# get ride of unwanted columns
umbs_plantcomp_20 <- remove_col(umbs_plantcomp_20, name=c("Julian", "Notes", "Date"))
str(umbs_plantcomp_20)

# Change column names to lowercase so that we can merge with the biomass file
names(umbs_plantcomp_20) <- tolower(names(umbs_plantcomp_20))

#umbs_ANPP <- merge(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))
umbs_ANPP_20 <- full_join(umbs_biomass_20, umbs_plantcomp_20, by = c("plot", "species", "site"))
View(umbs_ANPP_20)

umbs_ANPP_20$year <- "2020" # add year to data frame

umbs_ANPP_20 <- umbs_ANPP_20[, c("site", "year", "plot", "species", "cover", "weight_g")] # reorganize column order
View(umbs_ANPP_20)

# Merge the kbs and umbs files together
#final_ANPP <- merge(kbs_ANPP, umbs_ANPP, by = c("site", "year", 'plot", "species"))
final_ANPP_20 <- full_join(kbs_ANPP_20, umbs_ANPP_20)
View(final_ANPP_20)

# Now that the two sites are merged, now the species list needs to be cleaned like the other scripts we have i.e
# phenology and plant comp

spp_name(final_ANPP_20) # need to fix a few species names
site_name(final_ANPP_20) # need to change one site name

final_ANPP_20 <- change_spp(final_ANPP_20)
spp_name(final_ANPP_20) # looks good

final_ANPP_20 <- change_site(final_ANPP_20)
site_name(final_ANPP_20) # looks good

# remove rows with "Total" in the "species" column - this will be calculated in R later
final_ANPP_20 <- subset(final_ANPP_20, species != "Total")
View(final_ANPP_20)

################################################################################
# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final_ANPP_20, file.path(L1_dir,"ANPP/final_ANPP_2020.csv"))

################################################################################
# 2021 ANPP

# Clean data
# Biomass data
View(umbs_biomass_21)
# get rid of unnecessary columns
umbs_biomass_21 <- remove_col(umbs_biomass_21, name=c("weight_g", "dried_bag_weight", "bag_size", "bag_code", 
                                                      "measurement_type", "date"))
str(umbs_biomass_21)

umbs_biomass_21$quadrat_number <- 1

colnames(umbs_biomass_21) <- sub("plant_biomass_g", "weight_g", colnames(umbs_biomass_21)) # change column name weight_g

# Plant Comp data
View(umbs_plantcomp_21)
# get ride of unwanted columns
umbs_plantcomp_21 <- remove_col(umbs_plantcomp_21, name=c("Julian", "Notes", "Date"))
str(umbs_plantcomp_21)

# Change column names to lowercase so that we can merge with the biomass file
names(umbs_plantcomp_21) <- tolower(names(umbs_plantcomp_21))

#umbs_ANPP <- merge(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))
umbs_ANPP_21 <- full_join(umbs_biomass_21, umbs_plantcomp_21, by = c("plot", "species", "site", "quadrat_number"))
View(umbs_ANPP_21)

umbs_ANPP_21$year <- "2021" # add year to data frame

umbs_ANPP_21 <- umbs_ANPP_21[, c("site", "year", "plot", "species", "quadrat_number", "cover", "weight_g")] # reorganize column order
View(umbs_ANPP_21)

# Now that the two sites are merged, now the species list needs to be cleaned like the other scripts we have i.e
# phenology and plant comp

spp_name(umbs_ANPP_21) # need to fix a few species names
site_name(umbs_ANPP_21) # need to change one site name

umbs_ANPP_21 <- change_spp(umbs_ANPP_21)
spp_name(umbs_ANPP_21) # looks good

umbs_ANPP_21 <- change_site(umbs_ANPP_21)
site_name(umbs_ANPP_21) # looks good

View(umbs_ANPP_21)
################################################################################
# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final_ANPP_21, file.path(L1_dir,"ANPP/final_ANPP_2021.csv"))

