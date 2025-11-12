# TITLE:          warmXtrophic plant height cleanup
# AUTHORS:        Moriah Young
# COLLABORATORS:  Kara Dobson, Phoebe Zarnetske, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing ANPP biomass and plant composition data for both kbs and umbs sites is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           November, 2025

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(stringr)

# Set working directory 
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")

################################################################################
# Read in csv files
taxon <- read.csv(file.path(L0_dir,"taxon.csv")) # taxon meta data 
meta <- read.csv(file.path(L0_dir, "plot.csv")) # plot meta data

# Source in needed functions
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/biomass_functions_L1.R") 
#source("~/warmXtrophic/R/L1/biomass_functions_L1.R")

## KBS
kbs_height_25 <- read.csv(file.path(L0_dir, "KBS/2025/WarmX_2025_KBS_Goldenrod_ANPP_and_height_data.csv"))

# UMBS
umbs_height_25 <- read.csv(file.path(L0_dir, "UMBS/2025/WarmX 2025_UMBS_Fern_ANPP_and_height.csv"))

################################################################################

# Change column names to lowercase so that we can merge with the biomass file
names(kbs_height_25) <- tolower(names(kbs_height_25))
names(umbs_height_25) <- tolower(names(umbs_height_25))

# change column names to match other data frames
names(kbs_height_25)[names(kbs_height_25)=="plot_id"] <- "plot"
names(umbs_height_25)[names(umbs_height_25)=="plot_id"] <- "plot"
names(kbs_height_25)[names(kbs_height_25)=="species_code"] <- "species"
names(umbs_height_25)[names(umbs_height_25)=="species_code"] <- "species"

# Change species codes from all caps to first character capitalized and the rest lower case to match taxon data frame
umbs_height_25$species <- str_to_lower(umbs_height_25$species) # make all lowercase
umbs_height_25$species <- str_replace(umbs_height_25$species, "^.", toupper(str_sub(umbs_height_25$species, 1, 1)))


# fixing plant comp species names
spp_name(umbs_height_25) # looks good
site_name(umbs_height_25) # need to change site names to lowercase
umbs_height_25 <- change_site(umbs_height_25)
site_name(umbs_height_25) # looks good

spp_name(kbs_height_25) # looks good
site_name(kbs_height_25) # need to change site names to lowercase
kbs_height_25 <- change_site(kbs_height_25)
site_name(kbs_height_25) # looks good

# merge data
kbs_height <- full_join(kbs_height_25, meta, by = c("plot"))
View(kbs_height)

kbs_height$year <- "2025" # add year to data frame

umbs_height <- full_join(umbs_height_25, meta, by = c("plot"))
View(umbs_height)

umbs_height$year <- "2025" # add year to data frame

# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(kbs_height, file.path(L1_dir,"height/kbs_height_L1.csv"), row.names=F)
write.csv(umbs_height, file.path(L1_dir,"height/umbs_height_L1.csv"), row.names=F)


