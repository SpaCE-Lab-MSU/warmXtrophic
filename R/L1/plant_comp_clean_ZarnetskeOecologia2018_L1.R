# TITLE:          warmXtrophic Plant composition cleanup for Lotte Korell et al. synthesis project - effects of temperature change on plant diversity
#                       script modified from 2015-2021 analysis, for 2015 & 2016 data only to align with Welshofer et al. Oecologia 2018 paper
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Kara Dobson, Moriah Young, Kileigh Welshofer
# DATA INPUT:     Level 0 (L0) data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A Level 1 (L1) csv file containing plant comp data for all sites & years is created
# PROJECT:        warmXtrophic
# DATE:           Original script from 2017; updated December, 2020, April 2022

# Clear all existing data
rm(list=ls())

# Source in needed functions from the github repo (or point to the script where you've saved it)
source("/Users/phoebezarnetske/Documents/GitHub/warmXtrophic/R/L1/plant_comp_functions_L1.R")

# Set working directory
L0_dir <- "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L0"
L1_dir <- "/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1"

# Read in data (only need columns 1-7 for the umbs files)
meta <- read.csv(file.path(L0_dir, "plot_2015_2016.csv"))
taxon <- read.csv(file.path(L0_dir,"taxon.csv"))
kbs_2015 <- read.csv(file.path(L0_dir,"KBS/2015/kbs_plant_comp_2015.csv"))
kbs_2016 <- read.csv(file.path(L0_dir,"KBS/2016/kbs_plant_comp_2016.csv"))
umbs_2015 <- read.csv(file.path(L0_dir,"UMBS/2015/umbs_plant_comp_2015.csv"))
umbs_2016 <- read.csv(file.path(L0_dir,"UMBS/2016/umbs_plant_comp_2016.csv"))

# add site column for kbs & umbs 2016 
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column names
colnames(umbs_2016) <- sub("Percent_Cover", "Cover", colnames(umbs_2016))

# Add dataframes into a list so that needed functions can be applied 
comp_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016,
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016)
comp_list <- lapply(comp_list, remove_col, name=c("Julian", "Notes", "Quadrat", "Julian_Day", "Project"))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names
lapply(comp_list, site_name) # need to make these all the same for each site

# Fixing species names
comp_list <- lapply(comp_list, change_spp)
lapply(comp_list, spp_name)

# Rare species were not removed in the Oecologia 2018 analysis so we are not using "remove_spp" function. 

# Fixing site name
comp_list <- lapply(comp_list, change_site)
lapply(comp_list, site_name)

# Merge final data
comp_merge <- rbind(comp_list$kbs_2015, comp_list$kbs_2016, 
                    comp_list$umbs_2015, comp_list$umbs_2016)
str(comp_merge)
sort(unique(comp_merge$Species))

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
plant_comp_merge <- left_join(meta, comp_merge, by = "plot")
sort(unique(plant_comp_merge$species))

# taxon contains "site" which is the site where the species is found on our meta-data table, but those data exist in our plant_comp_merge dataset already.
# Delete "site" from taxon so it doesn't accidentally get merged in.
taxon$site<-NULL
plant_comp_merge2 <- left_join(taxon, plant_comp_merge, by = "species")
sort(unique(plant_comp_merge2$species))

# remove species not found in our dataset
plant_comp_merge3 <- plant_comp_merge2[!is.na(plant_comp_merge2$cover),]
sort(unique(plant_comp_merge3$species))

# remove unnecessary columns
plant_comp_merge3$old_code <- NULL
plant_comp_merge3$old_name <- NULL
plant_comp_merge3$resolution <- NULL

# re organize order of column names 
plant_comp_merge4 <- plant_comp_merge3[, c("site", "plot", "state","species", "cover", "date", "julian", "year", "month", "treatment_key", "state",
                           "insecticide", "mammals", "scientific_name", "common_name", "USDA_code", "LTER_code",
                           "origin", "group", "family", "duration", "growth_habit")]

# Subset out only necessary columns for this meta-analysis
plant_comp_merge5 <- plant_comp_merge4[, c("year", "date", "julian", "site", 
                                           "treatment_key", "state", "insecticide", "mammals", "plot", 
                                           "species", "scientific_name", "common_name",
                                           "cover")]
# Upload clean data csv to google drive without the index column
write.csv(plant_comp_merge5, file.path(L1_dir, "plant_composition/ZarnetskeOecologia2018cover.csv"), row.names=FALSE)
