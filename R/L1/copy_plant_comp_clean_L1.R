# TITLE:          warmXtrophic Plant composition cleanup
# AUTHORS:        Kara Dobson, Phoebe Zarnetske, Moriah Young
# COLLABORATORS:  Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing plant comp data for all sites & years is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           December, 2020; updated with KBS 2021 data in Nov, 2021, updated Jan. 2023

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Source in needed functions from the github repo
#source("~/warmXtrophic/R/L1/plant_comp_functions_L1.R") # Kara's
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/plant_comp_functions_L1.R")
#source("~/DATA/git/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions_L1.R") # PLZ's location

# Set working directory
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L1_dir)

# Read in data (only need columns 1-7 for the umbs files)
meta <- read.csv(file.path(L0_dir, "plot.csv"))
taxon <- read.csv(file.path(L0_dir,"taxon.csv"))
taxon <- taxon[,-c(6,7,16,18)] # get rid of unneeded columns
kbs_2015 <- read.csv(file.path(L0_dir,"KBS/2015/kbs_plant_comp_2015.csv"))
kbs_2016 <- read.csv(file.path(L0_dir,"KBS/2016/kbs_plant_comp_2016.csv"))
kbs_2017 <- read.csv(file.path(L0_dir,"KBS/2017/kbs_plant_comp_2017.csv"))
kbs_2018 <- read.csv(file.path(L0_dir,"KBS/2018/kbs_plant_comp_2018.csv"))
kbs_2019 <- read.csv(file.path(L0_dir,"KBS/2019/kbs_plant_comp_2019.csv"))
kbs_2020 <- read.csv(file.path(L0_dir,"KBS/2020/kbs_plant_comp_2020.csv"))
kbs_2021 <- read.csv(file.path(L0_dir,"KBS/2021/kbs_plant_comp_2021.csv"))
umbs_2015 <- read.csv(file.path(L0_dir,"UMBS/2015/umbs_plant_comp_2015.csv"))
umbs_2016 <- read.csv(file.path(L0_dir,"UMBS/2016/umbs_plant_comp_2016.csv"))
umbs_2017 <- read.csv(file.path(L0_dir,"UMBS/2017/umbs_plant_comp_2017.csv"))
umbs_2018 <- read.csv(file.path(L0_dir,"UMBS/2018/umbs_plant_comp_2018.csv"))
umbs_2019 <- read.csv(file.path(L0_dir,"UMBS/2019/umbs_plantcomp_2019.csv"))[,1:7]
umbs_2020 <- read.csv(file.path(L0_dir,"UMBS/2020/umbs_plantcomp_2020.csv"))[,1:7]
umbs_2021 <- read.csv(file.path(L0_dir,"UMBS/2021/umbs_plantcomp_2021.csv"))

# remove all empty rows for umbs_2019
umbs_2019 <- umbs_2019[-c(5047:6024), ]

# add site column for kbs & umbs 2016 and 2021
kbs_2016$Site <- "kbs"
kbs_2021$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column names
colnames(umbs_2016) <- sub("Percent_Cover", "Cover", colnames(umbs_2016))
colnames(kbs_2021) <- sub("Quad_ID", "Plot", colnames(kbs_2021))
colnames(kbs_2021) <- sub("Species_Code", "Species", colnames(kbs_2021))

# Add dataframes into a list so that needed functions can be applied 
comp_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, kbs_2021=kbs_2021,
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020, umbs_2021=umbs_2021)
comp_list <- lapply(comp_list, remove_col, name=c("Julian", "Notes", "Quadrat", "Julian_Day", "Project"))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names
lapply(comp_list, site_name) # need to make these all the same for each site

# Fixing species names
comp_list <- lapply(comp_list, change_spp)
lapply(comp_list, spp_name)

# Fixing site name
comp_list <- lapply(comp_list, change_site)
lapply(comp_list, site_name)

# Merge final data
comp_merge <- rbind(comp_list$kbs_2015, comp_list$kbs_2016, comp_list$kbs_2017, comp_list$kbs_2018, comp_list$kbs_2019, comp_list$kbs_2020, comp_list$kbs_2021,
                    comp_list$umbs_2015, comp_list$umbs_2016, comp_list$umbs_2017, comp_list$umbs_2018, comp_list$umbs_2019, comp_list$umbs_2020, comp_list$umbs_2021)
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
plant_comp_merge <- left_join(comp_merge, meta, by = "plot")
sort(unique(plant_comp_merge$species))

# taxon contains "site" which is the site where the species is found on our meta-data table, but those data exist in our plant_comp_merge dataset already.
# Delete "site" from taxon so it doesn't accidentally get merged in.
taxon$site<-NULL
plant_comp_merge2 <- left_join(plant_comp_merge, taxon, by = "species")
sort(unique(plant_comp_merge2$species))

# remove species not found in our data set
plant_comp_merge3 <- plant_comp_merge2[!is.na(plant_comp_merge2$cover),]
sort(unique(plant_comp_merge3$species))

# remove unnecessary columns
#plant_comp_merge3$old_code <- NULL
#plant_comp_merge3$old_name <- NULL
plant_comp_merge3$resolution <- NULL
plant_comp_merge3$rhizomatous <- NULL
plant_comp_merge3$MH_rhizomatous_suggestion <- NULL

# re organize order of column names 
plant_comp_merge3 <- plant_comp_merge3[, c("site", "plot", "species", "cover", "date", "julian", "year", "month", "treatment_key", "state",
                                           "insecticide", "scientific_name", "common_name", "USDA_code", "LTER_code",
                                           "origin", "group", "family", "duration", "growth_habit")]

# remove "total" species bc it's not a species
plant_comp_merge4 <- plant_comp_merge3[!(plant_comp_merge3$species=="Total"),]

sort(unique(plant_comp_merge4$species))

# Upload clean data csv to google drive without the index column
write.csv(plant_comp_merge4, file.path(L1_dir, "plant_composition/final_plantcomp_L1.csv"), row.names=F)

# The below code removes species that are rare in the data set or were only recorded once or twice. It also removes
# those that aren't species, like "Bare_Ground" or "Litter"

# species that were removed previously
# Alpe, Apca, Brin, Juni, Plla, Romu, Stme, Umsp, Ulsp, Spsp, Acru, Oebi, Besp, Prse, Quru, Prpe, 
# Veth, Anma, Vaan, Gnul, Trdu, Alpe, Sypi, Hipi, Pops, Acsy, Lapu, Sysa

# Those that should also be removed
# Bare_Ground, Vert_Litter, Animal_Disturbance, Litter, Brown, Moss/Lichen, Lisp (lichen), Herbicide
# Cesp. Ruag, Soju, Cofo, Coca, Leca, Crsp, Erci, Asun, Amla (these either need to be edited or removed)

# removed but maybe shouldn't be:
# Brin,

plant_comp_merge5 <- remove_spp(plant_comp_merge4)
sort(unique(plant_comp_merge5$species))


# Upload clean data csv to google drive without the index column - this has species that have been removed
write.csv(plant_comp_merge3, file.path(L1_dir, "plant_composition/final_plantcomp_species_removed_L1.csv"), row.names=F)
