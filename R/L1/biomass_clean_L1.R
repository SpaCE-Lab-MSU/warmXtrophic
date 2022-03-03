# TITLE:          warmXtrophic ANPP biomass and plant composition cleanup
# AUTHORS:        Moriah Young, Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing ANPP biomass and plant composition data for both kbs and umbs sites is uploaded
#                 to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           October, 2021; updated Feb 2022 by Kara

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
meta <- read.csv(file.path(L0_dir, "plot.csv"))

# Source in needed functions
#source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/ANPP_functions_L1.R") 
source("~/warmXtrophic/R/L1/biomass_functions_L1.R") # this works for Kara
# need to figure out how this works with the .environ?

## KBS
kbs_biomass_20 <- read.csv(file.path(L0_dir, "KBS/2020/kbs_ancillary_biomass_2020.csv"))
kbs_plantcomp_20 <- read.csv(file.path(L0_dir, "KBS/2020/kbs_ancillary_plantcomp_2020.csv"))

kbs_biomass_21 <- read.csv(file.path(L0_dir, "KBS/2021/kbs_biomass_2021.csv"))
kbs_plantcomp_21 <- read.csv(file.path(L0_dir, "KBS/2021/kbs_anpp_plant_comp_2021.csv"))

# UMBS
umbs_biomass_20 <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_ancillary_ANPP_2020.csv"))
umbs_plantcomp_20 <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_ancillary_plantcomp_2020.csv"))

umbs_biomass_21 <- read.csv(file.path(L0_dir, "UMBS/2021/umbs_ANPP_biomass_2021.csv"))
umbs_plantcomp_21 <- read.csv(file.path(L0_dir, "UMBS/2021/umbs_ANPP_plantcomp_2021.csv"))

################################################################################
# Making separate datasets for 2020 and 2021 biomass data
# 2020 data was collected in ancillary plots, so no plant material was collected from within treatments
# We'd need to regress this biomass data against plant heights to come up with an equation to convert height to weight
# This hasn't been done

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
# Note from Kara: do we want to merge these? we can use this data to see how accurately % cover reflects
# plant abundance with the biomass data, but biomass was collected over the span of a month while % cover was
# collected on one data in August, so the correlation may not be accurate here. Leaving it for now

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
 
# checking species and site names
spp_name(final_ANPP_20) # need to fix a few species names
site_name(final_ANPP_20) # need to change one site name

final_ANPP_20 <- change_spp(final_ANPP_20)
spp_name(final_ANPP_20) # looks good

final_ANPP_20 <- change_site(final_ANPP_20)
site_name(final_ANPP_20) # looks good

# remove rows with "Total" in the "species" column - this will be calculated in R later
# Kara's notes: could keep "total" and compare total biomass per plot w/ total % cover
# final_ANPP_20 <- subset(final_ANPP_20, species != "Total")
View(final_ANPP_20)

# merging taxon info with dataframe
colnames(taxon)[which(names(taxon) == "code")] <- "species" # changing column name for merging
# removing columns I don't want from taxon
taxon <- remove_col(taxon, name=c("X", "USDA_code", "LTER_code", "site", "old_name", 
                                  "old_code", "X.1"))
# merging with taxon information
# left join with final_ANPP as left dataframe to keep all biomass data, but only keep taxon info for given species in biomass dataframe
final_ANPP_20_join <- left_join(final_ANPP_20, taxon, by = "species")

# setting NA's to 0 for plant comp - doing this because the species wasn't seen when % cover was taken, so its effectively 0
# not doing this until asking phoebe/mark for their opinion
#final_ANPP_20_join$cover[is.na(final_ANPP_20_join$cover)] <- 0

################################################################################
# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final_ANPP_20_join, file.path(L1_dir,"ANPP/biomass_2020_L1.csv"))


################################################################################
# 2021 biomass was collected within the treatment plots themselves

# UMBS

# Clean data
# Biomass data
View(umbs_biomass_21)
# get rid of unnecessary columns
umbs_biomass_21 <- remove_col(umbs_biomass_21, name=c("weight_g", "dried_bag_weight", "bag_size", 
                                                      "measurement_type", "date"))
str(umbs_biomass_21)

# adding quadrat number to match with plant comp data
# Kara's notes: quadrat 1 = clipped area (I think)
umbs_biomass_21$quadrat_number <- 1

colnames(umbs_biomass_21) <- sub("plant_biomass_g", "weight_g", colnames(umbs_biomass_21)) # change column name weight_g

# Plant Comp data
View(umbs_plantcomp_21)
# get ride of unwanted columns
umbs_plantcomp_21 <- remove_col(umbs_plantcomp_21, name=c("Julian", "Notes", "Date"))
str(umbs_plantcomp_21)

# Change column names to lowercase so that we can merge with the biomass file
names(umbs_plantcomp_21) <- tolower(names(umbs_plantcomp_21))

# restricting plant comp data to only quadrat 1
umbs_plantcomp_21 <- umbs_plantcomp_21[!grepl(2, umbs_plantcomp_21$quadrat_number),]

# fixing biomass species names
spp_name(umbs_biomass_21) # need to fix a few species names
site_name(umbs_biomass_21) # need to change one site name
umbs_biomass_21 <- change_spp(umbs_biomass_21)
spp_name(umbs_biomass_21) # looks good
umbs_biomass_21 <- change_site(umbs_biomass_21)
site_name(umbs_biomass_21) # looks good

# fixing plant comp species names
spp_name(umbs_plantcomp_21) # need to fix a few species names
site_name(umbs_plantcomp_21) # need to change one site name
umbs_plantcomp_21 <- change_spp(umbs_plantcomp_21)
spp_name(umbs_plantcomp_21) # looks good
umbs_plantcomp_21 <- change_site(umbs_plantcomp_21)
site_name(umbs_plantcomp_21) # looks good

#umbs_ANPP <- merge(umbs_biomass, umbs_plant_comp, by = c("plot", "species"))
umbs_ANPP_21 <- full_join(umbs_biomass_21, umbs_plantcomp_21, by = c("plot", "species", "site", "quadrat_number"))
View(umbs_ANPP_21)

umbs_ANPP_21$year <- "2021" # add year to data frame

umbs_ANPP_21 <- umbs_ANPP_21[, c("site", "year", "plot", "species", "quadrat_number", "cover", "weight_g")] # reorganize column order
View(umbs_ANPP_21)

# merging taxon info with dataframe
colnames(taxon)[which(names(taxon) == "code")] <- "species" # changing column name for merging
# removing columns I don't want from taxon
taxon <- remove_col(taxon, name=c("X", "USDA_code", "LTER_code", "site", "old_name", 
                                  "old_code", "X.1"))

# merging with taxon information
# left join with final_ANPP as left dataframe to keep all biomass data, but only keep taxon info for given species in biomass dataframe
final_umbs_anpp_21 <- left_join(umbs_ANPP_21, taxon, by = "species")

# setting NA's to 0 for plant comp - doing this because the species wasn't seen when % cover was taken, so its effectively 0
# not doing this until asking phoebe/mark for their opinion
#final_ANPP_20_join$cover[is.na(final_ANPP_20_join$cover)] <- 0

# merging with plot info
final2_umbs_anpp_21 <- left_join(final_umbs_anpp_21, meta, by = "plot")

# removing columns I don't want
final2_umbs_anpp_21 <- remove_col(final2_umbs_anpp_21, name=c("quadrat_number"))
final2_umbs_anpp_21 <- remove_col(final2_umbs_anpp_21, name=c("X.2"))

# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final2_umbs_anpp_21, file.path(L1_dir,"ANPP/umbs_biomass_2021_L1.csv"))

################################################################################
## KBS

# Clean data
# Biomass data
View(kbs_biomass_21)
# get rid of unnecessary columns
kbs_biomass_21 <- remove_col(kbs_biomass_21, name=c("Mass_g", "Dried.bag.mass..g.", "Bag.Size", "direct.or.indirect.mass", 
                                                      "Notes", "Date.Clipped","proofing.notes"))
str(kbs_biomass_21)

# adding quadrat type to match with plant comp data
kbs_biomass_21$quadrat <- "clip"
# adding site
kbs_biomass_21$site <- "kbs"

# change column names
colnames(kbs_biomass_21) <- sub("Plot.ID", "Plot", colnames(kbs_biomass_21))
colnames(kbs_biomass_21) <- sub("Species.Code", "Species", colnames(kbs_biomass_21))
colnames(kbs_biomass_21) <- sub("Dried.Plant.Biomass..g.", "weight_g", colnames(kbs_biomass_21))

# Plant Comp data
View(kbs_plantcomp_21)
colnames(kbs_plantcomp_21) <- sub("Species_Code", "Species", colnames(kbs_plantcomp_21))

# restricting plant comp data to observations from september
kbs_plantcomp_21 <- kbs_plantcomp_21[-c(161:557),]

# get ride of unwanted columns
kbs_plantcomp_21 <- remove_col(kbs_plantcomp_21, name=c("Julian", "Notes", "Date"))
str(kbs_plantcomp_21)

# Change column names to lowercase
names(kbs_plantcomp_21) <- tolower(names(kbs_plantcomp_21))
names(kbs_biomass_21) <- tolower(names(kbs_biomass_21))

# fixing biomass species names
spp_name(kbs_biomass_21) # need to fix a few species names
site_name(kbs_biomass_21) # good
kbs_biomass_21 <- change_spp(kbs_biomass_21)
spp_name(kbs_biomass_21) # looks good

# fixing plant comp species names
spp_name(kbs_plantcomp_21) # need to fix a few species names
site_name(kbs_plantcomp_21) # good
kbs_plantcomp_21 <- change_spp(kbs_plantcomp_21)
spp_name(kbs_plantcomp_21) # looks good

#kbs_ANPP <- merge(kbs_biomass, kbs_plant_comp, by = c("plot", "species"))
kbs_ANPP_21 <- full_join(kbs_biomass_21, kbs_plantcomp_21, by = c("plot", "species", "site", "quadrat"))
View(kbs_ANPP_21)

kbs_ANPP_21$year <- "2021" # add year to data frame

kbs_ANPP_21 <- kbs_ANPP_21[, c("site", "year", "plot", "species", "quadrat", "cover", "weight_g")] # reorganize column order
View(kbs_ANPP_21)

# merging taxon info with dataframe
colnames(taxon)[which(names(taxon) == "code")] <- "species" # changing column name for merging
# removing columns I don't want from taxon
taxon <- remove_col(taxon, name=c("X", "USDA_code", "LTER_code", "site", "old_name", 
                                  "old_code", "X.1", "X.2"))
# merging with taxon information
# left join with final_ANPP as left dataframe to keep all biomass data, but only keep taxon info for given species in biomass dataframe
final_kbs_anpp_21 <- left_join(kbs_ANPP_21, taxon, by = "species")

# setting NA's to 0 for plant comp - doing this because the species wasn't seen when % cover was taken, so its effectively 0
# not doing this until asking phoebe/mark for their opinion
#final_ANPP_20_join$cover[is.na(final_ANPP_20_join$cover)] <- 0

# merging with plot info
final2_kbs_anpp_21 <- left_join(final_kbs_anpp_21, meta, by = "plot")

# removing columns I don't want
final2_kbs_anpp_21 <- remove_col(final2_kbs_anpp_21, name=c("quadrat"))
################################################################################
# write a new cvs with the cleaned and merge data and upload to the shared google drive in L1
write.csv(final2_kbs_anpp_21, file.path(L1_dir,"ANPP/kbs_biomass_2021_L1.csv"))

