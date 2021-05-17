# TITLE:          Herbivory cleanup
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           May 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Source in needed functions
source("~/warmXtrophic/R/L1/herbivory_functions_L1.R")

# Get data
Sys.getenv("L0DIR")
L0_dir<-Sys.getenv("L0DIR")
list.files(L0_dir)
meta <- read.csv(file.path(L0_dir, "plot.csv"))
taxon <- read.csv(file.path(L0_dir, "taxon.csv"))
kbs_2015 <- read.csv(file.path(L0_dir, "KBS/2015/kbs_leaf_herbivory_2015.csv"))
kbs_2016 <- read.csv(file.path(L0_dir, "KBS/2016/kbs_leaf_herbivory_2016.csv"))
kbs_2017 <- read.csv(file.path(L0_dir, "KBS/2017/kbs_leaf_herbivory_2017.csv"))
kbs_2018 <- read.csv(file.path(L0_dir, "KBS/2018/kbs_leaf_herbivory_2018.csv"))
kbs_2019 <- read.csv(file.path(L0_dir, "KBS/2019/kbs_leaf_herbivory_2019.csv"))
kbs_2020 <- read.csv(file.path(L0_dir, "KBS/2020/kbs_herbivory_2020.csv"))
umbs_2015 <- read.csv(file.path(L0_dir, "UMBS/2015/umbs_herbivory_2015.csv"))
umbs_2016 <- read.csv(file.path(L0_dir, "UMBS/2016/umbs_leaf_herbivory_2016.csv"))
umbs_2017 <- read.csv(file.path(L0_dir, "UMBS/2017/umbs_leaf_herbivory_2017.csv"))
umbs_2018 <- read.csv(file.path(L0_dir, "UMBS/2018/umbs_leaf_herbivory_2018.csv"))
umbs_2019 <- read.csv(file.path(L0_dir, "UMBS/2019/umbs_herbivory_2019.csv"))
umbs_2020 <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_herbivory_2020.csv"))

# add site column for kbs & umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# made a plant number column for 2015 - each plot only had one plant measured as opposed to later years where plots had multiple
# plants measured. Technically the plant number for each plot could be 1, but it doesn't matter in the end
kbs_2015$plant_number <- sapply(strsplit(as.character(kbs_2015$plot), ""), "[", 2)
umbs_2015$plant_number <- sapply(strsplit(as.character(umbs_2015$plot), ""), "[", 2)

# remove "total" column for 2015 data
kbs_2015 <- subset (kbs_2015, select = -total)
umbs_2015 <- subset (umbs_2015, select = -total)

# Put dataframes into a list so that functions can be applied
herb_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)

# Fix ID column elements in 2015 data
herb_list[1] <- lapply(herb_list[1], transform, 
                                   ID = format(as.Date(sub(".*_(\\d{4}_\\d{2}_\\d{2})_.*",
                                                           "\\1", ID), "%Y_%m_%d"), "%m/%d/%Y"))
herb_list[7] <- lapply(herb_list[7], transform, 
                       ID = format(as.Date(sub(".*_(\\d{4}_\\d{2}_\\d{2})_.*",
                                               "\\1", ID), "%Y_%m_%d"), "%m/%d/%Y"))

# Apply other cleaning functions
herb_list <- lapply(herb_list, lowercase)
herb_list[1] <- lapply(herb_list[1], change_col_names)
herb_list[7] <- lapply(herb_list[7], change_col_names)
herb_list <- lapply(herb_list, change_date)
# only apply this function to non-2015 data because it removes all data from those files (??)
herb_list[2:6] <- lapply(herb_list[2:6], remove_col, name=c("julian", "notes"))
herb_list[8:12] <- lapply(herb_list[8:12], remove_col, name=c("julian", "notes"))

# check for misspellings
# ones im not sure of: Sora (kbs2017), no other misspellings
lapply(herb_list, spp_name)
lapply(herb_list, site_name)

# fix misspellings for site and species names
herb_list <- lapply(herb_list, change_site)
herb_list <- lapply(herb_list, change_spp)

# remove species
herb_list <- lapply(herb_list, remove_spp)
lapply(herb_list, spp_name)

# Merge final data
herb_merge <- rbind(herb_list$kbs_2015, herb_list$kbs_2016, herb_list$kbs_2017, herb_list$kbs_2018, herb_list$kbs_2019, herb_list$kbs_2020, 
                    herb_list$umbs_2015, herb_list$umbs_2016, herb_list$umbs_2017, herb_list$umbs_2018, herb_list$umbs_2019, herb_list$umbs_2020)
str(herb_merge)

# Add year column
herb_merge$year <- format(herb_merge$date,format="%Y")

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"

# Merge metadata with data
herb <- left_join(meta, herb_merge, by = "plot")
herb2 <- left_join(taxon, herb, by = "species")

# remove uneeded columns
herb2$site.x <- NULL
herb2$scientific_name <- NULL
herb2$USDA_code <- NULL
herb2$LTER_code <- NULL
herb2$old_code <- NULL
herb2$old_name <- NULL
herb2$resolution <- NULL
herb2$group <- NULL
herb2$family <- NULL
herb2$common_name <- NULL

# fix column name
colnames(herb2)[which(names(herb2) == "site.y")] <- "site"

# Upload clean csv to google drive
write.csv(herb2, file="L1/herbivory/final_herbivory_L1.csv")
