# TITLE:          warmXtrophic Phenology Dates
# AUTHORS:        Kara Dobson, Moriah Young, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    The following CSV files are uploaded to the phenology L1 folder:
#                 "final_greenup_L1.csv" = the date of 50% cover for greenup 
#                 "???.csv" for flowering and seeding first dates
# PROJECT:        warmXtrophic
# DATE:           March 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Source in needed functions from the github repo
source("~/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R")
#source("~/DATA/git/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R") # PLZ's location
#source("~/Documents/GitHub/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R") # PLZ's location

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

#### **** GREENUP HALF COVER DATE **** ####
# Read in composition data 
plantcomp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
str(plantcomp)

# Greenup is extracted from % cover data, but not all season. What are the last dates of "greenup"?
# Greenup should be computed from a dataset that includes the first % cover observation, by species, by plot.

# ## KBS Greenup % cover ~ every 3 days ended on Julian Day:
# # 2016: 104
# # 2017: 121
# # 2018: 130
# # 2019: 
# # 2020: 
# 
# unique(greenup17$species)
# # simplify greenup17 to just a few columns for easier manipulating
# vars <- names(greenup17) %in% c("plot", "species", "year", "date", "julian", "cover")
# greenup17 <- greenup17[vars]
# # look at the last few dates and the species list - we need to find the date when all plots had all of their species
# 
# # get the species list for 2017
# greenup17 %>% group_by(plot,species)

# 2015 data should be omitted from greenup because the chambers were not deployed until May.
greenup<-subset(plantcomp, year != 2015)
greenup1 <- greenup %>% select(site,plot,species,year,julian)

greenup2 <- greenup1 %>%
  group_by(site,plot,species,year) %>%
  mutate(
    firstgreen = min(julian, na.rm = T)
  ) %>%
  arrange(site,plot,year,species)

# Compute mean first greenup day by species, across

### making a csv for greenup at the species-by-plot level ###
# split the plant_comp dataframe
dataus <- split(x = plantcomp, f = plantcomp[, c("plot","species","site","year")])

### making a csv for greenup at the plot level ###
dataup <- split(x = plantcomp, f = plantcomp[, c("plot", "site", "year")])

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){
  x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]
}))
# Determine dates for each plot where the value of `cover` is at least half the max value
half_cover_datep <- unlist(lapply(X = dataup, FUN = function(x){
  x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]
}))


# make each into a dataframe
half_cover_dates_df <- data.frame("plot.species.site.year" = names(half_cover_dates),
                                  "spp_half_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

half_cover_datep_df <- data.frame("plot.site.year" = names(half_cover_datep),
                                  "plot_half_cover_date" = unname(half_cover_datep), stringsAsFactors = FALSE)

# fix species and plot columns
half_cover_dates_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dates_df[["species"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_dates_df[["site"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_dates_df[["year"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
half_cover_dates_df$plot.species.site.year <- NULL

# fix plot column
half_cover_datep_df[["plot"]] <- sapply(X = strsplit(x = half_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_datep_df[["site"]] <- sapply(X = strsplit(x =half_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_datep_df[["year"]] <- sapply(X = strsplit(x =half_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_datep_df$plot.site.year <- NULL


# is this value correlated with first date of greenup?
# determine first date of emergence for correlation with 'green-up' index
min_date <- aggregate(plant_comp_merge$julian,by=plant_comp_merge[,c("plot","species")],FUN=min)
colnames(min_date) <- c("plot", "species", "min_emerg_date")

# merge min date dateframe with "green-up index" df
combined <- merge(half_cover_dates_df, min_date, by=c("plot", "species"))

# calculate correlation
cor.test(combined$min_emerg_date, combined$half_cover_date) # yes

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"


# re-merge data with meta data info
final <- left_join(meta, half_cover_dates_df, by = "plot")
final <- left_join(taxon, final, by = "species")
final <- left_join(half_cover_datep_df, final, by = c("plot", "year"))

# remove uneeded columns
final$cover <- NULL
final$julian <- NULL
final$X <- NULL
final$site.x <- NULL
final$year.x <- NULL
final$site <-  NULL
final$scientific_name <- NULL
final$USDA_code <- NULL
final$LTER_code <- NULL
final$old_code <- NULL
final$old_name <- NULL
final$resolution <- NULL
final$group <- NULL
final$family <- NULL
final$common_name <- NULL

# fix column names
colnames(final)[which(names(final) == "site.y")] <- "site"
colnames(final)[which(names(final) == "year.y")] <- "year"

# upload greenup csv to google drive
write.csv(final, file="L1/greenup/final_greenup_L1.csv")
