# TITLE:          Plant composition cleanup
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Moriah Young
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing plant comp data for all sites & years is uploaded
#                 to the L1 plant comp folder
#                 Also, a csv containing data for the date of 50% cover (greenup) is uploaded
# PROJECT:        warmXtrophic
# DATE:           December, 2020

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Source in needed functions
source("~/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R")

# Read in data (only need columns 1-7 for the umbs files)
meta <- read.csv("L0/plot.csv")
taxon <- read.csv("L0/taxon.csv")
kbs_2015 <- read.csv("L0/KBS/2015/kbs_plant_comp_2015.csv")
kbs_2016 <- read.csv("L0/KBS/2016/kbs_plant_comp_2016.csv")
kbs_2017 <- read.csv("L0/KBS/2017/kbs_plant_comp_2017.csv")
kbs_2018 <- read.csv("L0/KBS/2018/kbs_plant_comp_2018.csv")
kbs_2019 <- read.csv("L0/KBS/2019/kbs_plant_comp_2019.csv")
kbs_2020 <- read.csv("L0/KBS/2020/kbs_plant_comp_2020.csv")
umbs_2015 <- read.csv("L0/UMBS/2015/umbs_plant_comp_2015.csv")
umbs_2016 <- read.csv("L0/UMBS/2016/umbs_plant_comp_2016.csv")
umbs_2017 <- read.csv("L0/UMBS/2017/umbs_plant_comp_2017.csv")
umbs_2018 <- read.csv("L0/UMBS/2018/umbs_plant_comp_2018.csv")
umbs_2019 <- read.csv("L0/UMBS/2019/umbs_plantcomp_2019.csv")[,1:7]
umbs_2020 <- read.csv("L0/UMBS/2020/umbs_plantcomp_2020.csv")[,1:7]

# remove all empty rows for umbs_2019
umbs_2019 <- umbs_2019[-c(5047:6024), ]

# add site column for kbs & umbs 2016
kbs_2016$Site <- "kbs"
umbs_2016$Site <- "umbs"

# Change column name for umbs 2016
colnames(umbs_2016) <- sub("Percent_Cover", "Cover", colnames(umbs_2016))

# Add dataframes into a list so that needed functions can be applied 
comp_list <- list(kbs_2015=kbs_2015, kbs_2016=kbs_2016, kbs_2017=kbs_2017, kbs_2018=kbs_2018, kbs_2019=kbs_2019, kbs_2020=kbs_2020, 
                  umbs_2015=umbs_2015, umbs_2016=umbs_2016, umbs_2017=umbs_2017, umbs_2018=umbs_2018, umbs_2019=umbs_2019, umbs_2020=umbs_2020)
comp_list <- lapply(comp_list, remove_col, name=c("Julian", "Notes", "Quadrat", "Julian_Day"))
comp_list <- lapply(comp_list, change_date)
lapply(comp_list, spp_name) # need to fix a few species names
lapply(comp_list, site_name) # need to make these all the same for each site

# Fixing species names
# Ruag (Rual), Smooth_oat (Arre? Arel?), Cofo (?)
comp_list <- lapply(comp_list, change_spp)
lapply(comp_list, spp_name) # looks good

# Fixing site name
comp_list <- lapply(comp_list, change_site)
lapply(comp_list, site_name) # looks good

# Merge final data
comp_merge <- rbind(comp_list$kbs_2015, comp_list$kbs_2016, comp_list$kbs_2017, comp_list$kbs_2018, comp_list$kbs_2019, comp_list$kbs_2020, 
                    comp_list$umbs_2015, comp_list$umbs_2016, comp_list$umbs_2017, comp_list$umbs_2018, comp_list$umbs_2019, comp_list$umbs_2020)
str(comp_merge)

# Change column names to lowercase so they can be merged with metadata
names(comp_merge) <- tolower(names(comp_merge))

# Make year, month, and julian date columns
comp_merge$year <- format(comp_merge$date,format="%Y")
comp_merge$month <- format(comp_merge$date,format="%m")
comp_merge$julian <- format(comp_merge$date, "%j")

# Make julian column numeric
comp_merge$julian <- as.numeric(comp_merge$julian)

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"

# Merge meta-data with plant comp data
plant_comp_merge <- left_join(meta, comp_merge, by = "plot")
plant_comp_merge2 <- left_join(taxon, plant_comp_merge, by = "species")

# remove uneeded columns
plant_comp_merge2$species.y <- NULL
plant_comp_merge2$X <- NULL
plant_comp_merge2$site.x <- NULL
plant_comp_merge2$year.y <- NULL
plant_comp_merge2$scientific_name <- NULL
plant_comp_merge2$USDA_code <- NULL
plant_comp_merge2$LTER_code <- NULL
plant_comp_merge2$old_code <- NULL
plant_comp_merge2$old_name <- NULL
plant_comp_merge2$resolution <- NULL
plant_comp_merge2$group <- NULL
plant_comp_merge2$family <- NULL
plant_comp_merge2$common_name <- NULL

# fix column name
colnames(plant_comp_merge2)[which(names(plant_comp_merge2) == "site.y")] <- "site"

# Upload clean data csv to google drive
write.csv(plant_comp_merge2, file="L1/plant_composition/final_plantcomp_L1.csv")



### making a csv for greenup at the species-by-plot level (date at which 50% of max cover was reached per species, per plot) ###
# split the plant_comp dataframe
dataus <- split(x = plant_comp_merge2, f = plant_comp_merge2[, c("plot", "species","site", "year")])

### making a csv for greenup at the plot level (date at which 50% of max cover was reached per plot, regardless of species) ###
dataup <- split(x = plant_comp_merge2, f = plant_comp_merge2[, c("plot", "site", "year")])

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
