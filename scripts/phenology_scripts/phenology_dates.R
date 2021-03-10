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

# Read in data
meta <- read.csv("L0/plot.csv")
taxon <- read.csv("L0/taxon.csv")
plantcomp <- read.csv("L1/plant_composition/final_plantcomp_L1.csv")
str(plantcomp)

#### **** GREENUP JULIAN DATE **** ####

# Greenup can be computed in a few ways by extracting information from % cover data:

### ** Approach (1) Greenup computed over the timeframe of observations of first % cover by species, by plot. 
# This approach requires knowing the last dates of greenup phenology measurements. 

## ** Approach (2) Half Cover Date: Greenup computed as Julian days until 50% percent cover is reached by species, by plot.

# Regardless of the approach, 2015 data should be omitted from greenup because the chambers were not deployed until May in 2015.
greenup<-subset(plantcomp, year != 2015)
# Remove non-plant data
greenup<-greenup[!(greenup$species=="Bare_Ground" | 
                     greenup$species=="Unknown" | 
                     greenup$species=="Brown" | 
                     greenup$species=="Litter" | 
                     greenup$species=="Vert_Litter" | 
                     greenup$species=="Animal_Disturbance"), ]

# For Approach (1): First % Cover Date 
# KBS Greenup % cover ~ every 3 days. Duration: greenup observations ended on Julian Day:
# 2016: 104
# 2017: 121
# 2018: 130
# 2019: 
# 2020: 
# 
# UMBS Greenup % cover ~ every 3 days. Duration: greenup observations ended on Julian Day:


# Find the first Julian day of % cover per species, per plot:
greenup1 <- greenup %>% select(site,plot,species,year,julian,cover)

greenup2 <- greenup1 %>%
  group_by(site,plot,species,year) %>%
  mutate(
    firstgreen = min(julian, na.rm = T)
  ) %>%
  arrange(site,plot,year,species)

greenup2 <- greenup1 %>%
  group_by(site,plot,species,year) %>%
  summarise(firstjulian = min(julian, na.rm = T))
greenup2   

greenup2p <- greenup1 %>%
  group_by(site,plot,year) %>%
  summarise(firstjulian = min(julian, na.rm = T))
greenup2p   

# Max cover by species-plot
greenup3 <- greenup1 %>%
  group_by(site,plot,species,year,julian) %>%
  summarise(maxcov = max(cover, na.rm = T)) 
greenup3   

# save these as dataframes
# species-plot level 
greendate1st_mn_s<-as.data.frame(greenup2)
greendate1st_mn_p<-as.data.frame(greenup2p)
# stopping here for now with Approach (1)...

# Greenup Approach (2) Half Cover Date
### making a csv for greenup at the species-by-plot level ###
# split the plant_comp dataframe
dataus <- split(x = greenup, f = greenup[, c("plot","species","site","year")])

### making a csv for greenup at the plot level ###
dataup <- split(x = greenup, f = greenup[, c("plot", "site", "year")])

# Determine dates for each plot-species combination where the value of `cover` is the max value
max_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){
  x[which.max(x[["cover"]]), "julian"]
}))
# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){
  x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]
}))
# Determine dates for each plot where the value of `cover` is at the max value
max_cover_datep <- unlist(lapply(X = dataup, FUN = function(x){
  x[which.max(x[["cover"]]), "julian"]
}))
# Determine dates for each plot where the value of `cover` is at least half the max value
half_cover_datep <- unlist(lapply(X = dataup, FUN = function(x){
  x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]
}))

# make each into a dataframe
max_cover_dates_df <- data.frame("plot.species.site.year" = names(half_cover_dates),
                                  "spp_max_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

half_cover_dates_df <- data.frame("plot.species.site.year" = names(half_cover_dates),
                                  "spp_half_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

max_cover_datep_df <- data.frame("plot.site.year" = names(half_cover_datep),
                                 "plot_max_cover_date" = unname(half_cover_datep), stringsAsFactors = FALSE)

half_cover_datep_df <- data.frame("plot.site.year" = names(half_cover_datep),
                                  "plot_half_cover_date" = unname(half_cover_datep), stringsAsFactors = FALSE)

# fix species and plot columns
half_cover_dates_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dates_df[["species"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_dates_df[["site"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_dates_df[["year"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
half_cover_dates_df$plot.species.site.year <- NULL

max_cover_dates_df[["plot"]] <- sapply(X = strsplit(x = max_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
max_cover_dates_df[["species"]] <- sapply(X = strsplit(x =max_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
max_cover_dates_df[["site"]] <- sapply(X = strsplit(x =max_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
max_cover_dates_df[["year"]] <- sapply(X = strsplit(x =max_cover_dates_df[["plot.species.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
max_cover_dates_df$plot.species.site.year <- NULL

# fix plot column
half_cover_datep_df[["plot"]] <- sapply(X = strsplit(x = half_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_datep_df[["site"]] <- sapply(X = strsplit(x =half_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_datep_df[["year"]] <- sapply(X = strsplit(x =half_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_datep_df$plot.site.year <- NULL

max_cover_datep_df[["plot"]] <- sapply(X = strsplit(x = max_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
max_cover_datep_df[["site"]] <- sapply(X = strsplit(x =max_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
max_cover_datep_df[["year"]] <- sapply(X = strsplit(x =max_cover_datep_df[["plot.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
max_cover_datep_df$plot.site.year <- NULL

# is this value correlated with first date of greenup per site, plot, year?
# determine first date of emergence for correlation with 'green-up' index
min_dates <- aggregate(greenup$julian,by=greenup[,c("site","plot","species","year")],FUN=min)
head(min_dates)
colnames(min_dates) <- c("site","plot","species","year","min_green_date")
# note that this is the same as "greenup2" above, which was created with tidy
head(min_dates)
greenup2

# plot-level (drop species)
min_datep <- aggregate(greenup$julian,by=greenup[,c("site","plot","year")],FUN=min)
head(min_datep)
colnames(min_datep) <- c("site","plot","year","min_green_date")

# merge min date dateframe with "half cover date" df
green_half_mins <- merge(half_cover_dates_df, min_dates, by=c("site","plot","species","year"))
green_half_minp <- merge(half_cover_datep_df, min_datep, by=c("site","plot","year"))

# calculate correlation
cor.test(green_half_mins$min_green_date, green_half_mins$spp_half_cover_date) 
# yes cor = 0.6944626; t = 47.607, df = 2433, p-value < 2.2e-16
plot(green_half_mins$min_green_date, green_half_mins$spp_half_cover_date)

# calculate correlation
cor.test(green_half_minp$min_green_date, green_half_minp$plot_half_cover_date) 
# no cor = -0.08616082; t = -1.3342, df = 238, p-value = 0.1834
plot(green_half_minp$min_green_date, green_half_minp$plot_half_cover_date)

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"
# taxon contains "site" which is the site where the species is found on our meta-data table, but those data exist in our plant_comp_merge dataset already. Delete "site" from taxon so it doesn't accidentally get merged in.
taxon$site<-NULL

# re-merge data with meta data info for species-level 
finalgreens <- left_join(meta, green_half_mins, by = "plot","site")
finalgreens <- left_join(taxon, finalgreens, by = "species")
# re-merge data with meta data info for plot-level 
finalgreenp <- left_join(meta, green_half_minp, by = c("plot"))

# remove unnecessary columns
finalgreens$old_code <- NULL
finalgreens$old_name <- NULL
finalgreens$resolution <- NULL

# remove NA values for species in the taxon table that do not exist in these data
finalgreens<-finalgreens[complete.cases(finalgreens), ]
finalgreenp<-finalgreenp[complete.cases(finalgreenp), ]

# upload greenup species-plot level csv to google drive
write.csv(finalgreens, file="L1/greenup/final_greenup_species_L1.csv", row.names=FALSE)
write.csv(finalgreenp, file="L1/greenup/final_greenup_plot_L1.csv", row.names=FALSE)
