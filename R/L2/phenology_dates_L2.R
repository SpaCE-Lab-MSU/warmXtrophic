# TITLE:          warmXtrophic Phenology Dates
# AUTHORS:        Kara Dobson, Moriah Young, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Pat Bills
# DATA INPUT:     Data imported as csv files from shared Google drive L1 folder
# DATA OUTPUT:    The following CSV files are uploaded to the phenology L2 folder:
#                 "final_greenup_species_L2.csv" = the date of 50% cover for greenup by species, 
#                 "final_greenup_plot_L2.csv" = the date of 50% cover for greenup by plot,
#                 "final_greenup_growthhabit_L2.csv" = the date of 50% cover for greenup by plot and growth habit,
#                 "final_greenup_origin_L2.csv" = the date of 50% cover for greenup by plot and origin,
#                 "final_flwr_species_L2.csv" = minimum, median, and duration of Julian date of flowering by species,
#                 "final_flwr_plot_L2.csv" = minimum, median, and duration of Julian date of flowering by plot,
#                 "final_sd_species_L2.csv" = minimum Julian date of seed set by species,
#                 "final_sd_plot_L2.csv" = minimum Julian date of seed set by plot
# PROJECT:        warmXtrophic
# DATE:           March 2021; modified July 9, 2021; modified Oct. 5 2021; modified Oct. 19th 2021
# NOTE:           We decided to add in L2 portion of "phenology_clean_L1.R" into this script
#                 to organize by metric: (1) Julian Date Calculation, (2) Duration of days of each
#                 event, (3) Julian Date of 50% (cover for greenup, obs for flower & seed)

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Source in needed functions from the github repo - could add this to Renviron?
source("/Users/moriahyoung/Documents/GitHub/warmXtrophic/R/L1/plant_comp_functions_L1.R") # Moriah's location
#source("~/warmXtrophic/R/L1/plant_comp_functions_L1.R") # Kara's location
#source("~/DATA/git/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R") # PLZ's location
#source("~/Documents/GitHub/warmXtrophic/scripts/plant_comp_scripts/plant_comp_functions.R") # PLZ's location

# Set working directory
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
L2_dir <- Sys.getenv("L2DIR")

# Read in data
# read in meta data for plots
plot_info <- read.csv(file.path(L0_dir, "plot.csv"))

# read in meta taxon list
taxon <- read.csv(file.path(L0_dir, "taxon.csv"))
taxon$X <- NULL # get rid of "X" column that shows up
taxon$X.1 <- NULL # get rid of "X.1" column that shows up
# change column name for from "code" to "Species" to match cleaned plant comp data
colnames(taxon) <- sub("code", "species", colnames(taxon))

# read in plant composition data
plantcomp <- read.csv(file.path(L1_dir,"/plant_composition/final_plantcomp_L1.csv"))
plantcomp <- plantcomp %>% select(-X) # get rid of "X" column that shows up
str(plantcomp)

# read in flower and seed set data
flwr_sd <- read.csv(file.path(L1_dir, "phenology/final_flwr_sd_L1.csv"))
flwr_sd <- flwr_sd %>% select(-X) # get rid of "X" column that shows up
str(flwr_sd)

#### **** JULIAN DATE **** #### 
# Computing various Julian dates of importance by species and plot
# For greenup: Julian date of first % cover and Julian days until 50% percent cover is reached by species and by 
# by plot
# For flowering: first Julian date of flower, median Julian date of flower, and duration of flowering
# For seed set: first Julian date of seed set, median Julian date of seed set, and duration (?) of seed set

# GREENUP
# Greenup can be computed in a few ways by extracting information from % cover data:

### ** Approach (1) Greenup computed over the timeframe of observations of first % cover by species, by plot. 
# This approach requires knowing the last dates of greenup phenology measurements. 

## ** Approach (2) Half Cover Date: Greenup computed as Julian days until 50% percent cover is reached by species, 
# by plot.

# Regardless of the approach, 2015 data should be omitted from greenup because the chambers were not deployed until 
# May in 2015.
greenup <- subset(plantcomp, year != 2015)
# Remove non-plant data
greenup <- greenup[!(greenup$species=="Bare_Ground" | 
                     greenup$species=="Unknown" | 
                     greenup$species=="Brown" | 
                     greenup$species=="Litter" | 
                     greenup$species=="Vert_Litter" | 
                     greenup$species=="Animal_Disturbance"), ]
sort(unique(greenup$species))



# Note: we went with approach 2, below
###### For Approach (1): Greenup Window: % Cover Dates ######
# KBS Greenup % cover ~ every 3 days. Duration: greenup observations ended on Julian Day:
# 2016: 104
# 2017: 121
# 2018: 130
# 2019: 143
# 2020: 119 - few measurements
# 2021: 103 - very few measurements
# 
# UMBS Greenup % cover ~ every 3 days. Duration: greenup observations ended on Julian Day:
# 2016: 
# 2017: 
# 2018: 
# 2019: 
# 2020: 

# Find the first Julian day of % cover per species, per plot:
greenup1 <- greenup %>% 
  select(site, plot, species, year, julian, cover)

# Re-arrange the data to simplify
greenup1a <- greenup1 %>%
  group_by(site,plot,species,year) %>%
  mutate(
    firstjulian = min(julian, na.rm = T)
  ) %>%
  arrange(site,plot,year,species)

# A quicker way to summarize: For each site, plot, species, year, extract just the firstjulian
greenup2 <- greenup1 %>%
  group_by(site,plot,species,year) %>%
  summarise(firstjulian = min(julian, na.rm = T))
View(greenup2)

greenup2kbs <- greenup2 %>%
  filter(site == "kbs")
greenup2umbs <- greenup2 %>%
  filter(site == "umbs")

# By plot, first julian date by each year
greenup2p <- greenup1 %>%
  group_by(site,plot,year) %>%
  summarise(firstjulian = min(julian, na.rm = T))
View(greenup2p)  

# Take a look at these data for each site separately
k <- ggplot(greenup2kbs, aes(year, firstjulian, colour = species)) + geom_point() + 
  labs(title = "KBS first julian day of % cover by species") + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE))
k + facet_wrap(vars(species)) + theme(axis.text.x = element_text(angle = 90), legend.position="bottom") 

u <- ggplot(greenup2umbs, aes(year, firstjulian, colour = species)) + geom_point() + 
  labs(title = "UMBS first julian day of % cover by species") + 
  guides(fill=guide_legend(nrow=3, byrow=TRUE))
u + facet_wrap(vars(species)) + theme(axis.text.x = element_text(angle = 90), legend.position="bottom") 
#ggsave(file="./L1/greenup/UMBS_firstjulianday_by_species.png", width = 8.5, height = 11) #doesn't work for Moriah

# What are the outliers in terms of firstjulian? 
# There are several singletons (species that are only observed once or twice), some that may be mis-IDed, and some that are only first noticed in July or August, which is hard to believe. For now they should be removed but the cleaning on this should take place in plant_comp_clean_L0.R.
# Need to make some decisions about which of these species to include or re-assign to a different species. All of that needs to be done in plant_comp_clean_L0.R

## PLZ Stopped updating here 10:20am March 10, 2021

# By species, find the max of the firstjulian at the site level to determine the last day of the greenup window per site and year
greenup3 <- greenup2 %>%
  group_by(site, year) %>%
  summarise(firstjulian = max(firstjulian, na.rm = T))
View(greenup3)   

# Max cover by species-plot
greenup4 <- greenup1 %>%
  group_by(site, plot, species, year, julian) %>%
  summarise(maxcov = max(cover, na.rm = T)) 
View(greenup4)

# save these as dataframes
# species-plot level 
greendate1st_mn_s <- as.data.frame(greenup2) # species level
greendate1st_mn_p <- as.data.frame(greenup2p) # plot level
# stopping here for now with Approach (1)...




###### Greenup Approach (2) Half Cover Date ######
### making a csv for greenup at the species-by-plot level ###
# split the plant_comp dataframe at the species level and at the plot level
dataus <- split(x = greenup, f = greenup[, c("plot","species","site","year")])
dataus <- dataus[sapply(dataus, nrow)>0] # removing the dataframes that contain no data

### making a csv for greenup at the plot level ###
dataup <- split(x = greenup, f = greenup[, c("plot", "site", "year")])
dataup <- dataup[sapply(dataup, nrow)>0]

### making a csv for greenup at the growth form level ###
dataug <- split(x = greenup, f = greenup[, c("plot","growth_habit","site","year")])
dataug <- dataug[sapply(dataug, nrow)>0]

### making a csv for greenup at the origin level ###
datauo <- split(x = greenup, f = greenup[, c("plot","origin","site","year")])
datauo <- datauo[sapply(datauo, nrow)>0]


# SPECIES LEVEL
# Determine dates for each plot-species combination where the value of `cover` is the max value
max_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){
  x[which.max(x[["cover"]]), "julian"]
}))

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){
  x[which.max(x[["cover"]] >= which.max(x[["cover"]])/2), "julian"]
}))

# PLOT LEVEL
# Determine dates for each plot where the value of `cover` is at the max value
max_cover_datep <- unlist(lapply(X = dataup, FUN = function(x){
  x[which.max(x[["cover"]]), "julian"]
}))

# Determine dates for each plot where the value of `cover` is at least half the max value
half_cover_datep <- unlist(lapply(X = dataup, FUN = function(x){
  x[which.max(x[["cover"]] >= which.max(x[["cover"]])/2), "julian"]
}))

# GROWTH HABIT LEVEL
# Determine dates for each plot-species combination where the value of `cover` is the max value
max_cover_dateg <- unlist(lapply(X = dataug, FUN = function(x){
        x[which.max(x[["cover"]]), "julian"]
}))

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dateg <- unlist(lapply(X = dataug, FUN = function(x){
        x[which.max(x[["cover"]] >= which.max(x[["cover"]])/2), "julian"]
}))

# ORIGIN LEVEL
# Determine dates for each plot-species combination where the value of `cover` is the max value
max_cover_dateo <- unlist(lapply(X = datauo, FUN = function(x){
        x[which.max(x[["cover"]]), "julian"]
}))

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dateo <- unlist(lapply(X = datauo, FUN = function(x){
        x[which.max(x[["cover"]] >= which.max(x[["cover"]])/2), "julian"]
}))


# make each into a dataframe
max_cover_dates_df <- data.frame("plot.species.site.year" = names(max_cover_dates),
                                  "spp_max_cover_date" = unname(max_cover_dates), stringsAsFactors = FALSE)

half_cover_dates_df <- data.frame("plot.species.site.year" = names(half_cover_dates),
                                  "spp_half_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

max_cover_datep_df <- data.frame("plot.site.year" = names(max_cover_datep),
                                 "plot_max_cover_date" = unname(max_cover_datep), stringsAsFactors = FALSE)

half_cover_datep_df <- data.frame("plot.site.year" = names(half_cover_datep),
                                  "plot_half_cover_date" = unname(half_cover_datep), stringsAsFactors = FALSE)

max_cover_dateg_df <- data.frame("plot.growth_habit.site.year" = names(max_cover_dateg),
                                 "habit_max_cover_date" = unname(max_cover_dateg), stringsAsFactors = FALSE)

half_cover_dateg_df <- data.frame("plot.growth_habit.site.year" = names(half_cover_dateg),
                                  "habit_half_cover_date" = unname(half_cover_dateg), stringsAsFactors = FALSE)

max_cover_dateo_df <- data.frame("plot.origin.site.year" = names(max_cover_dateo),
                                 "origin_max_cover_date" = unname(max_cover_dateo), stringsAsFactors = FALSE)

half_cover_dateo_df <- data.frame("plot.origin.site.year" = names(half_cover_dateo),
                                  "origin_half_cover_date" = unname(half_cover_dateo), stringsAsFactors = FALSE)


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

# fix growth habit and plot columns
half_cover_dateg_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dateg_df[["growth_habit"]] <- sapply(X = strsplit(x =half_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_dateg_df[["site"]] <- sapply(X = strsplit(x =half_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_dateg_df[["year"]] <- sapply(X = strsplit(x =half_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
half_cover_dateg_df$plot.growth_habit.site.year <- NULL

max_cover_dateg_df[["plot"]] <- sapply(X = strsplit(x = max_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
max_cover_dateg_df[["growth_habit"]] <- sapply(X = strsplit(x =max_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
max_cover_dateg_df[["site"]] <- sapply(X = strsplit(x =max_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
max_cover_dateg_df[["year"]] <- sapply(X = strsplit(x =max_cover_dateg_df[["plot.growth_habit.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
max_cover_dateg_df$plot.growth_habit.site.year <- NULL

# fix origin and plot columns
half_cover_dateo_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dateo_df[["origin"]] <- sapply(X = strsplit(x =half_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
half_cover_dateo_df[["site"]] <- sapply(X = strsplit(x =half_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
half_cover_dateo_df[["year"]] <- sapply(X = strsplit(x =half_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
half_cover_dateo_df$plot.origin.site.year <- NULL

max_cover_dateo_df[["plot"]] <- sapply(X = strsplit(x = max_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
max_cover_dateo_df[["origin"]] <- sapply(X = strsplit(x =max_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 2L)
max_cover_dateo_df[["site"]] <- sapply(X = strsplit(x =max_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 3L)
max_cover_dateo_df[["year"]] <- sapply(X = strsplit(x =max_cover_dateo_df[["plot.origin.site.year"]], split = ".", fixed = TRUE), FUN = `[`, 4L)
max_cover_dateo_df$plot.origin.site.year <- NULL


# Is this value correlated with first date of greenup per site, plot, year?
# determine first date of emergence for correlation with 'green-up' index
min_dates <- aggregate(greenup$julian, by=greenup[ ,c("site","plot","species","year")], FUN=min)
head(min_dates)
colnames(min_dates) <- c("site","plot","species","year","min_green_date")
# note that this is the same as "greenup2" above, which was created with tidy
head(min_dates)
#greenup2

# plot-level (drop species)
min_datep <- aggregate(greenup$julian,by=greenup[,c("site","plot","year")],FUN=min)
head(min_datep)
colnames(min_datep) <- c("site","plot","year","min_green_date")
head(min_datep)
#names(min_datep)[4] <- "firstjulian" # renaming column X to firstjulian

# growth_habit
min_dateg <- aggregate(greenup$julian, by=greenup[ ,c("site","plot","growth_habit","year")], FUN=min)
head(min_dateg)
colnames(min_dateg) <- c("site","plot","growth_habit","year","min_green_date")
head(min_dateg)

# origin
min_dateo <- aggregate(greenup$julian, by=greenup[ ,c("site","plot","origin","year")], FUN=min)
head(min_dateo)
colnames(min_dateo) <- c("site","plot","origin","year","min_green_date")
head(min_dateo)


# merge min date dateframe with "half cover date" df
green_half_mins <- merge(half_cover_dates_df, min_dates, by=c("site","plot","species","year"))
green_half_minp <- merge(half_cover_datep_df, min_datep, by=c("site","plot","year"))
green_half_ming <- merge(half_cover_dateg_df, min_dateg, by=c("site","plot","growth_habit","year"))
green_half_mino <- merge(half_cover_dateo_df, min_dateo, by=c("site","plot","origin", "year"))

# calculate correlation
cor.test(green_half_mins$min_green_date, green_half_mins$spp_half_cover_date) 
# yes cor = 0.8065159; t = 66.914, df = 2406, p-value < 2.2e-16
plot(green_half_mins$min_green_date, green_half_mins$spp_half_cover_date)

# calculate correlation
cor.test(green_half_minp$min_green_date, green_half_minp$plot_half_cover_date)
# yes cor = 0.1597517; t = 2.732, df = 285, p-value = 0.006688 -- Moriah got different numbers 7/12/21
plot(green_half_minp$min_green_date, green_half_minp$plot_half_cover_date)

# change taxon column name for merging
colnames(taxon)[which(names(taxon) == "code")] <- "species"
# taxon contains "site" which is the site where the species is found on our meta-data table, 
# but those data exist in our plant_comp_merge dataset already. Delete "site" from taxon so it doesn't 
# accidentally get merged in.
taxon$site<-NULL

# re-merge data with meta data info for species-level 
finalgreens <- left_join(plot_info, green_half_mins, by = "plot","site")
finalgreens <- left_join(taxon, finalgreens, by = "species")
# re-merge data with meta data info for plot-level 
finalgreenp <- left_join(plot_info, green_half_minp, by = c("plot"))
finalgreeng <- left_join(plot_info, green_half_ming, by = c("plot"))
finalgreeno <- left_join(plot_info, green_half_mino, by = c("plot"))

# remove unnecessary columns
finalgreens$old_code <- NULL
finalgreens$old_name <- NULL
finalgreens$old_species <- NULL
finalgreens$resolution <- NULL

# remove NA values for species in the taxon table that do not exist in these data
finalgreens<-finalgreens[complete.cases(finalgreens), ]
finalgreenp<-finalgreenp[complete.cases(finalgreenp), ]
finalgreeng<-finalgreeng[complete.cases(finalgreeng), ]
finalgreeno<-finalgreeno[complete.cases(finalgreeno), ]
# removing empty growth habit and origin rows
finalgreeng <- with(finalgreeng, finalgreeng[!(growth_habit == "" | is.na(growth_habit)), ])
finalgreeno <- with(finalgreeno, finalgreeno[!(origin == "" | is.na(origin)), ])

# Create some plots to visualize these data
# histograms for each year - look at them together:
p1 <- ggplot(data = finalgreenp, aes(x = plot_half_cover_date, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p1 + facet_wrap(~year) + labs(title="Plot-level half cover date")

p1 <- ggplot(data = finalgreens, aes(x = spp_half_cover_date, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p1 + facet_wrap(~year) + labs(title="Species-level half cover date")

# this will just show sampling date artifact
p2 <- ggplot(data = finalgreenp, aes(x = min_green_date, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p2 + facet_wrap(~year)

p2 <- ggplot(data = finalgreens, aes(x = min_green_date, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p2 + facet_wrap(~year) + labs(title="Species-level min cover date")

# Density plot
p3 <- ggplot(data = finalgreenp, aes(x = plot_half_cover_date, fill=state)) + geom_density(alpha=0.5)
p3 + facet_wrap(~year) + labs(title="Plot-level half cover date")

p3 <- ggplot(data = finalgreens, aes(x = spp_half_cover_date, fill=state)) + geom_density(alpha=0.5)
p3 + facet_wrap(~year) + labs(title="Species-level half cover date")

# this will just show sampling date artifact
p4 <- ggplot(data = finalgreenp, aes(x = min_green_date, fill=state)) + geom_density(alpha=0.5)
p4 + facet_wrap(~year)

# reorganize order of column names 
finalgreens <- finalgreens[, c("site", "plot", "year", "species", "spp_half_cover_date", "min_green_date",
                               "treatment_key", "state", "insecticide", "scientific_name", "common_name", 
                               "USDA_species", "LTER_species", "origin", "group", "family", "duration", 
                               "growth_habit")]
finalgreenp <- finalgreenp[, c("site", "plot", "year", "treatment_key", "state", "insecticide", 
                               "plot_half_cover_date", "min_green_date" )]
finalgreeng <- finalgreeng[, c("site", "plot", "year", "growth_habit", "treatment_key", "state", "insecticide", 
                               "habit_half_cover_date", "min_green_date" )]
finalgreeno <- finalgreeno[, c("site", "plot", "year", "origin", "treatment_key", "state", "insecticide", 
                               "origin_half_cover_date", "min_green_date" )]

# upload greenup species-plot level csv to google drive
write.csv(finalgreens, file.path(L2_dir, "greenup/final_greenup_species_L2.csv"))
write.csv(finalgreenp, file.path(L2_dir, "greenup/final_greenup_plot_L2.csv"))
write.csv(finalgreeng, file.path(L2_dir, "greenup/final_greenup_growthhabit_L2.csv"))
write.csv(finalgreeno, file.path(L2_dir, "greenup/final_greenup_origin_L2.csv"))


## Finding species whose mean half cover was within the window of green-up observations each year
# First, finding the mean half cover date per species
half_cover_mean <- half_cover_dates_df %>%
        group_by(species, site, year) %>%
        mutate(half_cover_mean = mean(spp_half_cover_date))

# making a new column that contains the date green-up observations ended
# (found the end date of greenup by visually inspecting the dataframes for the last data of record)
# these are the same dates listed under approach one
half_cover_mean$end_greenup <- NA
half_cover_mean$end_greenup[half_cover_mean$year == 2016 & half_cover_mean$site == "kbs"] = 104
half_cover_mean$end_greenup[half_cover_mean$year == 2017 & half_cover_mean$site == "kbs"] = 121
half_cover_mean$end_greenup[half_cover_mean$year == 2018 & half_cover_mean$site == "kbs"] = 130
half_cover_mean$end_greenup[half_cover_mean$year == 2019 & half_cover_mean$site == "kbs"] = 143
half_cover_mean$end_greenup[half_cover_mean$year == 2020 & half_cover_mean$site == "kbs"] = 119
half_cover_mean$end_greenup[half_cover_mean$year == 2021 & half_cover_mean$site == "kbs"] = 103

# testing just with KBS at first
half_cover_mean_kbs <- subset(half_cover_mean, site == "kbs")

# find species whose half cover mean is less than or equal to the date greenup observations ended
half_cover_kbs <- half_cover_mean_kbs %>%
        filter(half_cover_mean <= end_greenup)
half_cover_kbs <- unique(half_cover_kbs[c("species", "year")])
# this dataframe shows species who reached their half cover dates within the greenup window
# can pick these out and analyze these separately


###### FLOWERING (Moriah did this) ######

# Create separate data frames for flowering and seeding
phen_flwr <- subset(flwr_sd, action == "flower")
phen_sd <- subset(flwr_sd, action == "seed")

### Create a data frame at the SPECIES LEVEL that includes median date of flower, first flower date, and duration

# First Flower by SPECIES LEVEL - filter data to contain the date of first flower for each species at each plot
FirstFlwr_spp <- phen_flwr %>%
  group_by(plot, year, species, state, site, action, origin, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarize(julian_min = min(julian, na.rm=T))

# Kara - looking at species w/ earliest and latest flowering time
# this dataframe was only used to determine these species
phen_flwr_control_kbs <- subset(phen_flwr, state == "ambient" & site == "kbs")
phen_flwr_control_umbs <- subset(phen_flwr, state == "ambient" & site == "umbs")
early_late_kbs <- phen_flwr_control_kbs %>%
        add_count(species) %>%
        group_by(species, n) %>%
        summarize(julian_min = min(julian, na.rm=T)) # n = number of observations for that species
early_late_umbs <- phen_flwr_control_umbs %>%
        add_count(species) %>%
        group_by(species, n) %>%
        summarize(julian_min = min(julian, na.rm=T)) # n = number of observations for that species

# Median Flower Date by SPECIES LEVEL - filter data to contain the median date of flower for each species at each plot
MedianFlwr_spp <- phen_flwr %>%
  group_by(plot, year, species, state, site, action, origin, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarize(julian_median = median(julian, na.rm=T))

# Duration of flowering time at the SPECIES level
flwr_dur_s <- phen_flwr %>% 
  group_by(site, plot, species, year, state, action, origin, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarise(flwr_duration = max(julian) - min(julian)) 

# Merge the data frames above so that you have one data frame that includes median date of flower, first date
# of flower, and duration of flowering at SPECIES LEVEL
phen_flwr_spp <- merge(FirstFlwr_spp, MedianFlwr_spp)
phen_flwr_spp <- merge(phen_flwr_spp, flwr_dur_s)

# write a new csv with flowering data at the SPECIES LEVEL and upload to the shared google drive
write.csv(phen_flwr_spp, file.path(L2_dir, "phenology/final_flwr_species_L2.csv"))

### Create a data frame at the PLOT LEVEL that includes median date of flower, first flower date, and duration

# Average first Flower Date by PLOT LEVEL
FirstFlwr_plot <- phen_flwr_spp %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor) %>%
  summarize(julian_min = mean(julian_min, na.rm=T))

# Average median Flower Date by PLOT LEVEL
MedianFlwr_plot <- phen_flwr_spp %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor) %>%
  summarize(julian_median = mean(julian_median, na.rm=T))

# Duration of flowering time for at the PLOT level
flwr_dur_plot <- phen_flwr_spp %>% 
  group_by(site, plot, year, state, action, insecticide, treatment_key, year_factor) %>%
  summarise(flwr_duration = mean(flwr_duration)) 

# Merge the data frames above so that you have one data frame that includes median date of flower, first date
# of flower, and duration of flowering at PLOT LEVEL
phen_flwr_plot <- merge(FirstFlwr_plot, MedianFlwr_plot)
phen_flwr_plot <- merge(phen_flwr_plot, flwr_dur_plot)

# write a new csv with flowering data at the PLOT LEVEL and upload to the shared google drive
write.csv(phen_flwr_plot, file.path(L2_dir, "phenology/final_flwr_plot_L2.csv"))


# Average first Flower Date by PLOT LEVEL by ORIGIN
FirstFlwr_plot_origin <- phen_flwr_spp %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor, origin) %>%
  summarize(julian_min = mean(julian_min, na.rm=T))

# Median Flower Date by PLOT LEVEL by ORIGIN
MedianFlwr_plot_origin <- phen_flwr_spp %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor, origin) %>%
  summarize(julian_median = mean(julian_median, na.rm=T))

# Duration of flowering time by PLOT LEVEL by ORIGIN
flwr_dur_plot_origin <- phen_flwr_spp %>% 
  group_by(site, plot, year, state, action, insecticide, treatment_key, year_factor, origin) %>%
  summarise(flwr_duration = mean(flwr_duration)) 

# Merge the data frames above so that you have one data frame that includes median date of flower, first date
# of flower, and duration of flowering at PLOT LEVEL
phen_flwr_plot_origin <- merge(FirstFlwr_plot_origin, MedianFlwr_plot_origin)
phen_flwr_plot_origin <- merge(phen_flwr_plot_origin, flwr_dur_plot_origin)

# write a new csv with flowering data at the PLOT LEVEL and upload to the shared google drive
write.csv(phen_flwr_plot_origin, file.path(L2_dir, "phenology/final_flwr_plot_origin_L2.csv"))


# Average first Flower Date by PLOT LEVEL by GROWTH HABIT
FirstFlwr_plot_growthhabit <- phen_flwr_spp %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarize(julian_min = mean(julian_min, na.rm=T))

# Average median Flower Date by PLOT LEVEL by GROWTH HABIT
MedianFlwr_plot_growthhabit <- phen_flwr_spp %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarize(julian_median = mean(julian_median, na.rm=T))

# Average duration of flowering time by PLOT LEVEL by GROWTH HABIT
flwr_dur_plot_growthhabit <- phen_flwr_spp %>% 
  group_by(site, plot, year, state, action, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarise(flwr_duration = mean(flwr_duration)) 

# Merge the data frames above so that you have one data frame that includes median date of flower, first date
# of flower, and duration of flowering at PLOT LEVEL
phen_flwr_plot_growthhabit <- merge(FirstFlwr_plot_growthhabit, MedianFlwr_plot_growthhabit)
phen_flwr_plot_growthhabit <- merge(phen_flwr_plot_growthhabit, flwr_dur_plot_growthhabit)

# write a new csv with flowering data at the PLOT LEVEL and upload to the shared google drive
write.csv(phen_flwr_plot_growthhabit, file.path(L2_dir, "phenology/final_flwr_plot_growthhabit_L2.csv"))


# Create some plots to visualize these data (NOT FINISHED - MY 7/11/21)
# histograms for each year - look at them together:
p1 <- ggplot(data = phen_flwr_plot, aes(x = julian_min, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p1 + facet_wrap(~year) + labs(title="Plot-level First Flower")

p1 <- ggplot(data = phen_flwr_spp, aes(x = julian_min, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p1 + facet_wrap(~year) + labs(title="Species-level First Flower")

# this will just show sampling date artifact
p2 <- ggplot(data = phen_flwr_plot, aes(x = julian_min, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p2 + facet_wrap(~year)

p2 <- ggplot(data = phen_flwr_spp, aes(x = julian_min, fill=state)) + geom_histogram(alpha=0.5, binwidth=10)
p2 + facet_wrap(~year) + labs(title="Species-level first Flower")

# Density plot
p3 <- ggplot(data = phen_flwr_plot, aes(x = julian_min, fill=state)) + geom_density(alpha=0.5)
p3 + facet_wrap(~year) + labs(title="Plot-level First Flower")

p3 <- ggplot(data = phen_flwr_spp, aes(x = julian_min, fill=state)) + geom_density(alpha=0.5)
p3 + facet_wrap(~year) + labs(title="Species-level First Flower")

# this will just show sampling date artifact
p4 <- ggplot(data = phen_flwr_plot, aes(x = julian_min, fill=state)) + geom_density(alpha=0.5)
p4 + facet_wrap(~year)


###### SEED SET (Moriah did this) ######
### Create a data frame at the SPECIES LEVEL that includes first date of seed
# First Seed by SPECIES LEVEL - filter data to contain the date of first seed for each species at each plot
FirstSd_spp <- phen_sd %>%
  group_by(plot, year, species, state, site, action, origin, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarize(julian_min = min(julian, na.rm=T))

### Create a data frame at the PLOT LEVEL that includes first date of seed
# First Seed Date by PLOT LEVEL
FirstSd_plot <- FirstSd_spp  %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor) %>%
  summarize(julian_min = mean(julian_min, na.rm=T))

# First Seed Date by PLOT LEVEL for ORIGIN
FirstSd_plot_origin <- FirstSd_spp  %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor, origin) %>%
  summarize(julian_min = mean(julian_min, na.rm=T))

# First Seed Date by PLOT LEVEL for GROWTH HABIT
FirstSd_plot_growthhabit <- FirstSd_spp  %>%
  group_by(plot, year, state, site, action, insecticide, treatment_key, year_factor, growth_habit) %>%
  summarize(julian_min = mean(julian_min, na.rm=T))

# write a new csv with first seed date at the SPECIES LEVEL and upload to the shared google drive
write.csv(FirstSd_spp, file.path(L2_dir, "phenology/final_sd_species_L2.csv"))

# write a new csv with first seed date at the PLOT LEVEL and upload to the shared google drive
write.csv(FirstSd_plot, file.path(L2_dir, "phenology/final_sd_plot_L2.csv"))

# write a new csv with first seed date at the ORIGIN PLOT LEVEL and upload to the shared google drive
write.csv(FirstSd_plot_origin, file.path(L2_dir, "phenology/final_sd_plot_origin_L2.csv"))

# write a new csv with first seed date at the GROWTH HABIT PLOT LEVEL and upload to the shared google drive
write.csv(FirstSd_plot_growthhabit, file.path(L2_dir, "phenology/final_sd_plot_growthhabit_L2.csv"))

