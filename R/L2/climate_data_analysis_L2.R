# TITLE: warmXtrophic - climate data analyses
# AUTHORS: Kara Dobson
# COLLABORATORS: Phoebe Zarnetske, Nina Lany, Kathryn Schmidt, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young
# DATA INPUT: Prism data downloaded to the L1 folder in the warmX drive
# DATA OUTPUT: 30 year precip and temp normals for KBS and UMBS counties (kzoo and emmet)
# PROJECT: warmXtrophic
# DATE: June 2021

# clear all existing data
rm(list=ls())

# load in packages and set working directory
library(tidyverse)
library(prism)
library(reshape2)
library(raster)
library(sp)

# set directory that prism data will be saved to
L1_dir<-Sys.getenv("L1DIR")
prism_set_dl_dir("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/L1/climate_data/")

# get 30-year normals for precip and temperature
# code below commented out because data is already downloaded - redownload when 2020 normals available
#get_prism_normals("ppt", "4km", annual = TRUE, keepZip = FALSE)
#get_prism_normals("tmean", "4km", annual = TRUE, keepZip = FALSE)
prism_archive_ls()

# code below is copied from https://rpubs.com/collnell/get_prism
## raster file of data
RS <- pd_stack(prism_archive_ls())
## assign projection info
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## convert raster to point data frame
df <- data.frame(rasterToPoints(RS))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat")
str(m.df)

# for some reason this doesn't work - these coordinates are in the dataset but it can't find them ?
# subset and filtering won't work, so I searched through the dataset for each coordinates' values
kzoo <- subset(m.df, lat == "42.25000" & lon == "-85.50000")
emmet <- m.df %>%
        filter(lat == 45.62500 & lon == -84.95833)

# chose coordinates within each county (above) and found these values in m.df manually
# precipitation 30 yr normals
# kzoo - 975.440002
# emmet - 770.690002

# temp 30 yr normals
# kzoo - 9.330833
# emmet - 6.421667

# actual coordinates for Kalamazoo county and Emmet county - not in data set, using closest coordinates
# kzoo <- c(42.2278, 85.5200)
# emmet <- c(45.6419, 84.9769)





### old code with NOAA climate data in warmX drive ###
# Set working directory
L1_dir<-Sys.getenv("L1DIR")

# load in the data
UMBS <- read.csv(file.path(L1_dir,"umbs_climate_data.csv"))
KBS <- read.csv(file.path(L1_dir,"kbs_climate_data.csv"))

# find temp averages
mean(KBS$TAVG, na.rm=T) 
mean(UMBS$TAVG, na.rm=T)

# precip averages
KBS$DATE <- as.Date(KBS$DATE)
UMBS$DATE <- as.Date(UMBS$DATE)
KBS$year <- format(KBS$DATE,format="%Y")
UMBS$year <- format(UMBS$DATE,format="%Y")

KBS %>% 
        group_by(year) %>%
        summarize(sum = sum(PRCP, na.rm=T)) %>%
        summarize(mean = mean(sum, na.rm = T))
UMBS %>%
        group_by(year) %>%
        summarize(sum = sum(PRCP, na.rm=T)) %>%
        summarize(mean = mean(sum, na.rm = T))

