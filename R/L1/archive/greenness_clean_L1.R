# TITLE:          warmxtrophic: Greenness cleanup
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Moriah Young, Pat Bills, Phoebe Zarnetske, Mark Hammond, Kristin Wolford
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing clean greenness is uploaded to the L1 folder
# PROJECT:        warmXtrophic
# DATE:           July 2021


########### not used in manuscript #############

# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse)

# Load data
L0_dir<-Sys.getenv("L0DIR")
L1_dir<-Sys.getenv("L1DIR")
meta <- read.csv(file.path(L0_dir, "plot.csv"))
green <- read.csv(file.path(L0_dir, "KBS/2021/kbs_chlorophyll_2021.csv"))

# Add species column
green$species <- "Soca"

# Data otherwise looks good, merge with meta data
green_merge <- left_join(meta, green, by = "plot")

# Remove rows w/ NAs for plots that weren't samples from
green_clean <- green_merge[complete.cases(green_merge),]

# Upload clean csv to google drive
write.csv(green_clean, file.path(L1_dir,"Greenness/greenness_L1.csv"), row.names=F)
