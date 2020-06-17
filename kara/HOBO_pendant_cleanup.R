# **** This script makes the following datasets:
# pendk:        KBS HOBO pendant data from 2015-2019
# pendu:        UMBS HOBO pendant data from 2015-2019
# pend1519:     Combined HOBO pendant data from KBS and UMBS from 2015-2019

# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# Set working directory - links to the data folder in warmXtrophic repository
setwd("/warmXtrophic/data/")

## Edit below for any packages you'll be using
for (package in c("tidyverse")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}