# Cleaning Taxon List

# Clear existing data
rm(list = ls())

# Load packages
library(tidyverse)
library(googlesheets)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
taxa <- read.csv()