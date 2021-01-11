# TITLE:          Cleaning Taxon List
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    
# PROJECT:        warmXtrophic
# DATE:           November, 2020

# Clear existing data
rm(list = ls())

# Load packages
library(tidyverse)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in data
taxa <- read.csv("L2/taxon.csv")
# Make sure that you create a new .csv file of the taxon_list.xlsx file IN the Google shared folder
# before you read in the data to ensure that you have the most up to date species list

# Clean data
taxa1 <- taxa[-(1:20),] #get rid of the first 20 rows
names(taxa1) <- taxa1[1,] #make the first row the column names
taxon_uptodate <- taxa1[-1,] #get rid of the first row because it's now the column names

# Save a .csv file with the cleaned taxon list
write.csv(taxon_uptodate, file = "L2/taxon.csv", row.names = FALSE) 
#this will save an up to date taxon list .csv file in the shared folder of the warmXtrophic project
