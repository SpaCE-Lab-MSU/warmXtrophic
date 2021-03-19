# TITLE:          Carbon and Nitrogen Data Cleanup
# AUTHORS:        Moriah Young
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Pat Bills, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN data is uploaded to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           March, 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory to Google Drive
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Read in csv files with CN content
CN1 <- read.csv("L0/CN_data/Zarnetske_CN_2019_P01.csv")
CN2 <- read.csv("L0/CN_data/Zarnetske_CN_2019_P02.csv")
CN3 <- read.csv("L0/CN_data/Zarnetske_CN_2019_P03.csv")
CN4 <- read.csv("L0/CN_data/Zarnetske_CN_2019_P04.csv")

# read in meta files for CN data
umbs_CN <- read.csv("L0/UMBS/2019/umbs_CN_2019.csv")
kbs_CN <- read.csv("L0/KBS/2019/kbs_CN_2019.csv")
meta <- read.csv("L0/plot.csv")

# Clean CN data
View(CN1)
CN1 <- CN1[-(1:2),] #get rid of the first 2 rows
names(CN1) <- CN1[1,] #make the first row the column names
CN1 <- CN1[-1,] #get rid of the first row because it's now the column names
CN1 <- CN1[-(1:7),] # get rid of first 7 rows
CN1 <- CN1[c(3, 4, 10, 11)] # get rid of unwanted columns
View(CN1)

View(CN2)
CN2 <- CN2[-(1:2),] #get rid of the first 2 rows
names(CN2) <- CN2[1,] #make the first row the column names
CN2 <- CN2[-1,] #get rid of the first row because it's now the column names
CN2 <- CN2[-(1:7),] # get rid of first 7 rows
CN2 <- CN2[c(3, 4, 10, 11)] # get rid of unwanted columns
View(CN2)

View(CN3)
CN3 <- CN3[-(1:2),] #get rid of the first 2 rows
names(CN3) <- CN3[1,] #make the first row the column names
CN3 <- CN3[-1,] #get rid of the first row because it's now the column names
CN3 <- CN3[-(1:7),] # get rid of first 7 rows
CN3 <- CN3[c(3, 4, 10, 11)] # get rid of unwanted columns
CN3 <- na.omit(CN3) # get rid of rows with NAs
View(CN3)

View(CN4)
CN4 <- CN4[-(1:2),] #get rid of the first 2 rows
names(CN4) <- CN4[1,] #make the first row the column names
CN4 <- CN4[-1,] #get rid of the first row because it's now the column names
CN4 <- CN4[-(1:7),] # get rid of first 7 rows
CN4 <- CN4[c(3, 4, 10, 11)] # get rid of unwanted columns
CN4 <- na.omit(CN4) # get rid of rows with NAs
View(CN4)

# create a function for the above lines of codes?

# merge separate CN files into one data frame
CN_all <- merge(CN1, CN2, all = TRUE)
CN_all <- merge(CN_all, CN3, all = TRUE)
CN_all <- merge(CN_all, CN4, all = TRUE)

CN_all$Sample[CN_all$Sample == "Blind Standard"] <- NA
CN_all <- na.omit(CN_all)
View(CN_all)

# clean meta data
umbs_CN <- umbs_CN[-c(7, 8, 9)] # get rid of unwanted columns
kbs_CN <- kbs_CN[-c(7, 8, 9)] # get rid of unwanted columns

meta_CN <- merge(umbs_CN, kbs_CN, all = TRUE)
colnames(meta_CN) <- sub("Unique_number", "Sample", colnames(meta_CN))# merge two meta data files
View(meta_CN)

# merge new CN data frame with 
CN_final <- merge(meta_CN, CN_all, by = "Sample")
names(CN_final) <- tolower(names(CN_final)) # column names to lower case
#CN_final <- clean_names(CN_final) # get rid of space and parenthesis in "weight (mg)" column
# ^ don't have the function for this

# change column name for merging
colnames(meta)[which(names(meta) == "treatment_key")] <- "treatment"
CN_final2 <- merge(CN_final, meta, by = c("plot", "treatment"))

View(CN_final)

# write a new cvs with the cleaned and merge data and upload to the shared google drive L1 folder
write.csv(CN_final2, file="L1/final_CN_L1.csv", row.names=FALSE)
