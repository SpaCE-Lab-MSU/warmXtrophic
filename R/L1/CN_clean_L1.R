# TITLE:          Carbon and Nitrogen Data Cleanup
# AUTHORS:        Moriah Young, Pat Bills
# COLLABORATORS:  Phoebe Zarnetske, Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN data is uploaded to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           March, 2021

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(janitor)

# Set working directory
Sys.getenv("L0DIR")
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L0_dir)

# Read in csv files with CN content
CN1_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_1_2019.csv"))
CN2_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_2_2019.csv"))
CN3_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_3_2019.csv"))
CN4_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_4_2019.csv"))

# clean up CN csv files before merging
CN_csvdata_initial_prep <- function(cn_data){
        cn_data <- cn_data[-(1:2),] #get rid of the first 2 rows because it's not data
        names(cn_data) <- cn_data[1,] #make the first row the column names
        cn_data <- cn_data[-1,] #get rid of the first row because it's now the column names
        cn_data <- cn_data[-(1:7),] #get rid of first 7 rows because these are the "standards" data
        cn_data <- cn_data[c(3, 4, 10, 11)] #get rid of unwanted columns that don't have data
        return(cn_data[!apply(is.na(cn_data) | cn_data == "", 1, all),])
}

# Clean CN data 
CN1 <- CN_csvdata_initial_prep(CN1_raw)
CN2 <- CN_csvdata_initial_prep(CN2_raw)
CN3 <- CN_csvdata_initial_prep(CN3_raw)
CN4 <- CN_csvdata_initial_prep(CN4_raw)

# read in meta files for CN data
umbs_CN <- read.csv(file.path(L0_dir, "UMBS/2019/umbs_CN_2019.csv"))
kbs_CN <- read.csv(file.path(L0_dir, "KBS/2019/kbs_CN_2019.csv"))
meta <- read.csv(file.path(L0_dir, "plot.csv"))

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
CN_final <- clean_names(CN_final) # get rid of space and parenthesis in "weight (mg)" column **function from janitor package**

# change column name for merging
colnames(meta)[which(names(meta) == "treatment_key")] <- "treatment"
CN_final <- merge(CN_final, meta, by = c("plot", "treatment"))

View(CN_final)

# write a new cvs with the cleaned and merge data and upload to the shared google drive L1 folder
write.csv(CN_final, file.path(L1_dir, "CN_L1.csv"))
