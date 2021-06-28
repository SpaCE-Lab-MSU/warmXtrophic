# TITLE:          Leaf Traits: Carbon and Nitrogen, Specific Leaf Area (SLA) Data Cleanup 
# AUTHORS:        Moriah Young, Pat Bills, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN & SLA data is uploaded to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           March-June, 2021

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
source(file.path(L1_dir,"PAR_functions_L1.R"))

# Read in csv files with CN, SLA content

#### PLOT ID INFO ####
meta <- read.csv(file.path(L0_dir, "plot.csv"))

#### SLA ####
## 2017 ##
# ***   pre-2018 data don't seem to exist (Although I thought we started in 2017 - maybe these are just for the slug herbivory trials and not plot samples??)

## 2018 ##
# KBS
# ***   2018 missing KBS SLA data
# UMBS
sla18u_raw <- read.csv(file.path(L0_dir, "UMBS/2018/umbs_SLA_2018.csv"))

## 2019 ##
# KBS
sla19k_raw <- read.csv(file.path(L0_dir, "KBS/2019/kbs_SLA_2019.csv")) 
# UMBS
sla19u_raw <- read.csv(file.path(L0_dir, "UMBS/2019/umbs_SLA_2019.csv")) 

## 2020 ##
# KBS
# ***   2020 KBS SLA data missing??
# UMBS
sla20u_raw <- read.csv(file.path(L0_dir, "UMBS/2020/umbs_CN_SLA_2020.csv")) 

#### CN ####
## 2017 ##
# KBS & UMBS 
# ***   2017 data are difficult to understand (a few species, and only 2 Acmi sheets with SLA). This is the summer I was on medical leave so I don't recall everything, maybe it was only for slug herbivory samples??
#               CN_WeighSheet_Acmi_KBS_2017.xlsx, CN_Acmi_2017_final.xlsx
#       Other sheets (Cest and Solidago) have mass only
#       Leave 2017 off for now until we can confirm these data.

## 2018 ##
# KBS - but this doesn't contain data; however the "CN_Inventory.xlsx" file in /CD_data suggests these exist
cn18k_raw <- read.csv(file.path(L0_dir, "KBS/2018/kbs_CN_2018.csv"))
# UMBS
# ***   Where are these data? the "CN_Inventory.xlsx" file in /CD_data suggests these exist.

## 2019 ##
# ***   Why are these data separate and not nested inside /KBS or /UMBS?
cn19_1_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_1_2019.csv"))
cn19_2_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_2_2019.csv"))
cn19_3_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_3_2019.csv"))
cn19_4_raw <- read.csv(file.path(L0_dir, "CN_data/2019/CN_WeighSheet_4_2019.csv"))

#### Create L1 SLA data ####
head(sla18u_raw)
head(sla19k_raw) # these data also contain Length_cm2 Avg_width_cm2 Fresh_leaf_g Fresh_midriff_g Dry_leaf_g Dry_midriff_g
head(sla19u_raw)
head(sla20u_raw) 
unique(sla20u_raw$notes) # notes column indicates need to drop zeros, and edit one entry
sort(unique(sla20u_raw$Mass_g))
# Edit 2020 SLA UMBS
sla20u_raw["Mass_g"][sla20u_raw["Mass_g"]==0.080]<-0.008
sla20u_raw$Unique.number<-NULL
sla20u_raw$notes<-NULL
sla20u_raw$Treatment<-NULL

# Are the masses Wet or Dry? Assume Wet for now (check with MORIAH)
# Edit 2019 KBS SLA to add up mass
sla19k_raw$Mass_g<-sla19k_raw$Fresh_leaf_g + sla19k_raw$Fresh_midriff_g
sla19k_raw$Fresh_leaf_g<-NULL
sla19k_raw$Fresh_midriff_g<-NULL
sla19k_raw$Dry_leaf_g<-NULL
sla19k_raw$Dry_midriff_g<-NULL
sla19k_raw$Length_cm2<-NULL
sla19k_raw$Avg_width_cm2<-NULL

# Common columns = Site    Date Julian Plot Species Plant_Number Area_cm2  Mass_g
# Merge data
sla_final<-rbind(sla18u_raw,sla19k_raw, sla19u_raw, sla20u_raw)
# column names to lower case
names(sla_final) <- tolower(names(sla_final)) 
# remove zero values for Mass
sla_final<-sla_final[sla_final$mass_g != 0, ]
summary(sla_final) # 46 NA values for mass, area; omit them
dim(sla_final)
sla_final<-sla_final[rowSums(is.na(sla_final[ , 6:8])) == 0, ]
dim(sla_final)

# Merge in plot ID info
sla_final <- merge(sla_final, meta, by = c("plot"))

# convert to day, month, year -- use POSIXlt *** NEED TO UPDATE FOR 2018 bc in diff format
sla_final$day<-strptime(sla_final$date, "%m/%d/%Y")$mday
# convert to day of year (Julian date)
sla_final$julian<-strptime(sla_final$date, "%m/%d/%Y")$yday+1
sla_final$month<-strptime(sla_final$date, "%m/%d/%Y")$mon+1
sla_final$year<-strptime(sla_final$date, "%m/%d/%Y")$year+1900

# write a new csv with the cleaned and merged data and upload to the shared google drive L1 folder
write.csv(sla_final, file.path(L1_dir, "SLA/SLA_L1.csv", row.names=F))

#### Create L1 CN data ####

### 2017 DATA ###

### 2018 DATA ###

### 2019 DATA ###

# create function to clean 2019 CN data files
CN_csvdata_initial_prep <- function(cn_data){
        cn_data <- cn_data[-(1:2),] #get rid of the first 2 rows because it's not data
        names(cn_data) <- cn_data[1,] #make the first row the column names
        cn_data <- cn_data[-1,] #get rid of the first row because it's now the column names
        cn_data <- cn_data[-(1:7),] #get rid of first 7 rows because these are the "standards" data
        cn_data <- cn_data[c(3, 4, 10, 11)] #get rid of unwanted columns that don't have data
        return(cn_data[!apply(is.na(cn_data) | cn_data == "", 1, all),])
}

# Clean 2019 CN data 
CN1 <- CN_csvdata_initial_prep(cn19_1_raw)
CN2 <- CN_csvdata_initial_prep(cn19_2_raw)
CN3 <- CN_csvdata_initial_prep(cn19_3_raw)
CN4 <- CN_csvdata_initial_prep(cn19_4_raw)

# read in 2019 meta files for CN data
umbs19_CN <- read.csv(file.path(L0_dir, "UMBS/2019/umbs_CN_2019.csv"))
kbs19_CN <- read.csv(file.path(L0_dir, "KBS/2019/kbs_CN_2019.csv"))

# merge separate CN files into one data frame
CN_all <- merge(CN1, CN2, all = TRUE)
CN_all <- merge(CN_all, CN3, all = TRUE)
CN_all <- merge(CN_all, CN4, all = TRUE)

CN_all$Sample[CN_all$Sample == "Blind Standard"] <- NA
CN_all <- na.omit(CN_all)
View(CN_all)

# clean meta data
umbs19_CN <- umbs19_CN[-c(7, 8, 9)] # get rid of unwanted columns
kbs19_CN <- kbs19_CN[-c(7, 8, 9)] # get rid of unwanted columns

### 2020 DATA ###

### EDIT OTHER YR FILES ABOVE IN CHRONOLOGICAL ORDER BEFORE NEXT STEP

# NOTE: edit to reflect diff yrs when add in other YEAR files from above
meta_CN <- merge(umbs19_CN, kbs19_CN, all = TRUE)
colnames(meta_CN) <- sub("Unique_number", "Sample", colnames(meta_CN))# merge two meta data files
View(meta_CN)

# merge new CN data frame with the meta data frame
CN_final <- merge(meta_CN, CN_all, by = "Sample")
names(CN_final) <- tolower(names(CN_final)) # column names to lower case
CN_final <- clean_names(CN_final) # get rid of space and parenthesis in "weight (mg)" column **function from janitor package**

# change column name for merging
colnames(meta)[which(names(meta) == "treatment_key")] <- "treatment"
CN_final <- merge(CN_final, meta, by = c("plot", "treatment"))

View(CN_final)

# write a new cvs with the cleaned and merge data and upload to the shared google drive L1 folder
write.csv(CN_final, file.path(L1_dir, "CN_L1.csv"))
