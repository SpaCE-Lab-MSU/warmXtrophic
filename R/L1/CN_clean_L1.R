# TITLE:          Leaf Traits: Carbon and Nitrogen Data Cleanup 
# AUTHORS:        Moriah Young, Pat Bills, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN data is uploaded to the L1 plant comp folder
# PROJECT:        warmXtrophic
# DATE:           March-Nov 2021
# NOTES:        We have two different scanners at UMBS and KBS. The one at KBS provides leaf area, 
#               leaf length, average width, and maximum width. The one at UMBS just provides an area.  
#               Often SLA data is associated or “linked” with other data sets, sometimes with the same 
#               plant being harvested or surveyed. For example, we often took herbivory visual 
#               assessment data and took tissue for Carbon/Nitrogen analysis at the same time and same 
#               individual plant that SLA was taken. So, there are “details” associated with the SLA 
#               data set that will be provided in the updated methods.

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(janitor)

# Set working directory
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L1_dir)

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Volumes/GoogleDrive/Shared\ drives/SpaCE_Lab_warmXtrophic/data/L0"
L1_dir <- "/Volumes/GoogleDrive/Shared\ drives/SpaCE_Lab_warmXtrophic/data/L1"

# Read in csv files with CN content

#### PLOT ID INFO ####
meta <- read.csv(file.path(L0_dir, "plot.csv"))

# create function to clean CN data files - this function can be used for data still in the "weighsheets" format
CN_csvdata_initial_prep <- function(cn_data){
        cn_data <- cn_data[-(1:2),] #get rid of the first 2 rows because it's not data
        names(cn_data) <- cn_data[1,] #make the first row the column names
        cn_data <- cn_data[-1,] #get rid of the first row because it's now the column names
        cn_data <- cn_data[-(1:7),] #get rid of first 7 rows because these are the "standards" data
        cn_data <- cn_data[c(3, 4, 10, 11)] #get rid of unwanted columns that don't have data
        return(cn_data[!apply(is.na(cn_data) | cn_data == "", 1, all),])
}

#### CN ####
## 2017 ##
# KBS & UMBS 

# KBS
# SOCA - 3 different plates ran in the combustion analysis instrument for this species
# No "meta" data file existed for the 2017 data - Mark essentially cleaned this data in excel and the result of that cleaning
# are the files being uploaded below
# These are in TRIPLICATES (we decided to do singlets for years after 2017)
# SOCA
cn17k_soca_1 <- read.csv(file.path(L0_dir, "./KBS/2017/kbs_CN_Solidago_plate1_2017.csv"))
cn17k_soca_2 <- read.csv(file.path(L0_dir, "./KBS/2017/kbs_CN_Solidago_plate2_2017.csv"))
cn17k_soca_3 <- read.csv(file.path(L0_dir, "./KBS/2017/kbs_CN_Solidago_plate3_2017.csv"))

# merge above 3 data frames into one
CN_2017_soca <- merge(cn17k_soca_1, cn17k_soca_2, all = TRUE)
CN_2017_soca <- merge(CN_2017_soca, cn17k_soca_3, all = TRUE)

# ACMI
cn17k_acmi_1 <- read.csv(file.path(L0_dir, "./KBS/2017/CN_Acmi_2017_final.csv"))

# UMBS
# ***   2017 data are a few species, and only 2 Acmi sheets with SLA). 
#               CN_WeighSheet_Acmi_KBS_2017.xlsx, CN_Acmi_2017_final.xlsx
#       Other sheets (Cest and Solidago) have mass only

# CEST
# CEST basal
cn17u_cest_basal_1 <- read.csv(file.path(L0_dir, "./UMBS/2017/umbs_CN_Cest_2017_basal_leaves_plate_1A.csv"))
# CEST stem
cn17u_cest_stem_1 <- read.csv(file.path(L0_dir, "./UMBS/2017/umbs_CN_Cest_2017_stem_leaves_plate_1a.csv"))

# Cleaning KBS 2017 CN data
# SOCA samples
names(CN_2017_soca)
CN_2017_soca_edited <- CN_2017_soca[,-c(3, 4, 9, 10, 11, 13, 14, 15)] # delete unneeded columns
names(CN_2017_soca_edited)[1] <- "Site" #changing column name
names(CN_2017_soca_edited)[2] <- "Year" #changing column name
names(CN_2017_soca_edited)[8] <- "Nitrogen" #changing column name
names(CN_2017_soca_edited)[9] <- "Carbon" #changing column name
names(CN_2017_soca_edited)[4] <- "Plot" #changing column name
CN_2017_soca_edited$Site <- "kbs" #change site name to lowercase to match majority of other dataframes
names(CN_2017_soca_edited) <- tolower(names(CN_2017_soca_edited)) # column names to lower case
CN_2017_soca_edited <- merge(CN_2017_soca_edited, meta, all = TRUE)
CN_2017_soca_edited <- na.omit(CN_2017_soca_edited)

# ACMI samples
names(cn17k_acmi_1)
CN_2017_acmi_edited <- cn17k_acmi_1[,-c(4, 8, 10, 11)] # delete unneeded columns
names(CN_2017_acmi_edited)[7] <- "Year" #changing column name
CN_2017_acmi_edited$Year <- "2017"
names(CN_2017_acmi_edited)[4] <- "Weight_mg" #changing column name
names(CN_2017_acmi_edited)[2] <- "Plant_number" #changing column name
names(CN_2017_acmi_edited)[5] <- "Nitrogen" #changing column name
names(CN_2017_acmi_edited)[6] <- "Carbon" #changing column name
CN_2017_acmi_edited$Site <- "kbs" # add site column
CN_2017_acmi_edited$Species <- "Acmi" # add site column
CN_2017_acmi_edited <- na.omit(CN_2017_acmi_edited)
names(CN_2017_acmi_edited) <- tolower(names(CN_2017_acmi_edited)) # column names to lower case
CN_2017_acmi_edited_1 <- merge(CN_2017_acmi_edited, meta, all = TRUE)
CN_2017_acmi_edited_1 <- na.omit(CN_2017_acmi_edited_1)
CN_2017_acmi_edited_1["replicate"][CN_2017_acmi_edited_1["replicate"]=="A"]<- 1
CN_2017_acmi_edited_1["replicate"][CN_2017_acmi_edited_1["replicate"]=="B"]<- 2
CN_2017_acmi_edited_1["replicate"][CN_2017_acmi_edited_1["replicate"]=="C"]<- 3
names(CN_2017_acmi_edited_1)[3] <- "subsample_number" #changing column name to match 2017 soca data
# ***should double check that "replicate" and "subsample_number" mean the same thing***

CN_kbs_2017 <- merge(CN_2017_acmi_edited_1, CN_2017_soca_edited, all = TRUE) # merge edited kbs soca and acmi into one dataframe

# Cleaning UMBS 2017 CN data
# basal
names(cn17u_cest_basal_1)
CN_2017_cest_basal_edited <- cn17u_cest_basal_1[,-c(2, 3, 4, 10, 11, 12, 13, 14)] # delete unneeded columns
names(CN_2017_cest_basal_edited)[1] <- "Year" #changing column name
names(CN_2017_cest_basal_edited)[2] <- "Plot" #changing column name
names(CN_2017_cest_basal_edited)[4] <- "Species" #changing column name
names(CN_2017_cest_basal_edited)[6] <- "weight_mg" #changing column name
names(CN_2017_cest_basal_edited)[7] <- "nitrogen" #changing column name
names(CN_2017_cest_basal_edited)[8] <- "carbon" #changing column name
CN_2017_cest_basal_edited$Site <- "umbs" # add site column
CN_2017_cest_basal_edited <- na.omit(CN_2017_cest_basal_edited) # remove NAs
# Note that the above na.omit is a complete case; removes rows that have mass values where there are no C or N data
# Edit the above if needing to keep these mass values (for SLA for example)
names(CN_2017_cest_basal_edited) <- tolower(names(CN_2017_cest_basal_edited)) # column names to lower case
CN_2017_cest_basal_edited_1 <- merge(CN_2017_cest_basal_edited, meta, all = TRUE)
CN_2017_cest_basal_edited_1 <- na.omit(CN_2017_cest_basal_edited_1) # remove NAs which are plots without C or N data

# stem
names(cn17u_cest_stem_1)
CN_2017_cest_stem_edited <- cn17u_cest_stem_1[,-c(3, 4, 5, 11, 12, 13, 14, 15)] # delete unneeded columns
names(CN_2017_cest_stem_edited)[1] <- "site" #changing column name
CN_2017_cest_stem_edited$site <- "umbs" #change site name to lowercase to match majority of other dataframes
names(CN_2017_cest_stem_edited)[2] <- "year" #changing column name
names(CN_2017_cest_stem_edited)[3] <- "plot" #changing column name
names(CN_2017_cest_stem_edited)[5] <- "species" #changing column name
names(CN_2017_cest_stem_edited)[7] <- "weight_mg" #changing column name
names(CN_2017_cest_stem_edited)[8] <- "nitrogen" #changing column name
names(CN_2017_cest_stem_edited)[9] <- "carbon" #changing column name
CN_2017_cest_stem_edited <- na.omit(CN_2017_cest_stem_edited) # remove NAs
# Note that the above na.omit is a complete case; removes rows that have mass values where there are no C or N data
# Edit the above if needing to keep these mass values (for SLA for example)
names(CN_2017_cest_stem_edited) <- tolower(names(CN_2017_cest_stem_edited)) # column names to lower case
CN_2017_cest_stem_edited_1 <- merge(CN_2017_cest_stem_edited, meta, all = TRUE)
CN_2017_cest_stem_edited_1 <- na.omit(CN_2017_cest_stem_edited_1) # remove NAs 
# Note that the above na.omit is a complete case; removes rows that have, in this case, no data (mass, C, and N values)
CN_umbs_2017 <- merge(CN_2017_cest_stem_edited_1, CN_2017_cest_basal_edited_1, all = TRUE) # merge edited cest basal and stem into one dataframe


# Merge all 2017 CN data together
CN_2017 <- merge(CN_umbs_2017, CN_kbs_2017, all = TRUE)
# reminder that kbs samples were analyzed in triplicate, umbs samples were analyzed in singlets

## 2018 ##
# KBS
cn18k_meta <- read.csv(file.path(L0_dir, "./KBS/2018/kbs_CN_UniqueID_2018.csv")) # meta data file
kbs_2018_1 <- read.csv(file.path(L0_dir, "./KBS/2018/kbs_CN_weightsheet_1_2018.csv")) # Soca
kbs_2018_2 <- read.csv(file.path(L0_dir, "./KBS/2018/kbs_CN_weightsheet_2_2018.csv")) # Acmi

kbs_2018_1_edited <- CN_csvdata_initial_prep(kbs_2018_1) # using function from top of script
kbs_2018_2_edited <- CN_csvdata_initial_prep(kbs_2018_2) # using function from top of script
kbs_2018_1_edited <- kbs_2018_1_edited[!(kbs_2018_1_edited$Sample=="Blind Standard"),] # delete blind standards in data
kbs_2018_2_edited <- kbs_2018_2_edited[!(kbs_2018_2_edited$Sample=="Blind Standard"),]

CN_kbs_2018 <- merge(kbs_2018_1_edited, kbs_2018_2_edited, all = TRUE) # merge kbs 2018 cn data into one dataframe
names(CN_kbs_2018)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_kbs_2018 <- merge(CN_kbs_2018, cn18k_meta, all = TRUE)
CN_kbs_2018 $Year <- "2018"
CN_kbs_2018 <- CN_kbs_2018[,-c(6, 10)] # delete unneeded columns
CN_kbs_2018 <- na.omit(CN_kbs_2018) # get rid of NAs which are data without C and N

# UMBS
cn18u_meta <- read.csv(file.path(L0_dir, "./UMBS/2018/umbs_CN_UniqueID_2018.csv"))
umbs_2018_1 <- read.csv(file.path(L0_dir, "./UMBS/2018/umbs_CN_weighsheet_1_2018.csv")) # Cest
umbs_2018_2 <- read.csv(file.path(L0_dir, "./UMBS/2018/umbs_CN_weighsheet_2_2018.csv")) # Popr

umbs_2018_1_edited <- CN_csvdata_initial_prep(umbs_2018_1) # using function from top of script
umbs_2018_2_edited <- CN_csvdata_initial_prep(umbs_2018_2) # using function from top of script
umbs_2018_1_edited <- umbs_2018_1_edited[!(umbs_2018_1_edited$Sample=="Blind Standard"),] # delete blind standards in data
umbs_2018_2_edited <- umbs_2018_2_edited[!(umbs_2018_2_edited$Sample=="Blind Standard"),]

CN_umbs_2018 <- merge(umbs_2018_1_edited, umbs_2018_2_edited, all = TRUE) # merge umbs 2018 cn data into one dataframe
names(CN_umbs_2018)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_umbs_2018 <- merge(CN_umbs_2018, cn18u_meta, all = TRUE)
CN_umbs_2018$Year <- "2018"
CN_umbs_2018 <- CN_umbs_2018[,-c(6, 10)] # delete unneeded columns
CN_umbs_2018 <- na.omit(CN_umbs_2018) # get rid of NAs which are data without C and N
CN_umbs_2018 <- CN_umbs_2018[!(CN_umbs_2018$Nitrogen == ""), ] # delete row that did not have C and N data due to leaking tin

CN_2018 <- merge(CN_kbs_2018, CN_umbs_2018, all = TRUE) # merge kbs and umbs 2018 cn data into one dataframe
names(CN_2018)[2] <- "weight_mg" #changing column name
names(CN_2018) <- tolower(names(CN_2018)) # column names to lower case
CN_2018 <- merge(CN_2018, meta, all = TRUE)

## 2019 ##
# These data were originally in 4 files at L0_dir, "./CN_data/2019/CN_WeighSheet_1_2019.csv"... "_2_", "_3_", "_4_"))
# KD manually separated files into appropriate site-level files
# KBS
cn19k_meta <- read.csv(file.path(L0_dir, "./KBS/2019/kbs_CN_2019.csv")) # this is basically a meta data file for the
# samples that were analyzed - the "unique_number" in this file should correspond with the "sample" column in the files
# below
cn19k_samples_1 <- read.csv(file.path(L0_dir, "./KBS/2019/kbs_CN_weighsheet_1_2019.csv")) # this has some umbs samples in it
# that will need to be deleted when cleaned (they were ran on the same plate for combustion analysis)
cn19k_samples_2 <- read.csv(file.path(L0_dir, "./KBS/2019/kbs_CN_weighsheet_2_2019.csv"))
# UMBS
cn19u_meta <- read.csv(file.path(L0_dir, "./UMBS/2019/umbs_CN_2019.csv")) # this is basically a meta data file for the
# samples that were analyzed - the "unique_number" in this file should correspond with the "sample" column in the files
# below
cn19u_samples_1 <- read.csv(file.path(L0_dir, "./UMBS/2019/umbs_CN_weighsheet_1_2019.csv"))
cn19u_samples_2 <- read.csv(file.path(L0_dir, "./UMBS/2019/umbs_CN_weighsheet_2_2019.csv"))
cn19u_samples_3 <- read.csv(file.path(L0_dir, "./UMBS/2019/umbs_CN_weighsheet_3_2019.csv")) # this has some kbs samples in it
# that will need to be deleted when cleaned

# Cleaning KBS CN 2019 samples
cn19k_samples_1_edited <- CN_csvdata_initial_prep(cn19k_samples_1)
cn19k_samples_1_edited <- cn19k_samples_1_edited[-(1:21),] # delete umbs samples in data
cn19k_samples_1_edited <- cn19k_samples_1_edited[!(cn19k_samples_1_edited$Sample=="Blind Standard"),] # delete blind standards in data
cn19k_samples_1_edited <- cn19k_samples_1_edited[-c(1,6),] # delete empty rows

cn19k_samples_2_edited <- CN_csvdata_initial_prep(cn19k_samples_2)
cn19k_samples_2_edited <- cn19k_samples_2_edited[!(cn19k_samples_2_edited$Sample=="Blind Standard"),] # delete blind standards in data

CN_kbs_2019 <- merge(cn19k_samples_1_edited, cn19k_samples_2_edited, all = TRUE) # merge kbs 2019 cn data into one dataframe
names(CN_kbs_2019)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_kbs_2019 <- merge(CN_kbs_2019, cn19k_meta, all = TRUE)
CN_kbs_2019 <- CN_kbs_2019[,-c(7, 12, 13)] # delete unneeded columns
CN_kbs_2019 <- na.omit(CN_kbs_2019) # get rid of NAs which are data without C and N

# Cleaning UMBS 2019 CN samples
cn19u_samples_1_edited <- CN_csvdata_initial_prep(cn19u_samples_1)
cn19u_samples_1_edited <- cn19u_samples_1_edited[!(cn19u_samples_1_edited$Sample=="Blind Standard"),] # delete blind standards in data

cn19u_samples_2_edited <- CN_csvdata_initial_prep(cn19u_samples_2)
cn19u_samples_2_edited <- cn19u_samples_2_edited[!(cn19u_samples_2_edited$Sample=="Blind Standard"),] # delete blind standards in data

cn19u_samples_3_edited <- CN_csvdata_initial_prep(cn19u_samples_3)
cn19u_samples_3_edited <- cn19u_samples_3_edited[!(cn19u_samples_3_edited$Sample=="Blind Standard"),] # delete blind standards in data
cn19u_samples_3_edited <- cn19u_samples_3_edited[c(1:20),] # only keep umbs data

CN_umbs_2019 <- merge(cn19u_samples_1_edited, cn19u_samples_2_edited, all = TRUE) # merge umbs 2019 cn data into one dataframe
CN_umbs_2019 <- merge(CN_umbs_2019, cn19u_samples_3_edited, all = TRUE) # merge umbs 2019 cn data into one dataframe
CN_umbs_2019 <- merge(CN_umbs_2019, cn19u_samples_3_edited, all = TRUE)
names(CN_umbs_2019)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_umbs_2019 <- merge(CN_umbs_2019, cn19u_meta, all = TRUE)
CN_umbs_2019 <- CN_umbs_2019[,-c(7, 12, 13)] # delete unneeded columns
CN_umbs_2019 <- na.omit(CN_umbs_2019) # get rid of NAs

CN_2019 <- merge(CN_umbs_2019, CN_kbs_2019, all = TRUE)
names(CN_2019)[2] <- "Weight_mg" #changing column name so that I merge this with the meta data
names(CN_2019) <- tolower(names(CN_2019)) # column names to lower case
CN_2019 <- CN_2019[,-c(1,6,8)] # delete unneeded columns
CN_2019$year <- 2019
CN_2019 <- merge(CN_2019, meta, all = TRUE) #merge with meta data

# 2020
# KBS
cn20k_meta <- read.csv(file.path(L0_dir, "./KBS/2020/kbs_CN_UniqueID_2020.csv")) # meta data file
kbs_2020_1 <- read.csv(file.path(L0_dir, "./KBS/2020/kbs_CN_weighsheet_1_2020.csv")) # Soca

kbs_2020_1_edited <- CN_csvdata_initial_prep(kbs_2020_1) # using function from top of script
kbs_2020_1_edited <- kbs_2020_1_edited[!(kbs_2020_1_edited$Sample=="Blind Standard"),] # delete blind standards in data

names(kbs_2020_1_edited)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_kbs_2020 <- merge(kbs_2020_1_edited, cn20k_meta, all = TRUE)
CN_kbs_2020$Year <- "2020"
CN_kbs_2020 <- CN_kbs_2020[,-c(6, 10)] # delete unneeded columns
CN_kbs_2020 <- na.omit(CN_kbs_2020) # get rid of NAs which are data without C and N

# UMBS
cn20u_meta <- read.csv(file.path(L0_dir, "./UMBS/2020/umbs_CN_UniqueID_2020.csv")) # meta data file
umbs_2020_1 <- read.csv(file.path(L0_dir, "./UMBS/2020/umbs_CN_weighsheet_1_2020.csv")) # Popr
umbs_2020_2 <- read.csv(file.path(L0_dir, "./UMBS/2020/umbs_CN_weighsheet_2_2020.csv")) # Cest

umbs_2020_1_edited <- CN_csvdata_initial_prep(umbs_2020_1) # using function from top of script
umbs_2020_2_edited <- CN_csvdata_initial_prep(umbs_2020_2)
umbs_2020_1_edited <- umbs_2020_1_edited[!(umbs_2020_1_edited$Sample=="Blind Standard"),] # delete blind standards in data
umbs_2020_2_edited <- umbs_2020_2_edited[!(umbs_2020_2_edited$Sample=="Blind Standard"),]

CN_umbs_2020 <- merge(umbs_2020_1_edited, umbs_2020_2_edited, all = TRUE) # merge umbs 2018 cn data into one dataframe
names(CN_umbs_2020)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_umbs_2020 <- merge(CN_umbs_2020, cn20u_meta, all = TRUE)
CN_umbs_2020$Year <- "2020"
CN_umbs_2020 <- CN_umbs_2020[,-c(6, 10)] # delete unneeded columns
CN_umbs_2020 <- na.omit(CN_umbs_2020) # get rid of NAs which are data without C and N
CN_umbs_2020 <- CN_umbs_2020[!(CN_umbs_2020$Nitrogen == ""), ] # delete row that did not have C and N data due to leaking tin

CN_2020 <- merge(CN_kbs_2020, CN_umbs_2020, all = TRUE) # merge kbs and umbs 2018 cn data into one dataframe
names(CN_2020)[2] <- "weight_mg" #changing column name
names(CN_2020) <- tolower(names(CN_2020)) # column names to lower case
CN_2020 <- merge(CN_2020, meta, all = TRUE)

# 2021
# KBS
cn21k_meta <- read.csv(file.path(L0_dir, "./KBS/2021/kbs_CN_UniqueID_2021.csv")) # meta data file
kbs_2021_1 <- read.csv(file.path(L0_dir, "./KBS/2021/kbs_CN_weighsheet_1_2021.csv")) # Soca

kbs_2021_1_edited <- CN_csvdata_initial_prep(kbs_2021_1) # using function from top of script
kbs_2021_1_edited <- kbs_2021_1_edited[!(kbs_2021_1_edited$Sample=="Blind Standard"),] # delete blind standards in data

names(kbs_2021_1_edited)[1] <- "Unique_number" #changing column name so that I merge this with the meta data
CN_kbs_2021 <- merge(kbs_2021_1_edited, cn21k_meta, all = TRUE)
CN_kbs_2021$Year <- "2021"
CN_kbs_2021 <- CN_kbs_2021[,-c(6, 10)] # delete unneeded columns
# CN_kbs_2021 <- na.omit(CN_kbs_2021) # get rid of NAs which are data without C and N
names(CN_kbs_2021)[2] <- "weight_mg" #changing column name
names(CN_kbs_2021) <- tolower(names(CN_kbs_2021)) # column names to lower case
CN_2021 <- merge(CN_kbs_2021, meta, all = TRUE) #merge with meta data
CN_2021 <- na.omit(CN_2021) # get rid of NAs which are data without C and N

# Merge each year's cleaned CN data - right now only have 2017 and 2019 cleaned CN data
CN_17_18 <- merge(CN_2017, CN_2018, all = TRUE)
CN_19_20 <- merge(CN_2019, CN_2020, all = TRUE)
CN <- merge(CN_17_18 , CN_19_20, all = TRUE)
CN_all <- merge(CN, CN_2021, all = TRUE)

# write a new csv with the cleaned and merge data and upload to the shared google drive L1 folder
write.csv(CN_all, file.path(L1_dir, "./CN/CN_L1.csv"), row.names=F)
