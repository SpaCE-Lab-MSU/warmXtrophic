# TITLE:          Leaf Traits: Carbon and Nitrogen, Specific Leaf Area (SLA) Data Cleanup 
# AUTHORS:        Moriah Young, Pat Bills, Phoebe Zarnetske
# COLLABORATORS:  Mark Hammond, Kara Dobson
# DATA INPUT:     Data imported as csv files from shared Google drive L0 folder
# DATA OUTPUT:    A csv file containing CN & SLA data is uploaded to the L1 plant comp folder
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
sla18u <- read.csv(file.path(L0_dir, "./UMBS/2018/umbs_SLA_2018.csv"))

## 2019 ##
# KBS 
sla19k <- read.csv(file.path(L0_dir, "./KBS/2019/kbs_SLA_2019.csv")) 
# UMBS
sla19u <- read.csv(file.path(L0_dir, "./UMBS/2019/umbs_SLA_2019.csv")) 

## 2020 ##
# KBS 
# Edit 2020 SLA KBS to remove first 3 rows, rename columns
## First 3 rows = notes:
# Unique Sample Code is the plot, species, and unique plant number together. This could provide a unique sample code which will be associated with Carbon Nitrogen sample analysis.															
# Yellowing of leaf assessment.  Samples were harvested in fall as natural senescence was starting with the forbs.  I did note if leaves were starting to turn yellow.  Y is for Yes.  N															
# Percent leaf surface damaged is the sum of visual assessment of leaf damage due to herbivory and/or a dark spot disease.  The disease appears to be ubiquitious on goldenrod in the fall.  															
headers21 = read.csv(file.path(L0_dir, "./KBS/2020/WarmX_KBS_2020_SLA Biomass_MHproof.csv"), skip = 3, header = F, nrows = 1, as.is = T)
sla20k = read.csv(file.path(L0_dir, "./KBS/2020/WarmX_KBS_2020_SLA Biomass_MHproof.csv"), skip = 4, header = F)
colnames(sla20k)= headers21
# UMBS
sla20u <- read.csv(file.path(L0_dir, "./UMBS/2020/umbs_CN_SLA_2020.csv")) 

## 2021 ##
# KBS #
sla21k <- read.csv(file.path(L0_dir, "./KBS/2021/KBS_WarmX_SLA_2021.csv")) 
# No UMBS 2021 SLA data #

#### CN ####
## 2017 ##
# KBS & UMBS 
# ***   2017 data are difficult to understand (a few species, and only 2 Acmi sheets with SLA). This is the summer I was on medical leave so I don't recall everything, maybe it was only for slug herbivory samples??
#               CN_WeighSheet_Acmi_KBS_2017.xlsx, CN_Acmi_2017_final.xlsx
#       Other sheets (Cest and Solidago) have mass only
#       Leave 2017 off for now until we can confirm these data.

# KBS
# SOCA - 3 different plates ran in the combustion analysis instrument for this species
# No "meta" data file existed for the 2017 data - Mark essentially cleaned this data in excel and the result of that cleaning
# are the files being uploaded below
# These are in TRIPLICATES (we decided to do singlets for years after 2017)
cn17k_soca_1 <- read.csv(file.path(L0_dir, "./KBS/2017/kbs_CN_Solidago_plate1_2017.csv"))
cn17k_soca_2 <- read.csv(file.path(L0_dir, "./KBS/2017/kbs_CN_Solidago_plate2_2017.csv"))
cn17k_soca_3 <- read.csv(file.path(L0_dir, "./KBS/2017/kbs_CN_Solidago_plate3_2017.csv"))

# UMBS
# CEST - not yet analyzed
# CEST basal sheets 1 and 2
# CEST stem sheets 1 and 2

## 2018 ##
# ***This data is currently being ground and prepped to be analyzed***
# KBS - but this doesn't contain data; however the "CN_Inventory.xlsx" file in /CD_data suggests these exist - MY: yes these
# exist, the file below acts like a meta data file for all the existing samples
cn18k_meta <- read.csv(file.path(L0_dir, "./KBS/2018/kbs_CN_2018.csv")) # meta data file
# UMBS
# ***   Where are these data? the "CN_Inventory.xlsx" file in /CD_data suggests these exist.
cn18u_meta <- read.csv(file.path(L0_dir, "./UMBS/2018/umbs_CN_2018.csv"))

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

#### Create L1 SLA data ####
names(sla18u)
# [1] "Site"         "Date"         "Julian"       "Plot"         "Species"      "Plant_Number" "Area_cm2"    
# [8] "Mass_g"   
names(sla19k) 
# [1] "Site"            "Date"            "Julian"          "Plot"            "Species"        
# [6] "Plant_Number"    "Area_cm2"        "Length_cm2"      "Avg_width_cm2"   "Fresh_leaf_g"   
# [11] "Fresh_midriff_g" "Dry_leaf_g"      "Dry_midriff_g"  
names(sla20k)  
# [1] "Harvest_Date"                         "Plot"                                
# [3] "Species_Code"                         "Plant_Number"                        
# [5] "Unique_Sample_code"                   "Leaf_Area_cm2"                       
# [7] "Leaf_Length_cm"                       "Avg_Width_cm"                        
# [9] "Max_Width_cm"                         "Yellowing_of_Leaf_Assessment"        
# [11] "Percent_of_Leaf_Surface_Area_Damaged" "Dried_Biomass_g"                     
# [13] "Dried_Biomass_Notes_from_Mark"        "No_Sample_for_SLA"                   
# [15] "mark_proofing_notes"                  "SLA_cm2_per_gram" 
names(sla21k)
# [1] "Harvest_Date"                 "Species_Code"                 "Plot_ID"                     
# [4] "Plant_Number"                 "Length_cm"                    "Average_Width_cm"            
# [7] "Dried_Biomass_g"              "leaf_area_cm2"                "Notes"                       
# [10] "Specific_Leaf_Area_cm2_per_g"
names(sla19u)
# [1] "Site"         "Date"         "Julian"       "Plot"         "Species"      "Plant_Number" "Area_cm2"    
# [8] "Mass_g"
names(sla20u) 
# [1] "Site"          "Date"          "Julian"        "Plot"          "Treatment"     "Species"      
# [7] "Plant_Number"  "Area_cm2"      "Mass_g"        "Unique.number" "notes"

unique(sla20u$notes) # notes column indicates need to drop zeros, and edit one entry
sort(unique(sla20u$Mass_g))

# Edit 2020 SLA UMBS
sla20u["Mass_g"][sla20u["Mass_g"]==0.080]<-0.008
sla20u$Unique.number<-NULL
sla20u$notes<-NULL
sla20u$Treatment<-NULL

# Edit 2020 SLA KBS
# Plant_Number has letters; omit (later we will code all plant numbers to letters)
sort(unique(sla20k$Plant_Number))
sla20k["Plant_Number"][sla20k["Plant_Number"]=="1a"]<-1
sla20k["Plant_Number"][sla20k["Plant_Number"]=="1b"]<-1
sla20k["Plant_Number"][sla20k["Plant_Number"]=="2a"]<-2
sla20k["Plant_Number"][sla20k["Plant_Number"]=="2b"]<-2
sla20k["Plant_Number"][sla20k["Plant_Number"]=="3a"]<-3
sla20k["Plant_Number"][sla20k["Plant_Number"]=="3b"]<-3
sla20k["Plant_Number"][sla20k["Plant_Number"]=="4a"]<-4
sla20k["Plant_Number"][sla20k["Plant_Number"]=="4b"]<-4
sla20k["Plant_Number"][sla20k["Plant_Number"]=="5a"]<-5
sla20k["Plant_Number"][sla20k["Plant_Number"]=="5b"]<-5
sort(unique(sla20k$Plant_Number))

# Are the masses Wet or Dry? Assume Wet for now (check with MORIAH)
# Edit 2019 KBS SLA to add up mass
sla19k$Mass_g<-sla19k$Fresh_leaf_g + sla19k$Fresh_midriff_g
sla19k$Fresh_leaf_g<-NULL
sla19k$Fresh_midriff_g<-NULL
sla19k$Dry_leaf_g<-NULL
sla19k$Dry_midriff_g<-NULL
# Edit to reflect no squared measures (check with MORIAH)
names(sla19k)[names(sla19k) == "Avg_width_cm2"] <-"Avg_Width_cm"
names(sla19k)[names(sla19k) == "Length_cm2"] <-"Length_cm"

# Edit KBS 2020 and 2021 data format
names(sla21k)[names(sla21k) == "Plot_ID"] <-"Plot"
names(sla20k)[names(sla20k) == "Harvest_Date"] <-"Date"
names(sla21k)[names(sla21k) == "Harvest_Date"] <-"Date"
names(sla20k)[names(sla20k) == "Species_Code"] <-"Species"
names(sla21k)[names(sla21k) == "Species_Code"] <-"Species"
names(sla20k)[names(sla20k) == "Leaf_Area_cm2"] <-"Area_cm2"
names(sla21k)[names(sla21k) == "leaf_area_cm2"] <-"Area_cm2"
names(sla20k)[names(sla20k) == "Leaf_Length_cm"] <-"Length_cm"
names(sla21k)[names(sla21k) == "Average_Width_cm"] <-"Avg_Width_cm"
names(sla20k)[names(sla20k) == "Dried_Biomass_g"] <-"Mass_g"
names(sla21k)[names(sla21k) == "Dried_Biomass_g"] <-"Mass_g"

sla20k$Unique_Sample_code<-NULL
sla20k$Yellowing_of_Leaf_Assessment<-NULL
sla20k$Percent_of_Leaf_Surface_Area_Damaged<-NULL
sla20k$Dried_Biomass_Notes_from_Mark<-NULL
sla20k$mark_proofing_notes<-NULL
sla20k$SLA_cm2_per_gram<-NULL
sla20k$No_Sample_for_SLA<-NULL
sla21k$Notes<-NULL
sla21k$Specific_Leaf_Area_cm2_per_g<-NULL
sla20k$Site <- "kbs"
sla21k$Site <- "kbs"
sla20k$Julian <- NA
sla21k$Julian <- NA

# Common columns = Site Date Julian Plot Species Plant_Number Area_cm2 Mass_g
# Need to add some 
sla18u$Avg_Width_cm <- NA
sla18u$Length_cm <- NA
sla18u$Max_Width_cm <- NA
sla19u$Avg_Width_cm <- NA
sla19u$Length_cm <- NA
sla19u$Max_Width_cm <- NA
sla19k$Max_Width_cm <- NA
sla20u$Avg_Width_cm <- NA
sla20u$Length_cm <- NA
sla20u$Max_Width_cm <- NA
sla21k$Max_Width_cm <- NA

# Merge data
sla_final<-rbind(sla18u, sla19k, sla19u, sla20k, sla20u, sla21k)
# column names to lower case
names(sla_final) <- tolower(names(sla_final)) 
# remove zero values for Mass
sla_final<-sla_final[sla_final$mass_g != 0, ]
summary(sla_final) # 152 - 156 NA values for mass, area; omit them
dim(sla_final)
sla_final<-sla_final[rowSums(is.na(sla_final[ , 7:8])) == 0, ]
dim(sla_final)

str(sla_final)
# plant_number is character; also rename to a-e 
sla_final$plant_id<-sla_final$plant_number
sla_final["plant_id"][sla_final["plant_id"]=="1"]<-"a"
sla_final["plant_id"][sla_final["plant_id"]=="2"]<-"b"
sla_final["plant_id"][sla_final["plant_id"]=="3"]<-"c"
sla_final["plant_id"][sla_final["plant_id"]=="4"]<-"d"
sla_final["plant_id"][sla_final["plant_id"]=="5"]<-"e"

# Merge in plot ID info
sla_final <- merge(sla_final, meta, by = c("plot"))

# convert to day, month, year -- use POSIXlt 
sla_final$day<-strptime(sla_final$date, "%m/%d/%Y")$mday
# convert to day of year (Julian date)
sla_final$julian<-strptime(sla_final$date, "%m/%d/%Y")$yday+1
sla_final$month<-strptime(sla_final$date, "%m/%d/%Y")$mon+1
sla_final$year<-strptime(sla_final$date, "%m/%d/%Y")$year+1900

# 2018 is different: 2-digit year
yr18<-sla_final$year == 18
sla_final$year[yr18]<-strptime(sla_final$date, "%m/%d/%y")$year[yr18] + 1900

sla_final<-sla_final[with(sla_final, order(site, year, species)),]

# Compute SLA
sla_final$sla<-sla_final$area_cm2/sla_final$mass_g

# SLA values and mixed effects modeling in SLA_analyses_L2.Rmd indicate the following recording errors (based on outliertest()):
spla[1375,] 
spla[1406,]
# These are extremely small numbers for area relative to mass; also an order of magnitude lower than other area measurements in 2020.
spla[697,] # this data point is an order of magnitude higher for mass; likely recording error. Edited to appropriate magnitude in SLA_analyses_L2.Rmd.
spla[835,] # this data point is an order of magnitude lower for mass (0.0017) than similar ones for Popr with similar area; likely recording error. Edited to appropriate magnitude in SLA_analyses_L2.Rmd.

# For clarity with modeling, these outliers are removed in SLA_analyses_L2.Rmd. 
# sla was re-calculated for that data frame. File output = "./SLA/SLA_L1_nooutliers.csv"

# write a new csv with the cleaned and merged data and upload to the shared google drive L1 folder
write.csv(sla_final, file.path(L1_dir, "./SLA/SLA_L1.csv"), row.names=F)

## PLZ stopped updating 11/17/2021 HERE; waiting on CN data

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
umbs19_CN <- read.csv(file.path(L0_dir, "./UMBS/2019/umbs_CN_2019.csv"))
kbs19_CN <- read.csv(file.path(L0_dir, "./KBS/2019/kbs_CN_2019.csv"))

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
write.csv(CN_final, file.path(L1_dir, "./CN/CN_L1.csv"))
