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

CN_L1_save <- function(){
        LO_dir <- Sys.getenv("LODIR")
        CN_final2 <- CN_L0(L0_DIR)
        # test that CN_file2 has charactistics we expect
        
        L1_dir <- Sys.getenv("L1DIR")
        write.csv(CN_final2, file=file.path(L1DIR, "final_CN_L1.csv"), row.names=FALSE)
        return()
}
        


# Set working directory to Google Drive
# setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")
# CN_LO_CHECK<- function(L0_dir){
#         if(! file.exists(file.path(L0_dir, "CN_data/CN_WeighSheet_2_2019.csv") {
#            warning("can't find #2")
#            return(F)
#         }
#         
# }


#' clean up CN CSV files before merging
cn_csvdata_initial_prep<- function(cn_data){
        cn_data <- cn_data[-(1:2),] #get rid of the first 2 rows because....
        names(cn_data) <- cn_data[1,] #make the first row the column names
        cn_data <- cn_data[-1,] #get rid of the first row because it's now the column names
        cn_data <- cn_data[-(1:7),] # get rid of first 7 rows
        cn_data <- cn_data[c(3, 4, 10, 11)] # get rid of unwanted columns
        
        return(omit.na(cn_data))
}


CN_L1 <- function(L0_dir){
        CN1 <- read.csv(file.path(LO_dir,"CN_data/CN_WeighSheet_1_2019.csv"))
        
        CN2 <- read.csv("L0/CN_data/CN_WeighSheet_2_2019.csv")
        CN3 <- read.csv("L0/CN_data/CN_WeighSheet_3_2019.csv")
        CN4 <- read.csv("L0/CN_data/CN_WeighSheet_4_2019.csv")

# read in meta files for CN data
        umbs_CN <- read.csv("L0/UMBS/2019/umbs_CN_2019.csv")
        kbs_CN <- read.csv("L0/KBS/2019/kbs_CN_2019.csv")
        meta <- read.csv("L0/plot.csv")
        
        # Clean CN data
        CN1<- cn_csvdata_initial_prep(CN1)
        CN2<- cn_csvdata_initial_prep(CN2)
        CN3<- cn_csvdata_initial_prep(CN3)
        CN4<- cn_csvdata_initial_prep(CN4)
        
        # merge separate CN files into one data frame
        CN_all <- merge(CN1, CN2, all = TRUE)
        CN_all <- merge(CN_all, CN3, all = TRUE)
        CN_all <- merge(CN_all, CN4, all = TRUE)
        
        CN_all$Sample[CN_all$Sample == "Blind Standard"] <- NA
        CN_all <- na.omit(CN_all)
        #View(CN_all)
        
        # clean meta data
        umbs_CN <- umbs_CN[-c(7, 8, 9)] # get rid of unwanted columns
        kbs_CN <- kbs_CN[-c(7, 8, 9)] # get rid of unwanted columns
        
        meta_CN <- merge(umbs_CN, kbs_CN, all = TRUE)
        colnames(meta_CN) <- sub("Unique_number", "Sample", colnames(meta_CN))# merge two meta data files
        #View(meta_CN)
        
        # merge new CN data frame with 
        CN_final <- merge(meta_CN, CN_all, by = "Sample")
        names(CN_final) <- tolower(names(CN_final)) # column names to lower case
        #CN_final <- clean_names(CN_final) # get rid of space and parenthesis in "weight (mg)" column
        # ^ don't have the function for this
        
        # change column name for merging
        colnames(meta)[which(names(meta) == "treatment_key")] <- "treatment"
        CN_final2 <- merge(CN_final, meta, by = c("plot", "treatment"))
        
        return(CN_final2)
        #View(CN_final)

}

# write a new cvs with the cleaned and merge data and upload to the shared google drive L1 folder
