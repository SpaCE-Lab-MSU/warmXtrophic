# TITLE:            HOBO L1 pendant data merge
# AUTHORS:          Kara Dobson
# COLLABORATORS:    Phoebe Zarnetske, Nina Lany, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young, Kathryn Schmidt
# DATA INPUT:       Data imported from .RData in L1 HOBO_data folder
# DATA OUTPUT:      Makes the following datasets in the L1 HOBO_pendant_data folder:
## KBS_HOBOpendant_L1:        KBS HOBO pendant data from 2015-2020
## UMBS_HOBOpendant_L1:       UMBS HOBO pendant data from 2015-2020
# PROJECT:          warmXtrophic
# DATE:             July, 2020

# NOTE: Must run HOBO_pendantdata_cleanup_L1 first

# clear environment
rm(list = ls())

# Load packages
library(tidyverse)

# Get data
Sys.getenv("L1DIR")
L1_dir<-Sys.getenv("L1DIR")
list.files(L1_dir)

# Load in .RData
load(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.RData"))
load(file.path(L1_dir,"HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.RData"))

# Merge KBS HOBO data from all years
diff1718k <- anti_join(pend18k, pend17k, by = "Date_Time")
diff1819k <- anti_join(pend19k, pend18k, by = "Date_Time")
diff1920k <- anti_join(pend20k, pend19k, by = "Date_Time")
diff2021k <- anti_join(pend21k, pend20k, by = "Date_Time")

pendk <- rbind(pend17k, diff1718k, diff1819k, diff1920k, diff2021k)
write.csv(pendk, file.path(L1_dir,"HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.csv"))

# Merge UMBS HOBO data from all years
diff1718u <- anti_join(pend18u, pend17u, by = "Date_Time")
diff1819u <- anti_join(pend19u, pend18u, by = "Date_Time")
diff1920u <- anti_join(pend20u, pend19u, by = "Date_Time")
diff2021u <- anti_join(pend21u, pend20u, by = "Date_Time")

pendu <- rbind(pend17u, diff1718u, diff1819u, diff1920u, diff2021u)
write.csv(pendu, file.path(L1_dir,"HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.csv"))
