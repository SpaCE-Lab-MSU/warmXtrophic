# TITLE:            HOBO L1 pendant data merge
# AUTHORS:          Kara Dobson
# COLLABORATORS:    Phoebe Zarnetske, Nina Lany, Mark Hammond, Pat Bills, Kileigh Welshofer, Moriah Young, Kathryn Schmidt
# DATA INPUT:       Data imported from .RData in L1 HOBO_data folder
# DATA OUTPUT:      Makes the following datasets in the L1 HOBO_pendant_data folder:
## KBS_HOBOpendant_L1:        KBS HOBO pendant data from 2015-2020
## UMBS_HOBOpendant_L1:       UMBS HOBO pendant data from 2015-2020
# PROJECT:          warmXtrophic
# DATE:             July, 2020

# Load packages
for (package in c("tidyverse")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

# Set working directory to Google Drive
# **** Update with the path to your Google drive on your computer
setwd("/Volumes/GoogleDrive/Shared drives/SpaCE_Lab_warmXtrophic/data/")

# Load in .RData
load("L1/HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.RData")
load("L1/HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.RData")

# Merge KBS HOBO data from all years
diff1718k <- anti_join(pend18k, pend17k, by = "Date_Time")
diff1819k <- anti_join(pend19k, pend18k, by = "Date_Time")
diff1920k <- anti_join(pend20k, pend19k, by = "Date_Time")

pendk <- rbind(pend17k, diff1718k, diff1819k, diff1920k)
write.csv(pendk, file="L1/HOBO_data/HOBO_pendant_data/KBS/KBS_HOBOpendant_L1.csv")

# Merge UMBS HOBO data from all years
diff1718u <- anti_join(pend18u, pend17u, by = "Date_Time")
diff1819u <- anti_join(pend19u, pend18u, by = "Date_Time")
diff1920u <- anti_join(pend20u, pend19u, by = "Date_Time")

pendu <- rbind(pend17u, diff1718u, diff1819u, diff1920u)
write.csv(pendu, file="L1/HOBO_data/HOBO_pendant_data/UMBS/UMBS_HOBOpendant_L1.csv")
