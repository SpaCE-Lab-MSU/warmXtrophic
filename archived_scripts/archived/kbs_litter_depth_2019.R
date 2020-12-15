###################################
#look at litter depth data with Moriah and Mark
#created by Nina Lany 19-Apr-2019
###################################

#this script reads in the L0_data_entry gsheet for early spring 2019 KBS litter depoth data and makes a few figures.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
gs_ls()
drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0_data_entry")

#read in the data and rename columns so they are less clunky:
data <- gs_title("Litter Depth KBS OTC 2019")
dat <- as.data.frame(gs_read(ss = data, ws = "data entry"))
str(dat)

names(dat) <- c("Site", "Plot_Letter", "Plot_Number", "Plot_ID", "Warming", "Pesticide", "Fence", "Date", "Replicate", "Litter_Depth")

#check for equal sampling
tapply(dat$Litter_Depth, dat$Plot_ID, length)
boxplot(dat$Litter_Depth ~ dat$Warming, ylab = "Litter depth (cm)")

plot(jitter(as.numeric(as.factor(dat$Warming))), dat$Litter_Depth, xlim = c(0.5,2.5))

#calculate the mean and standard deviation for each plot:

