#Moriah's independent work with R to understand and compute the data compiled from the warmxtrophic project
#Moriah Young created 06/14/2019

rm(list = ls())

#Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read gsheets into R directly from the internet (Google Drive) and authenticate with Google Drive
gs_ls()
drive_find(n_max=10)

#attempt to load L3 data
L3dat <- read_csv("https://drive.google.com/drive/u/0/folders/1yL4zUcsg6u9VD5BDuN_gqxtkRWLZaoQ-")
#parsing problems

#downloaded L3dat.csv onto personal computer from shared google drive and read in data this way - seemed to work, so I must be doing something from wrong with the above.
L3dat <- read.csv("~/Downloads/L3dat.csv")

#another attempt to load L3 data
library(googledrive)
drive_download("~/warmXtrophic/data/L3/L3dat.csv")
#can't identify file - wrong path?