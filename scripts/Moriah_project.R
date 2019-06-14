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

#load L3 data
L3dat <- read_csv("https://drive.google.com/drive/u/0/folders/1yL4zUcsg6u9VD5BDuN_gqxtkRWLZaoQ-")
