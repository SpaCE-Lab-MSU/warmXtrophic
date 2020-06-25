# Title: Looking at PAR data in relation to plant composition
# Date: 2020 June 19
# Author: Moriah Young

# Find the file you want to import into R =========================================================
# Ideally I would do this directly from Google FileStream (don't have on computer) or from the
# shared folder online, but for now I am downloading the desired files from the shared folder onto
# my desktop to be called into R

file.choose() #this will open up a window on your computer so that you can navigate to the file you 
# wish to download and it will give you the path to that file in the console. Copy and paste this 
# into the read.csv() command

# Start by looking at 2019 UMBS data so we will need to download Plant Comp and PAR data files 
# PAR was collected on 6/03, 6/18, 7/28
# Plant Comp collected on 6/4, 6/19, 7/12 - these dates are the closest dates to the PAR measurements taken
# from 2019


# Data import and set working directory =================================================== 
PAR2019 <- read.csv("/Users/moriahyoung/Downloads/umbs_par_2019.csv")
PC <- read.csv("/Users/moriahyoung/Downloads/umbs_plantcomp_2019.csv")

# Convert date column to date class ============================================================
<<<<<<< HEAD
PC$Date <- as.Date(PC$Date,
                   format = "%m/%d/%y")
class(PC$Date) # check 
=======
PC$Date <- as.Date(PlantComp$Date,
                          format = "%m/%d/%y")
>>>>>>> b65b544c4b8e7669281f3df1a432775d717071d6

# Filter through the PC data to select the dates that correspond with the dates that PAR was 
# recorded to create a new plant comp data frame with only those dates

<<<<<<< HEAD
PlantComp <- filter(PC, PC$Date == c("2020-06-04", "2020-06-19", "2020-07-12"))
=======
PlantComp <- filter(PC, Date == c("2020-06-04", "2020-06-19", "2020-07-12"))
>>>>>>> b65b544c4b8e7669281f3df1a432775d717071d6

# Compute percent cover data as absolute ===================================================

# Remove non-plant taxa
PlantDat <- PlantComp %>%
        dplyr::filter(Species != "Brown",  # Remove the non-taxa codes
                      Species != "Bare_Ground",
                      Species != "Unknown",
                      Species != "Litter",
                      Species != "Animal_Disturbance",
                      Species != "Vert_litter",
                      Species != "Herbicide")

# Calculate absolute cover for each plot per each date
# Create a new column called "Total_Cover" in a new data frame with just those absolute cover values
# for each plot
AbsoluteCover <- PlantDat %>%
        group_by(Site, Plot, Date, Species) %>%
        dplyr::summarize(Cover = sum(Cover, na.omit=T)) %>% 
        ungroup() #this doesn't work yet

view(AbsoluteCover)
<<<<<<< HEAD
=======


>>>>>>> b65b544c4b8e7669281f3df1a432775d717071d6
