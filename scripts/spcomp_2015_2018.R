## TITLE:         MI Trophic Warming Field Experiment 2015-2018 Data Prep
## AUTHORS:       Phoebe Zarnetske (PLZ), Kathryn Schmidt (KS) 
## COLLABORATORS: Mark Hammond, Nina Lany
## DATA:          MI Warming x Herbivory Open Top Chamber experiment 2015-2018
## PROJECT:       "Direct and Indirect Effects of Climate Warming on Plant Communities"
## OUTPUT:        creates species composition data per plot and year
## DATE:          July 27 PLZ
## LAST RUN:      Aug 14 KS

## This script reads in the species composition data and generates plots
## Uses following dataset
## spcomp1518:    Plant community composition data for 2015, 2016, 2017, 2018 across KBS and UMBS

# Clear all existing data
rm(list=ls())

# Close graphics devices
graphics.off()

# set working directory (if you're not PLZ, change this to the correct path for your
# Google Drive directory. It should point to where we have /final_data
# setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/final_data/")
setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/final_data/")

## Edit below for any packages you'll be using
for (package in c("ggplot2","dplyr","tidyr","vegan")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages("package")
    library(package, character.only=T)
  }
}

# This code for ggplot2 sets the theme to mostly black and white 
# (Arial font, and large font, base size=24)
theme_set(theme_bw(12))
theme_update(axis.text.x = element_text(size = 10, angle = 90),
             axis.text.y = element_text(size = 10))

# function to calculate standard errors
stderr <- function(x) sqrt(var(x)/length(x))

#************************
####** DATA IMPORT **####
#************************

# Uncomment this if you want to read in the saved workspace
#load("spcomp_2015_2018.RData")

## Occasionally add this line to save your workspace.
save.image("spcomp_2015_2018.RData")

# read in data
spcomp<-read.csv("spcomp1518.csv")

#look at data
unique(sort(spcomp$species))

#Edit species names . standardize so there is 1
#name per species. Reference "plant_codes.csv" in the /data/raw_data/ folder
spcomp$species <- as.character(spcomp$species)
spcomp$species[spcomp$species == "Anspp"] <- "Ansp"
spcomp$species[spcomp$species == "Elrre"] <- "Elre"
spcomp$species[spcomp$species == "Bare"] <- "Bare_ground"
spcomp$species[spcomp$species == "Bare Groud"] <- "Bare_ground"
spcomp$species[spcomp$species == "Bare Groud "] <- "Bare_ground"
spcomp$species[spcomp$species == "Bare Ground "] <- "Bare_ground"
spcomp$species[spcomp$species == "Bare Ground"] <- "Bare_ground"
spcomp$species[spcomp$species == "Bareground"] <- "Bare_ground"
spcomp$species[spcomp$species == "Bown"] <- "Brown"
spcomp$species[spcomp$species == "Brown "] <- "Brown"
spcomp$species[spcomp$species == "Litter"] <- "Brown"
spcomp$species[spcomp$species == "Ramu"] <- "Romu"

#remove "Brown" and "Bare_ground" from species
spcomp<- spcomp[spcomp$species != "Bare_ground",]
spcomp<- spcomp[spcomp$species != "Brown",]
unique(sort(spcomp$species))

# where is the "Unknown" and is it unique? 
#KBS, D1, 2017. Yes. (ask Mark to resolve if possible)
whereisit <- spcomp %>%
  filter(species == "Unknown")

#plot KBS and UMBS data to see if there are inconsistencies in spp. list across years
KBS.spcomp<-spcomp[(spcomp$site=="KBS"),]
UMBS.spcomp<-spcomp[(spcomp$site=="UMBS"),]

# comparing years for KBS
KBS.2015.spcomp<-KBS.spcomp[(KBS.spcomp$year=="2015"),]
KBS.2016.spcomp<-KBS.spcomp[(KBS.spcomp$year=="2016"),]
KBS.2017.spcomp<-KBS.spcomp[(KBS.spcomp$year=="2017"),]
A = KBS.2015.spcomp$species
B = KBS.2016.spcomp$species
C = KBS.2017.spcomp$species
A %in% B
intersect(A,B) # data that appears in both A and B
setdiff(A,B) # data that appears in A, but not in B
#[1] "Piau"  "Posp"  "Pesp"  "Trsp"  "Thar"  "Prsp"  "Acsa"  "Crsp"  "Juni"  "Posp2"
#[11] "Uhsp"  "Posp3" "Erci"  "Ersp"  "Soju"  "Sypi" 
setdiff(B,A)
# [1] "Cahi" "Alpe"
setdiff(B,C)
#[1] "Hisp" "Trpr" "Trre" "Vear" "Alpe" "Ceor"
setdiff(C,B)
#[1] "Hipr"    "Unknown" "Brin"    "Desp"    "Cesp"    "Assy"   

# comparing years for UMBS
UMBS.2015.spcomp<-UMBS.spcomp[(UMBS.spcomp$year=="2015"),]
UMBS.2016.spcomp<-UMBS.spcomp[(UMBS.spcomp$year=="2016"),]
UMBS.2017.spcomp<-UMBS.spcomp[(UMBS.spcomp$year=="2017"),]
D = UMBS.2015.spcomp$species
E = UMBS.2016.spcomp$species
F = UMBS.2017.spcomp$species
D %in% E
intersect(D,E) # data that appears in both D and E
setdiff(D,E) # data that appears in D, but not in E
#[1] "Hica" "Spsp" "Prpe" "Ulsp" ("Ulsp"= unknwon lichen sp., "Spsp"= Sporobolus species)
setdiff(E,D)
# [1] "Trdu" "Hipi"
setdiff(E,F)
#[1] "Trdu" "Hipi" "Quru"
setdiff(F,E)
#[1] "Hica"      "Vear"      "Anma"      "Prpe"      "Elre"      "Unknown 5"
#[7] "Sohi"      "Oebi"  
# KS Comments: Sohi might be Sone, only recorded as such in 2017. Plot A1. 
# Hica identified as Hipi in 2016 only. Mark confirming proper ID. 

#Plot it: look at C3 compositional shifts over time
UMBS.C3.spcomp<-UMBS.spcomp[(UMBS.spcomp$plot=="C3"),]
ggplot(UMBS.C3.spcomp, aes(x = julian, y = cover, color = species)) +
  facet_grid(. ~ year) +
  geom_point(size=1) +
  geom_line(aes(group = species)) +
  ylab("Percent cover") +
  theme_minimal() + theme(legend.position = "bottom")

####### JASON HELP NEEDED #######
# Rename species ID based off several conditions (specifically, rename "Prpe" to "Amla" in year 2015, at UMBS, in plot C3)
# make a new dataframe just in case this doesn't work, so you don't overwrite it (remove this step if it does work)
spcomp1<-spcomp 
dim(spcomp) # print number of rows & columns 
head(spcomp) # print first 6 rows
summary(spcomp) # print the summary details
# make a selection based on several columns (by using | )
spcomp$species[spcomp$site == "UMBS" | spcomp$year == 2015 | spcomp$plot =="C3" | spcomp$species == "Prpe",] <-"Amla"
dim(spcomp) # check number of rows & columns - do they match the output above in line 141?
head(spcomp) # check first 6 rows - do they match the output above in line 142?
summary(spcomp) # check the summary details - do they match the output above in line 143?
# If spcomp changed in the wrong way, then this doesn't work. If it worked then you don't need to use spcomp1 to rerun/edit

# You could also try using "within"
spcomp <- within(spcomp, species[species == 'Prpe' & site == 'UMBS' & year==2015, plot== "C3"] <- 'Amla')

# You could also try using dplyr:
spcomp %>%
     mutate(species=replace(species, species=="Prpe", site=="UMBS", year==2015, plot== "C3", "Amla")) %>%
     as.data.frame()

## Old code
#edits.UMBS <- spcomp %>%
#  filter(site == "UMBS", 
#         year == 2015,
#         plot == "C3")  %>% 
#  mutate(species = ifelse(species == "Prpe", "Amla", species))

#convert cover to relative abundance 
#first get summed cover for all plants per plot
spcomp$date<-NULL
cov.sum = aggregate(cover ~ plot*year*julian*site, data=spcomp, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="cover"]<-"cov.sum"
head(cov.sum)
spcomp1<-merge(spcomp,cov.sum, by=c("plot","julian","year","site"))

#merge in species trait info - if some species have no entry, add to the original
#trait lookup tables here. Use USDA plants/ Grime classification for growth forms
ktrait<-read.csv("../raw_data/KBS_Species_Traits_Comp.csv")
utrait<-read.csv("../raw_data/UMBS_Species_Traits_Comp.csv")
str(utrait)
str(ktrait)
uktrait<-rbind(utrait,ktrait)
spcomp1<-merge(spcomp1, uktrait, by=c("species"))
head(spcomp1)
#spcomp1<-merge(spcomp1, ktrait, by=c("species"), all.x=T, all.y=T)
#spcomp1<-merge(spcomp1, utrait, by=c("species"), all.x=T, all.y=T)

save.image("spcomp_2015_2018.RData")

#merge in plot info (if not already included)
trt<-read.csv("../raw_data/Treatment_key_updated.csv", header = TRUE)
str(trt)
spcomp1<-merge(spcomp1, trt, by=c("plot"), all.x=T, all.y=T)

#calculate relative percent cover PER SPECIES in each quadrat (="relative abundance")
spcomp1$relab<-spcomp1$cover/spcomp1$cov.sum
summary(spcomp1)
# check that above code worked... haven't run w new 2018 data


# to compute the relative abundance PER NATIVE vs. EXOTIC per plot, need to re-run the
# cov.sum analysis above, but include a column for native/exotic and aggregate
# to include that column as well... This will generate a new dataframe. Then run the relab calculation above 
# on that new native/exotic % cover dataframe.
# merge in species trait info
spcomp2<-merge(spcomp, uktrait, by=c("species"))
cov.sum2 = aggregate(cover ~ plot*year*julian*site*origin, data=spcomp2, FUN=sum, na.rm=T)
names(cov.sum2)[names(cov.sum2)=="cover"]<-"cov.sum"
head(cov.sum2)
spcomp2<-merge(spcomp2,cov.sum2, by=c("plot","julian","year","site","origin"))

#merge in plot info 
spcomp2<-merge(spcomp2, trt, by=c("plot"), all.x=T, all.y=T)

#calculate relative percent cover PER NATIVE VS. EXOTIC in each quadrat (="relative abundance")
spcomp2$relab<-spcomp2$cover/spcomp2$cov.sum
summary(spcomp2)

## Feel free to make some plots by summarizing relab by year, species, native/exotic, site.
## Plot is usually experimental unit, so when plotting take mean across plots (if there is 
# one value per plot).
# The script, "2016_SpComp_finaldata_by_site.R" has several analyses and 
# relative abundance plots that could be added below and run here to look at 
# differences over years.

# plot it:
ggplot(spcomp1, aes(x = julian, y = relab, color = origin)) +
  facet_grid(state ~ year) +
  geom_point(size=1) +
  #geom_line(aes(group = species)) +
  ylab("Percent cover") +
  theme_minimal() + theme(legend.position = "bottom")

# looking at Cest in ambient vs. warmed plots
spcomp2.cest<-spcomp2[(spcomp2$species=="Cest"),]
ggplot(spcomp2.cest, aes(x = julian, y = cover, color = year)) +
  facet_grid(state ~ site) +
  geom_point(size=1) +
  #geom_line(aes(group = species)) +
  ylab("Percent cover") +
  theme_minimal() + theme(legend.position = "bottom")

library(vegan)
library(MASS)
> vare.dis <- vegdist(varespec)
> vare.mds0 <- isoMDS(vare.dis)
