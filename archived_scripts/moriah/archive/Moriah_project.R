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


###These steps set you up to read gsheets into R directly from the internet (Google Drive):
#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
gs_ls()
drive_find(n_max=10)
#import metadata on files in relevant Google Drive directories
L0_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L0_data_entry")
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#load the taxon and plot lookup tables to we can cross-reference with them:
#get google_id for taxon table and load:
google_id <- L2_data_dir %>% filter(grepl('taxon.csv',name))%>%
  select(id) %>% 
  unlist()
taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
#taxa <- read.csv("data/L2/taxon.csv")

#get google_id for plot table and load:
google_id <- L2_data_dir %>% filter(grepl('plot.csv',name))%>%
  select(id) %>% 
  unlist()
plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)

#get google_id for event table and load:
google_id <- L2_data_dir %>% filter(grepl('event',name))%>%
  select(id) %>% 
  unlist()
events <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

#read in the gsheet for plant composition
#data check for 2019 plant composition data
data <- gs_title("umbs_plantcomp_2019")
dat <- as.data.frame(gs_read(data))

#look at the dataframe:
str(dat)
unique(dat$Site)
unique(dat$Date)
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y") #format the date
#are all sampling events on the event lookup table?
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")
dat$Julian_day <- as.numeric(format(dat$Date,"%j")) #convert the Date to a decimal number 001-36 for day of the year. This helps with plotting, especially when combining different years.
dat$Year <- 2019

#look at plot codes:
unique(dat$Plot)
setdiff(unique(dat$Plot), unique(plots$plot)) 

#compare species codes with taxon lookup table
unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code)) #these are the codes in the gsheet that are not on the taxon table.


####################################
# REPRODUCTIVE PHENOLOGY
####################################
data <- gs_title("umbs_flwr_sd_2019")
dat <- as.data.frame(gs_read(data))

str(dat)
colnames(dat)[which(names(dat) == "Action")] <- "Event"

unique(dat$Site)

dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
unique(dat$Date)
as.Date(setdiff(unique(dat$Date), unique(events$Date)), origin = "1970-01-01")
dat$Julian_day <- format(dat$Date,"%j") #convert the Date to a decimal number 001-36 for day of the year
dat$Julian_day <- as.numeric(dat$Julian_day)

#compare plot codes with plot lookup table:
setdiff(unique(dat$Plot), unique(plots$plot))
#compare species codes with taxon lookup table
unique(dat$Species)
setdiff(unique(dat$Species), unique(taxa$code))

#the only allowed events are "flower" and "seed"
unique(dat$Event)
dat$Year <- 2019
#select certain columns and rename to standard colnames:
dat <- dat[,c("Site","Year","Date", "Julian_day","Plot", "Species", "Event")]

str(dat)

#calculate annual first & last day of flowering, first day of seed for each species on each plot:
first.flower <- dat %>%
  dplyr::filter(Event == "flower") %>%
  group_by(Site, Plot, Species) %>%
  dplyr::summarize(First.flower = min(Julian_day, na.rm=T)) %>% 
  ungroup() 

last.flower <- dat %>%
  dplyr::filter(Event == "flower") %>%
  group_by(Site, Plot, Species) %>%
  dplyr::summarize(Last.flower = max(Julian_day, na.rm=T)) %>% 
  ungroup()

first.seed <- dat %>%
  dplyr::filter(Event == "seed") %>%
  group_by(Site, Plot, Species) %>%
  dplyr::summarize(First.seed = min(Julian_day, na.rm=T)) %>% 
  ungroup()


L3dat <- Reduce(function(x,y) merge(x,y, by = c("Site", "Plot", "Species"), all=T), list(first.flower, last.flower, first.seed))

#merge in ancillary info on plot trmt and species origin:
L3dat <- merge(L3dat, plots, by.x = "Plot", by.y = "plot", all.x=F, all.y=F)
L3dat <- merge(L3dat, taxa, by.x = "Species", by.y = "code", all.x=F, all.y=F)
str(L3dat)

#make a figure that compares first flowering dates for warmed vs. ambient:
tempDat <- L3dat[,c("Species", "First.flower", "state", "origin")]

meanfun <- function(x) {mean(x, na.rm=T)}
sdfun <- function(x) {sd(x, na.rm=T)}
lenfun <- function(x) {length(na.omit(x))}

#calculate mean of each group:
means <- as.data.frame(tapply(tempDat$First.flower, list(tempDat$Species, tempDat$state), meanfun))
#calculate standard deviation of each group:
sds <- as.data.frame(tapply(tempDat$First.flower, list(tempDat$Species, tempDat$state), sdfun))
#calculate sample size of each group:
n <- as.data.frame(tapply(tempDat$First.flower, list(tempDat$Species, tempDat$state), lenfun))
#calculate standard errors by dividing the sd by square root of n 
se <- sds/sqrt(n)

#gather into long format:
means$Species <- rownames(means)
se$Species <- rownames(se)
n$Species <- rownames(n)
means <- means %>%
  gather(key = state, value=mean.First.flower, -Species)
ses <- se %>%
  gather(key = state, value=se.First.flower, -Species)
n <- n %>%
  gather(key = state, value=n.First.flower, -Species)


figureDat <- Reduce(function(x,y) merge(x,y, by = c("Species", "state")), list(means, ses, n))

#make figure
colors <- ifelse(figureDat$state == 'warmed', 'red', 'blue')
uniqueID <- paste(figureDat$Species, figureDat$state, sep = "_")

plot(figureDat$mean.First.flower,
     as.numeric(as.factor(uniqueID)), 
     pch=19,
     yaxt = 'n',
     ylab = "Species",
     xlab= "Julian Day of first flower",
     main = "Mean Date of First Flower\nAmbient vs Warmed",
     col = colors,bty="n",
     xlim = c(125, 205)
)

box()

axis(side = 2, at = c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5, 13.5, 15.5, 17.5, 19.5, 21.5, 23.5, 25.5, 27.5, 29.5), labels = unique(figureDat$Species))

abline(h = c(2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5, 16.5, 18.5, 20.5, 22.5, 24.5, 26.5, 28.5, 30.5), lty=2)

arrows(figureDat$mean.First.flower - 2*figureDat$se.First.flower, as.numeric(as.factor(uniqueID)),figureDat$mean.First.flower + 2*figureDat$se.First.flower, as.numeric(as.factor(uniqueID)), code = 3, angle = 90, length = 0.05, lwd = 1.5, col = colors)

legend()

text(175, as.numeric(as.factor(uniqueID)), labels = paste0("(n = ", figureDat$n.First.flower,")"), font = 3)


#Below I'm trying to work with the L3 data file
library(googledrive)

#this sets the working directory onto the laptop on which you are using
setwd("~/Desktop")

#this downloads the desired dataset from google drive onto your local computer and saves it to the working directory above
drive_download("L3dat.csv", path = 'L3dat.csv', type = "csv", overwrite = TRUE)

#this reads in the data to R and allows you to see the data file
data <- read.csv('L3dat.csv')
View(data)

#subsetting data from L3 looking
ambient_native <- subset(data, Site == "umbs" & state == "ambient" & origin == "Native")
warmed_native <- subset(data, Site == "umbs" & state == "warmed" & origin == "Native")
ambient_exotic <- subset(data, Site == "umbs" & state == "ambient" & origin == "Exotic")
warmed_exotic <- subset (data, Site == "umbs" & state == "warmed" & origin == "Exotic")

#looking at the mean of first flower for NATIVE species in ambient and warmed plots over all years using the subset data from above
mean(ambient_native$First.flower, na.rm=TRUE)
mean(warmed_native$First.flower, na.rm=TRUE)

#looking at the mean of first flower for EXOTIC species in ambient and warmed plots over all years using the subset data from above
mean(ambient_exotic$First.flower, na.rm=TRUE)
mean(warmed_exotic$First.flower, na.rm=TRUE)
