###################################
#7. create L3 data
#created by Nina Lany 12-Jun-2019
###################################

#this script reads in the L2 data, summarizes it to annual values for each plot, and writes it out as a csv in the L3 directory on Google Drive.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

setwd("~/Documents/warmXtrophic")
#Check to make sure working directory is set to the github repo
if(basename(getwd())!="warmXtrophic"){cat("Plz change your working directory. It should be 'warmXtrophic'")}
#source script with useful functions
source("scripts/functions.R")

#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
#gs_ls()
#drive_find(n_max=10)

#import metadata on files in relevant Google Drive directories
L2_data_dir <- googledrive::drive_ls("~/warmXtrophic/data/L2")

#if you will be using Google Drive File Stream or Back Up and Sync (not the 'googledrive' and 'googlesheets' packages), set your working directory to the warmXtrophic folder on the shared Google Drive.
setwd("~/Google Drive File Stream/My Drive/warmXtrophic")


#######################################
#load the L2 data:
#get google_id for taxon table and load:
#google_id <- L2_data_dir %>% filter(grepl('taxon.csv',name))%>%
#		 select(id) %>% 
#		 unlist()
#taxa <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
taxa <- read.csv("data/L2/taxon.csv", stringsAsFactors=F)

#get google_id for plot table and load:
#google_id <- L2_data_dir %>% filter(grepl('plot.csv',name))%>%
#		 select(id) %>% 
#		 unlist()
#plots <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
plots <- read.csv("data/L2/plot.csv", stringsAsFactors=F)

#get google_id for event table and load:
#google_id <- L2_data_dir %>% filter(grepl('event',name))%>%
#		 select(id) %>% 
#		 unlist()
#events <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", google_id), stringsAsFactors=F)
events <- read.csv("data/L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")

pc <- read.csv("data/L2/observation_species.csv", stringsAsFactors=F)
phen <- read.csv("data/L2/observation_reproductive_phenology.csv", stringsAsFactors=F)


#calculate annual max cover for each species on each plot:
L3.pc <- pc %>%
	group_by(Site, Year, Plot, variable_name) %>%
	dplyr::summarize(Cover = max(value, na.omit=T)) %>% 
	ungroup()
	
#calculate annual index of greenup phenology for each species in each plot as the first day percent cover exceeded 1%.
L3.greenup <- pc %>%
		dplyr::filter(value > 1) %>%
		group_by(Site, Year, Plot, variable_name) %>%
		dplyr::summarize(Greenup = min(Julian_day, na.rm=T)) %>%
		ungroup()
		
#calculate annual first & last day of flowering, first day of seed for each species on each plot:
L3.first.flower <- phen %>%
	dplyr::filter(value == "flower") %>%
	group_by(Site, Year, Plot, variable_name) %>%
	dplyr::summarize(First.flower = min(Julian_day, na.rm=T)) %>% 
	ungroup() 
	
L3.last.flower <- phen %>%
	dplyr::filter(value == "flower") %>%
	group_by(Site, Year, Plot, variable_name) %>%
	dplyr::summarize(Last.flower = max(Julian_day, na.rm=T)) %>% 
	ungroup()
	
L3.first.seed <- phen %>%
	dplyr::filter(value == "seed") %>%
	group_by(Site, Year, Plot, variable_name) %>%
	dplyr::summarize(First.seed = min(Julian_day, na.rm=T)) %>% 
	ungroup()
	

L3 <- Reduce(function(x,y) merge(x,y, by = c("Site", "Year", "Plot", "variable_name"), all=T), list(L3.pc, L3.greenup, L3.first.flower, L3.last.flower, L3.first.seed))

#this does not mean that all the data in L3 "make sense". Need to look more closely. Some species snever exceeded 1% cover on a plot, other species are only on one or a few plots, non-taxa codes such as 'Litter' and 'Brown' are still included... apply more filters prior to analysis!

#merge in trait and treatment data
temp1 <- taxa[,c("code","origin", "duration", "growth_habit")]
L3 <- merge(L3, temp1, by.x = "variable_name", by.y="code", all.x=T, all.y=F)
temp2 <- plots[,c("plot","state", "insecticide")]
L3 <- merge(L3, temp2, by.x = "Plot", by.y="plot", all.x=T, all.y=F)
str(L3)

L3 <- L3[order(L3$Site, L3$Year, L3$Plot, L3$variable_name),]
L3 <- L3[,c("Site", "Year", "Plot", "variable_name", "Cover", "Greenup", "First.flower", "Last.flower", "First.seed", "origin", "duration", "growth_habit", "state", "insecticide")]

write.csv(L3, file = "~/Google Drive File Stream/My Drive/warmXtrophic/data/L3/L3dat.csv", row.names=F)

##old attempt to set greenup at day 10% of max biomass reached
#first remove non-taxa:
temp <- pc
colnames(temp) <- toupper(colnames(temp))
temp <- remove.non.taxa(temp)
		
#i = site
#t = year
#p = plot
#j = species
sites <- c('kbs', 'umbs')
quadrats <- unique(plots$plot)
for(i in 1:length(sites)){
	temp <- subset(temp, SITE == sites[i])
	years <- unique(temp$YEAR)
	for(t in 1:length(years)){
		temp <- subset(temp, YEAR == years[t])
		for(p in 1:length(quadrats)){
			temp <- subset(temp, PLOT == quadrats[p])
			species <- unique(temp$VARIABLE_NAME)
			for (j in 1:length(species)){
				temp <- subset(temp, VARIABLE_NAME == species[j])
				
			}
		}
	}
}		
		