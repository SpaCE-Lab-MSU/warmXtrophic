##Use data from this experiment to become more fluent with R!
#Nina Lany  created 04/19/2019

#this script reads in the L0_data_entry gsheet for UMBS 2019 and begins data checks.

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'googledrive', 'googlesheets')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


###These steps set you up to read gsheets into R directly from the internet (Google Drive):
#authenticate with Google Drive. A browser window may pop up and prompt you to allow the 'googledrive' or 'googlesheets' packages to access Google Drive from R.
#gs_ls()
#drive_find(n_max=10)
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
events <- read.csv("~/Google Drive File Stream/My Drive/warmXtrophic/data/L2/event.csv", stringsAsFactors=F)
events$Date <- as.Date(events$Date, format = "%m/%d/%y")


#read in the gsheet
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


###############################################
# GREENUP PHENOLOGY (percent cover over time)
#################################################

#PLOT GREENUP PHENOLOGY TIME SERIES FOR A SPECIFIC SPECIES
#this is the function
plot_cover_ts <- function (dat, site, species, year){
	newdat <- merge(dat, plots, by.x = "Plot", by.y = "plot")
	temp1 <- subset(newdat, Site == site & Species == species & Year == year)
	plot(temp1$Julian_day, temp1$Cover, type = "n", xlim = c(min(temp1$Julian_day), max(temp1$Julian_day)), ylim = c(0,max(temp1$Cover)), xlab = "Julian day", ylab = "Percent cover", main = paste(site, " - ", species," - ", year)) #this makes a blank plot

plot_id <- as.vector(unique(temp1$Plot))
for(p in 1:length(plot_id)){ #fil in the plot with a line for each plot
	temp2 <- subset(temp1, Plot == plot_id[p])
	temp2 <- temp2[order(temp2$Julian_day),] #make sure dataframe is ordered by date
	color_key <- ifelse(unique(temp2$state)=="warmed", "red", "blue") #color codeaccording to treatment
	lines(temp2$Julian_day, temp2$Cover, col = color_key, type = "o")
	}
	legend("topleft", col = c("blue", "red"), legend = c("ambient", "warmed"), pch = 1, lty=1) #add a legend only once	
}

#'call' the function like this:
plot_cover_ts(dat, site = "umbs", species = "Cest", year = "2019")

#look at nine most abundant species...
focal.taxa <- c("Cest", "Dasp","Frve","Popr", "Ruac","Hype", "Hiau", "Umsp", "Ptaq")
par(mfrow = c(3,3))
for (i in 1:length(focal.taxa)){
	plot_cover_ts(dat, site = "umbs", species = focal.taxa[i], year = "2019")
}


####################################
# COVER (warmed vs. ambient, by traits)
####################################
#Do warmed plots have more plant cover than ambient plots, and is there an interaction such that the increase in cover is greater for exotic vs. native species?

#remove non-plant taxa
  plantDat <- dat %>%
  dplyr::filter(Species != "Brown",  # Remove the non-taxa codes
                Species != "Bare_Ground",
                Species != "Unknown",
                Species != "Litter",
                Species != "Animal_Disturbance",
                Species != "Vert_litter",
                Species != "Herbicide")

#calculate annual max cover for each species on each plot:
ac <- plantDat %>%
	group_by(Site, Plot, Species) %>%
	dplyr::summarize(Cover = max(Cover, na.omit=T)) %>% 
	ungroup()

#merge in ancillary data on plant species traits:
#allDat <- merge(ac, plots, by.x = "Plot", by.y = "plot")
allDat <- merge(ac, taxa, by.x = "Species", by.y = "code", all.x=F, all.y=F)

#aggregate total cover by origin
cov <- as.data.frame(tapply(allDat$Cover, list(allDat$Plot, allDat$origin), sum))
cov$Plot <- rownames(cov)
str(cov)
cov <- cov[,c("Plot", "Exotic", "Native")] #remove taxa that are 'Both' exotic and native
#gather into long format:
cov <- cov %>%
	gather(key = origin, value=Cover, -Plot)
#merge in ancillary plot data:
cov <- merge(cov, plots, by.x = "Plot", by.y = "plot")

#make barplot
#calculate mean of each group:
means <- tapply(cov$Cover, list(cov$origin, cov$state), mean)
#calculate standard deviation of each group:
sds <- tapply(cov$Cover, list(cov$origin, cov$state), sd)
#calculate sample size of each group:
n <- tapply(cov$Cover, list(cov$origin, cov$state), length)
#calcualte standard errors by dividing the sd by square root of n 
se <- sds/sqrt(n)
#make barplot
barCenters <- barplot(means, beside=T, ylim = c(0,60),  cex.names=1.5, cex.axis = 1.5, ylab = "Percent cover", cex.lab = 1.5, legend.text = T, args.legend = list(x = "topleft"))
#draw bars representing twice the standard error (an approximation of the 95% confidence interval)
arrows(barCenters, means+se*2, barCenters, means - se*2, angle=90, code=3, lwd = 1.5,length = 0.1)

#check statistical assumptions
hist(cov$Cover)  ##one large outlier
which(cov$Cover > 100)
cov[15,] #it's native cover on a warmed plot, and is responsible for the large error bar for that group. But I don't see any justification for removing it.
hist(residuals(aov(cov$Cover ~ cov$state*cov$origin)))

#run 2-way ANOVA with interactions.If the interactin is significant, the response to warming is different for native vs. exotic cover.
summary(aov(cov$Cover ~ cov$state*cov$origin))  #no difference, even with the outlier.


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
	col = colors,bty="n",
	xlim = c(125, 180)
	)
	
axis(side = 2, at = c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labels = unique(figureDat$Species))

abline(h = c(2.5, 4.5, 6.5, 8.5, 10.5), lty=2)

arrows(figureDat$mean.First.flower - 2*figureDat$se.First.flower, as.numeric(as.factor(uniqueID)),figureDat$mean.First.flower + 2*figureDat$se.First.flower ,as.numeric(as.factor(uniqueID)), code = 3, angle = 90, length = 0.05, lwd = 1.5, col = colors)

text(175, as.numeric(as.factor(uniqueID)), labels = paste0("(n = ", figureDat$n.First.flower,")"), font = 3)