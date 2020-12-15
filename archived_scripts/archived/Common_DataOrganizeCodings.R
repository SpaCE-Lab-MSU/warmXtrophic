# This is a common set of R commands which can be helpful for organizing
# datasets once they are read into R.  
# Compilation started 11/01/07 (!) by PLZ
# I continue to update it.

# Recently tidyr (http://tidyr.tidyverse.org/) has become a great resource as well.
# It has replaced "reshape2" package (formerly "reshape")

# Here are some similar helpful lists of commands:
# See bottom of page: http://science.nature.nps.gov/im/datamgmt/statistics/r/rcourse/session1.cfm
# R Reference cards: scroll down at this link to "reference cards": https://cran.r-project.org/other-docs.html

##### STARTING UP R
#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

#set working directory
#avoid spaces! use "_" or "." to separate words
#this is an example; you'll need to change it to your own path
setwd("c:/plz/DATA/FOR870/lab_data")

#check your working directory
getwd()

# See what files and directories are in your working directory
list.files()
list.dirs()

##### READING IN DATA

#Read in data tables - best to save your data in CSV format or tab-delimited (readable by many programs)
#Avoid naming a column the same name as the dataset name
# Read in csv file from the web:
fish <- read.csv("http://www.ats.ucla.edu/stat/data/fish.csv", strip.white=T,na.strings="na")
# Read in csv file from your computer with full pathname
fish <- read.csv("c:/plz/DATA/FOR870/lab_data/fish.csv", strip.white=T,na.strings="na")
# Read in csv file from your computer relative to your working directory
# ".." means go up to a higher directory "." means go down one directory
fish <- read.csv("./lab1_data/fish.csv", strip.white=T,na.strings="na")

# read in a tab-delimited file: the Labdsv data (bryce canyon vegetation)
veg<-read.table("http://ecology.msu.montana.edu/labdsv/R/labs/lab1/bryceveg.R",strip.white=T,na.strings="na", header=T)
site<-read.table("http://ecology.msu.montana.edu/labdsv/R/labs/lab2/brycesite.R",strip.white=T,na.strings="na", header=T)

##### INFO ABOUT OBJECTS
str(veg)     # gives breakdown of dataframe variables and their type (factor, integer,...)
summary(veg) # gives description of mean, quartiles, etc. for each variable
dim(site)    # dimensions: rows columns
head(site)   # take a look at the first 6 lines of your data

# Describing what each column in your data is; in this case change 
# several columns to factors (they were treated as integers)
# and some columns to numerical (they were factors)
# see also: http://www.r-bloggers.com/convert-factors-to-numbers/)
fish1<-fish # make a copy of fish
str(fish)
fish1 <- within(fish1, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
  xb <- as.numeric(levels(xb)[xb])
  zg <- as.numeric(levels(zg)[zg])
})
# See what this did
str(fish1)

#### Editing DATA
## Get rid of duplicate rows of both elev and slope 
dups <- duplicated(site[, c('elev', 'slope')])

# See the number of duplicates
sum(dups)

# keep the records that are _not_ duplicated- note that this picks the first
# instance of a duplicate - I would only recommend using this if you
# know for sure what rows you're removing
site1 <- site[!dups, ]
# Remove any duplicate rows in a dataset and make a new dataframe
site1<-unique(site1) # site1 doesn't have duplicate rows so this won't change anything

# remove records with longitude=0 ("slope" has to be a numeric, 
# otherwise use "0" if it is a factor or character)
site2 <- site[site$slope != 0, ]

# Remove rownames - sometimes you don't want row names
site3<-row.names(site)<-NULL

# Getting rid of objects: sometimes you load in lots of data from a .RData file
# but only want to keep a few things.
# Remove all but a few R objects that are list in c()
rm(list= ls()[!(ls() %in% c('veg','fish','site'))]) 

# reorder by column name (and in this case remove some columns by not listing them)
fish2 <- fish[c("xb", "zg", "nofish")]

#reorder by column index
fish3 <- fish[c(1, 3, 2)]

#need dplyr package
install.packages("dplyr")
library(dplyr)
fish4 <- fish2 %>% select(c(nofish:xb)) # where you list the first:last column


##### CHANGING NAMES & COLUMNS

#Convert text to upper case
site$pos <- toupper(as.character(site$pos))

#Change the name of the variable east to easting, north to northing
names(site)[names(site)=="east"]<-"easting"
names(site)[names(site)=="north"]<-"northing"

# Across all column headers, replace all instances of "t" with "X"
site1<-site # make a copy and work on it
names(site1) <- gsub("t", "X", names(site1))
#change it back
names(site1) <- gsub("X", "t", names(site1))

# Change the name of the variable to the first 4 letters
site1$depth <- substr(as.character(site1$depth), 1,4)

## Rename levels of factor
site1$depth<-as.factor(site1$depth)
#check their order first so you enter the same order
levels(site1$depth)=c("Deep", "No Data", "Shallow")

# Get rid of unused Factor levels (useful if you subset from your original dataset,
# and your new data only includes some of the factor levels, but all the factor levels
# are linked to the factor even in the reduced dataset. So this code reduces the levels
# to just those that are currently in the dataset)
site1$depth <- factor(site1$depth)

#### REPLACING VALUES WITH OTHER VALUES  ******* Note that the column has to be as.character
site1$depth[site1$depth == "No Data"] <- "Missing Data" 
# generates a warning; column needs to be a character
site1$depth<-as.character(site1$depth)
site1$depth[site1$depth == "No Data"] <- "Missing Data" 

# make sites above the median elevation have no elevation entry
summary(site1$elev)
site1$elev[site1$elev > 7880 ] <- NA

# Change a value in one column by referencing a value in another 
# (here where slope=0 flag it in another column to indicate that data needs checking)
# new column for data checking
site1$data_check<-"OK"
site1$slope == 0                  #where slope=0; this just generates a vector of TRUE or FALSE
sel<- site1$slope == 0            #this saves the logical vector for use later
sel                               #look at the test criteria
site1$data_check[sel] <- "check"  #Makes data_check column "check" when sel == "TRUE" 

#Or do the same as above but in 1 line
site1$data_check[site1$slope==0] <- "check"  

# Double selection:
# Change all values > 50 in columns 2 through 4 to 1
sel<-site1[,2:4] > 50
site1[,2:4][sel]<- 1


#### OMITTING OR ADDING ROWS 

# Delete some rows based on referencing values in columns (where != is "not equal to" and == is "equal to")
# keep only data that doesn't need checking, make a new dataset (site2) in the process
site2 <- site1[site1$data_check != "check",] 

# Get rid of records (!=) by referencing two or more columns 
# for site1, delete "pc" quad and keep "Shallow" depth)
site2<-site2[site2$depth == "Shallow" | site2$quad != "pc",]

# Keep records where 2 columns are equal, and where a 3rd has no missing data
# first omit records where a column (elev) has missing data
site2 <-subset(site2, is.na(site2$elev) == F)
# or run code below for same output as above
site2<-subset(site2, site2$elev != "NA",)
# Only keep the locations where aspect is equal to slope
site2 <-subset(site2, site2$asp == site2$slope)

# make NA entries blank
site1[is.na(site1)]<-""     #for character; should work on numeric as well
site1[is.na(site1)]<-NA     #for numeric; may work on character as well
# Replace blanks in a column with a value (zero in this case)
site1$elev[is.na(site1$elev)] <- 0

#### OMITTING OR ADDING COLUMNS

# Delete columns by subsetting (and keeping the columns that are selected)
site3<-subset(site2, select=c(plotcode, asp, elev, slope))

# Omit columns one by one 
site3$elev <-NULL

# Add a column and call everything in the column "yes"
site3$new.col <- "yes"

## Removing outliers - once you plot identify them
# Use identify  - click on the points, then escape key to exit
# in this example, we want to remove extreme cases of steep slope and high elevation
plot(site1$elev,site1$slope)
identify(site1$elev,site1$slope,labels=site1$plotcode) # labels will label a 3rd variable

# Read off the plot to remove these
site4<-site1 # make a copy to work on
dim(site4)
site4<-subset(site4, site4$plotcode != 50171)
site4<-subset(site4, site4$plotcode != 50075)
site4<-subset(site4, site4$plotcode != 50167)
dim(site4)
# Check it; removed 3 records
plot(site4$elev,site4$slope)

#### MERGING 

# Merge 2 dataframes together by matching their row names. Keeps only records where veg occur in a site 
# site=x, veg=y
vegsite<-merge(site,veg,by=c("row.names"),all.x=TRUE)
# to merge by referencing a common column (e.g. "column1"), use by=c("column1") in the above statement
# to keep all records, even if there is no veg in a given site, use "all.x=TRUE, all.y=TRUE"

#### CONCATENATING (stack rows if you have same columns in each dataset;
# make sure the structure of each dataset matches before doing this)

# Concatenate 1988 and 2006 cover data
cover <- rbind(cov.2006, cov.1988)

#### CHANGE TO WIDE FORMAT ** see also tidyr package: http://tidyr.tidyverse.org/
site.w<-reshape(site,
                        v.names="pos",    # the variable you want to transpose to wide format
                        idvar=c("elev"),  # your independent variable(s); careful because if you
                                          # keep the other columns in your dataset, it may be confusing 
                                          # how the other columns relate to these new columns
                        timevar="depth",  # what do you want to fill the column, "v.names" with?
                        direction="wide") # the direction (can also be long)
# the warnings just mean you're creating new variables that have different numbers of entries

#### CHANGE TO LONG FORMAT
# try "reshape" package [reshape is no longer supported; try tidyr package: http://tidyr.tidyverse.org/]

library(reshape)
site.m<-melt(site, id=c(1:3,6,8))                   # This maintains the columns 1:3, and 6 and 8 as independent variables, melting all the others
# note that all other columns will be transposed and their name will appear in "variable" with their value in "value"
# this command is most useful when you first remove the columns you just want to reshape.
# do as above, but just on 2 variables:
site.m<-subset(site,select=c(plotcode,pos,depth))
site.m<-melt(site.m, id=c(1))                       # This maintains the column 1 as independent variable
# change the name of a column headings
names(site.m)[names(site.m)=="value"]<-"category"

# do this again, but this time with the veg data
# make a column for the row.names
veg1 <- cbind(Row.Names = rownames(veg), veg)
veg.m<-melt(veg1,id=c("Row.Names"))               # This maintains the bcnp plot as the independent variable
names(veg.m)[names(veg.m)=="variable"]<-"species"
names(veg.m)[names(veg.m)=="value"]<-"count"


#### AGGREGATING BY A VARIABLE
# Get the summed abundance across all species for each plot
# Can also do this for FUN=mean, stderr, etc.
# rename Row.Names in the process, to "plot"
attach(veg.m) # enables you to work directly on this dataset without referencing columns by "veg.m$column1"
veg.sum<-aggregate(veg.m[c("count")],list(plot=Row.Names),FUN=sum, na.rm=T)
detach(veg.m)
names(veg.sum)[names(veg.sum)=="count"]<-"count.sum"

#### APPLY FUNCTION TO DATA
# the apply function applies a function to each column (if we specify 2 
# as the second argument, rows if we specify 1) in turn. 
# The function(x){x-mean(x)} says to subtract the mean value of each 
# column from every value in that column, and then divide by the 
# standard deviation of that column. 
veg.center <- apply(veg,2,function(x){(x-mean(x))/sd(x)})
# or, with labdsv: center=TRUE argument means to center the columns by their mean, 
# and the scale=TRUE means divide by the standard deviation
veg.center <- scale(veg,center=TRUE,scale=TRUE)

#### COMBINING VARIABLES MATHEMATICALLY
# Divide the cover by summed cover to get relativized cover
# merge the summed count back into the veg count data so we can calculate rel. abundance by species
# first make sure you can merge by the same column name
names(veg.m)[names(veg.m)=="Row.Names"]<-"plot"
veg2<-merge(veg.m,veg.sum,by=c("plot")) # this will only keep the rows that match by "plot" in each dataset
veg2$rel.abund <- veg2$count/veg2$count.sum

#Get the mean cover of vegetation species across the sites
attach(veg2)
relab.mn<-aggregate(veg2[c("rel.abund")],list(species=species),FUN=mean, na.rm=T)
detach(veg2)

#### EXPORTING DATA
# export to csv
write.csv(site1, file="site1.csv")
# save your workspace (and all objects in it)
save.image("work.RData")
#dump an object to a file
dump(c("site1"),"site1.q") 

