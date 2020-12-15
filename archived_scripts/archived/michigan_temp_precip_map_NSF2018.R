# Create map of Michigan average temperatures & precip from PRISM data;
# Plot Field sites on map.
# Edited 15 July 2018 PLZ

#Clear all existing data
rm(list=ls())

#Close graphics devices
graphics.off()

#set working directory
#setwd("/Users/plz/DATA/Species_ClimateChange/TrophicWarming/NSFPreProposal/")
setwd("/Volumes/plz-lab/DATA/MIGrass/final_data/MI_map/")

#install.packages('rgdal') # install rgdal if not already installed
library(rgdal)
library(raster)
library(maptools)
library(sp)

## Load workspace
load("michigan_temp_precip_map.RData")
# Update map locations for just KBS & UMBS & MSU

#temps <- raster("mi_temps_norm1.asc") 
precip <- raster("../../../ClimateData/PRISM_ppt_30yr_normal_800mM2_all_asc/PRISM_ppt_30yr_normal_800mM2_annual_asc.asc")
plot(precip)
temp<-raster("../../../ClimateData/PRISM_tmean_30yr_normal_800mM2_all_asc/PRISM_tmean_30yr_normal_800mM2_annual_asc.asc")
plot(temp)

save.image("michigan_temp_precip_map.RData")

# nice! So just find a michigan state map:
# Here's a quick way to get administrative boundaries of US:
us <- getData("GADM", country="USA", level=1)
# Extract Michigan: but this is really coarse and includes
# Great Lakes boundaries as straight lines
mi<-c("Michigan")
mi = us[match(toupper(mi),toupper(us$NAME_1)),]

# Alternatively, read in the state below as a standalone shapefile
mi = readOGR(dsn="../../../MIGIS/mi_boundary", 
             layer="michigan_boundary1") 

# Read in the MI cities
#city= readOGR(dsn="../../../MIGIS/city_mi", 
#             layer="city_miv14a") 
city= readOGR(dsn="../../../USGIS/ci08au12", 
              layer="ci08au12") 
micity = city[city$STATE=="MICHIGAN",]

# Try getting more detailed city & town data
# tl_2011_26_place

plot(mi, axes=FALSE)
plot(temp, add=TRUE)
plot(mi,add=TRUE, lwd=3)

sort(unique(micity$NAME))
# Select out just ones nearby 
save.image("michigan_temp_precip_map.RData")

#Read in the MSU/UM field station locations:
#stations.orig<-read.csv("../../raw_data/mi_research_stations.csv")
# Chosen ones:
stations.orig<-read.csv("../../raw_data/mi_research_stations_NSF2015.csv")
stations<-stations.orig

# Below doesn't really work for some reason
# # Convert to a shapefile: https://help.nceas.ucsb.edu/r:spatial
# stations1 = SpatialPointsDataFrame(stations[c(2:3)],data.frame(stations[c(1:3)]))
# writeSpatialShape(coordinates(stations1),data.frame(stations1),"mi_stations")

# # now use the mask function (this can take a while)
# mi1 <- mask(temp, mi)

coordinates(stations)=~long+lat
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf
proj4string(stations)=CRS("+proj=longlat") # set it to Google Map's projection
stations.df <- SpatialPointsDataFrame(stations, data.frame(id=1:length(stations)))
writeOGR(stations, dsn="." ,layer="t3",driver="ESRI Shapefile")
writeSpatialShape(stations.df, "t2")


plot(mi)
plot(stations,add=TRUE,pch=22)
# Label the nodes
text(stations$long,stations$lat,stations$station,cex=0.5,adj=0,pos=4)

# Close, but we really want to extract out the right temperatures and 
# have the boundaries reflect the MI temps only.

# Better to clip the raster by the outline:
# Use "crop" in "raster" package
# This "crops" it to the rectangular extent of "mi"

# First need to convert "mi" into the same projection as PRISM:
mi1 <- spTransform(mi, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

mi.temp <- crop(temp, extent(mi1))
mi.precip <- crop(precip, extent(mi1))

# Then mask it to the border of the polygon so it's not to the rectangular extent of MI
mi.temp1 <- mask(mi.temp, mi1)
mi.precip1 <- mask(mi.precip, mi1)

save.image("michigan_temp_precip_map.RData")

## *** Great Source code: http://pakillo.github.io/R-GIS-tutorial/

# Extract the temp & precip normals to the research station points:
# http://www.inside-r.org/packages/cran/raster/docs/extract
stations1 <- spTransform(stations, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

stations1$precip<-extract(mi.precip1,stations1)
stations1$temp<-extract(mi.temp1,stations1)

# Extract out just the data/ attribute table!
station_climate<-as(stations1,"data.frame")
write.csv(station_climate,file="station_climate_normals.csv",row.names=F)
save.image("michigan_temp_precip_map.RData")

# Create a map of Michigan showing both the temperature & precip as 3 levels

par(mfrow=c(1,2))
pdf("mi_temp_precip_normals.pdf", height=4,width=4)
plot(mi.temp1, main="30-yr Temperature Normals (C)")
plot(mi1,add=TRUE, lwd=1)

plot(mi.precip1, main="30-yr Precipitation Normals (mm)")
plot(mi1,add=TRUE, lwd=1)
dev.off()

# Specify the breakpoints for 5 temperature zones:
summary(mi.temp1) # min=3.79, max=10.20
(10.20-3.79)/3 # 2.14 increments
3.79+2.14 # 5.93
3.79+2.14+2.14 # 8.07
3.79+2.14+2.14+2.14 # 10.21

summary(mi.precip1) # min=700.87, max=1023.82
(1023.82-700.87)/3 # 107.65 increments
700.87+107.65 # 808.52
700.87+107.65+107.65 # 916.17
700.87+107.65+107.65+107.65 # 1023.82


# jet.colors <-
#   colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
#                      "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# filled.contour(mi.temp, color = jet.colors, asp = 1)
# rgb.palette <- colorRampPalette(c("red", "orange", "blue"),
#                                 space = "rgb")

pdf("mi_3temps_3precip.pdf", width=5, height=6)
t.breakpoints <- c(3.79, 5.93, 8.07, 10.20)
p.breakpoints <- c(700.87, 808.52, 916.17, 1023.82)
t.colors <- c("#fee0d2","#fc9272","#de2d26")
p.colors <- c("#deebf7","#9ecae1","#3182bd")
plot(mi.temp1,breaks=t.breakpoints,col= t.colors, axes=FALSE)
plot(mi.precip1,add=TRUE,breaks=p.breakpoints,col=p.colors, axes=FALSE, alpha=0.5)
plot(mi,add=TRUE, lwd=1.5)
plot(stations,add=TRUE,pch=22,cex=0.5)
# Label the nodes
#text(stations$long,stations$lat,stations$station,cex=0.5,adj=0,pos=4)
legend(x=-90.2, y=44.3,legend = c("3.79-5.92", 
                                  "5.93-8.07", 
                                  "8.08-10.20"), 
       fill=t.colors, cex=.7) 

text(x = -89.5, y = 44.5, label = "Mean Annual 
     Temperature (C)",cex=.7)

legend(x=-90.2, y=43,legend = c("700-808", 
                                "809-916", 
                                "917-1024"), 
       fill=p.colors, cex=.7) 
text(x = -89.5, y = 43.2, label = "Mean Annual 
     Precipitation (mm)",cex=.7)

#legend.args=list(text='Mean Annual Temperature (C)', side=4, font=2, line=2.5, cex=0.8)
scalebar(250, type='bar', divs=1, below="km", lwd=.5, cex=.7)
dev.off()

# Just Temp
pdf("mi_3temps.pdf", width=5, height=6)
t.breakpoints <- c(3.79, 5.93, 8.07, 10.20)
t.colors <- c("#fee0d2","#fc9272","#de2d26")
plot(mi.temp1,breaks=t.breakpoints,col= t.colors, axes=FALSE)
plot(mi,add=TRUE, lwd=1.5)
plot(stations,add=TRUE,pch=22,cex=0.5)
# Label the nodes
text(stations$long,stations$lat,stations$station,cex=0.3,adj=0,pos=4)
legend(x=-90.2, y=44.3,legend = c("3.79-5.92", 
                                  "5.93-8.07", 
                                  "8.08-10.20"), 
       fill=t.colors, cex=.7) 

text(x = -89.3, y = 44.6, label = "Mean Annual 
     Temperature (C)",cex=.7)

scalebar(250, type='bar', divs=1, below="km", lwd=.5, cex=.7)
dev.off()

# Just Precip
pdf("mi_3precip.pdf", width=5, height=6)
p.breakpoints <- c(700.87, 808.52, 916.17, 1023.82)
p.colors <- c("#deebf7","#9ecae1","#3182bd")
plot(mi.precip1,breaks=p.breakpoints,col=p.colors, axes=FALSE, alpha=0.5)
plot(mi,add=TRUE, lwd=1.5)
plot(stations,add=TRUE,pch=22,cex=0.5)
# Label the nodes
text(stations$long,stations$lat,stations$station,cex=0.3,adj=0,pos=4)
legend(x=-90.2, y=43,legend = c("700-808", 
                                "809-916", 
                                "917-1024"), 
       fill=p.colors, cex=.7) 
text(x = -89.3, y = 43.3, label = "Mean Annual 
     Precipitation (mm)",cex=.7)

scalebar(250, type='bar', divs=1, below="km", lwd=.5, cex=.7)
dev.off()

# 5 temps
(10.20-3.79)/5 # 1.282 increments
3.79+1.282 # 5.072
3.79+1.282+1.282 # 6.354
3.79+1.282+1.282+1.282 # 7.636
3.79+1.282+1.282+1.282+1.282 # 8.918
3.79+1.282+1.282+1.282+1.282+1.282 # 10.2

(10.20-3.79)/5 # 1.282 increments
3.8+1.3 # 5.1
3.8+1.3 + 1.3 # 6.4
3.8+1.3 + 1.3 + 1.3 # 7.7
3.8+1.3 + 1.3 + 1.3 + 1.3 # 9
3.8+1.3 + 1.3 + 1.3 + 1.3 + 1.3 # 10.3

# 2018 EDIT: Subset out just KBS, MSU, UMBS
stations2018<-stations
stations2018<-stations2018[stations2018$station == "KBS_LTER" | 
                             stations2018$station == "UMBS" | 
                             stations2018$station == "MSU_Campus",]

pdf("mi_5temps_NSF2018.pdf", width=5, height=5)
# remove box
par(col.axis = "white", col.lab = "white", tck = 0)
breakpoints <- c(3.8, 5.4, 6.4, 7.7, 9, 10.2)
colors <- c("#2c7bb6","#abd9e9","#ffffbf","#fdae61","#d7191c")
plot(mi.temp1,breaks=breakpoints,col=colors, axes=FALSE)
plot(mi,add=TRUE, lwd=1.5)
plot(stations2018,add=TRUE,pch=22,cex=0.7)
# Label the nodes
#text(stations2018$long,stations2018$lat,stations2018$station,cex=0.75,adj=0,pos=4)
legend(x=-90.2, y=44.3,legend = c("3.8-5.06", 
                                  "5.07-6.35", 
                                  "6.36-7.64",
                                  "7.65-8.92",
                                  "8.93-10.2"), 
       fill=colors, cex=.7) 
text(x = -88.3, y = 44.5, label = "Mean Annual Temperature (C)",cex=.7)
scalebar(250, type='bar', divs=1, below="km", lwd=.5, cex=.7)
# remove box
box(col = "white") # turn all of the lines to white
dev.off()


pdf("mi_5temps_NSF2015.pdf", width=5, height=5)
breakpoints <- c(3.8, 5.4, 6.4, 7.7, 9, 10.2)
colors <- c("#2c7bb6","#abd9e9","#ffffbf","#fdae61","#d7191c")
plot(mi.temp1,breaks=breakpoints,col=colors, axes=FALSE)
plot(mi,add=TRUE, lwd=1.5)
plot(stations,add=TRUE,pch=22,cex=0.7)
# Label the nodes
text(stations$long,stations$lat,stations$station,cex=0.75,adj=0,pos=4)
legend(x=-90.2, y=44.3,legend = c("3.8-5.06", 
                                  "5.07-6.35", 
                                  "6.36-7.64",
                                  "7.65-8.92",
                                  "8.93-10.2"), 
       fill=colors, cex=.7) 
text(x = -88.3, y = 44.5, label = "Mean Annual Temperature (C)",cex=.7)
scalebar(250, type='bar', divs=1, below="km", lwd=.5, cex=.7)
dev.off()

save.image("michigan_temp_precip_map.RData")

# Make a map with just the 5 sites: Kellogg Biological Station LTER, MSU Ag Fields, Lake City Research Center, UP Research & Extension Center



## STopped here 6/10. Phew!

tempsmi <- crop(temps, mi) 
tempsmi <-tempsmi[mi,] 
tempsmi1 <- mba.surf(tempsmi,no.X=500, no.Y=500,extend=TRUE, sp = TRUE)$xyz.est

tempsmi<-tempsmi[mi,] # clip surf1 to usa.shape, this is short for: A[!is.na(over(A,B)),]
tempsmi <- as.image.SpatialGridDataFrame(tempsmi)


mimask <- raster(mi) 
#res <- mask(bio1, crmask) 
res<- mask(tempsmi, mimask)

## See: http://r-sig-geo.2731867.n2.nabble.com/Problems-with-mask-raster-td5636566.html





# create a list of .bil files that exist in the wd
files <- list.files(pattern='\\.bil$')
# vars is a vector of bioclim variable numbers
len<-length(files)
vars <- seq(1,len)

# for each of vars, create raster object for each tile and merge
# (this is a bit messy, but all I could think of for now...)
# grids will be a list of rasters, each of which is the merged tiles for a BC var.
grids <- sapply(vars, function(x) {
  patt <- paste('temp', x, '_', sep='')
  tiles <- files[grep(patt, files)]
  merged <- eval(parse(text=paste('merge(', toString(paste('raster(', tiles, ')', 
                                                           sep='"')), ')', sep='')))
})
# give the list elements names
names(grids) <- paste('temp', vars, sep='')
# combine all list elements into a stack
s <- stack(grids)
# quick plot to make sure nothing went drastically wrong
plot(s)
# crop to your study extent
s.crop <- crop(s, WashingtonBoundary)
plot(s.crop)


