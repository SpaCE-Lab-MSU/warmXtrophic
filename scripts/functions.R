######################################
# functions for data checks
# functions for exploratory figures
# created by NKL May 9, 2019
#####################################

# Check for and install required packages
for (package in c('tidyverse')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

###############################
#data checks function for the L1 data (can use before writing each individual L1 data table and also after combining onto the combined L2 data table but before formatting into obs-variable-value long form and saving)
data_checks <- function(dat){
	#percent cover values
try(if(max(dat$Cover, na.rm=T) > 100) stop ('Percent cover values greater than 100'))
try(if(max(dat$Cover, na.rm=T) < 0) stop ('Percent cover values less than 0'))
try(if(max(dat$Cover, na.rm=T) >= 0 & max(dat$Cover, na.rm=T) <=100) cat ('Success! All percent cover values are between 0-100.\n'))

	#taxa codes
try(if(length(setdiff(unique(dat$Species), unique(taxa$code))) > "0") stop ('These taxa are not on the taxa master list:'))
try(if(length(setdiff(unique(dat$Species), unique(taxa$code))) > "0") cat (c(setdiff(unique(dat$Species), unique(taxa$code))),'\n'))
try(if(length(setdiff(unique(dat$Species), unique(taxa$code))) == 0) cat ('Success! All taxa match the taxa master list.\n'))

	#plot codes
try(if(length(setdiff(unique(dat$Plot), unique(plots$plot))) > "0") stop ('One or more plots not on the plot master list'))
try(if(length(setdiff(unique(dat$Plot), unique(plots$plot))) == 0) cat ('Success! All plot codes match the plot master list.\n'))

	#site codes
try(if(length(setdiff(unique(dat$Site), unique(events$Site))) > "0") stop ('In addition to kbs or umbs, this site was recorded:'))
try(if(length(setdiff(unique(dat$Site), unique(events$Site))) > "0") cat (c(setdiff(unique(dat$Site), unique(events$Site))),'\n'))
try(if(length(setdiff(unique(dat$Site), unique(events$Site))) == 0) cat ('Success! All site codes match the master list.\n'))

	#sampling events
try(if(length(setdiff(unique(dat$Date), unique(events$Date))) > "0") stop ('These sampling events are not on the event master list'))
try(if(length(setdiff(unique(dat$Date), unique(events$Date))) > "0") cat (setdiff(unique(dat$Date), unique(events$Date))))
try(if(length(setdiff(unique(dat$Date), unique(events$Date))) == 0) cat ('Success! All sampling events recorded on the event master list.\n'))

}


############################
# ALPHA DIVERSITY (SPECIES RICHNESS) OVER TIME AND SPACE
############################

#function to remove codes that don't represent taxa

remove.non.taxa <- function (EX) {
	EX <- EX %>%
  dplyr::filter(VARIABLE_NAME != "Brown",  # Remove the non-taxa codes
                VARIABLE_NAME != "Bare_Ground",
                VARIABLE_NAME != "Unknown",
                VARIABLE_NAME != "Litter",
                VARIABLE_NAME != "Groundhog",
                VARIABLE_NAME != "Vert_litter")
}


# TEMPORAL PATTERNS in observations of the number of species
# Note that the thick line indicates the total number of taxa among all plots
plot.no.taxa.ts <- function(EX, site){
  #prepare data frame
	colnames(EX) <- toupper(colnames(EX))
    EX <- subset(EX, SITE == site) #select site
    EX <- remove.non.taxa(EX) #remove Bare_Ground, Litter,etc.
      
  # Number of unique taxa at each site through time
   no.taxa <- EX %>%
    filter(VALUE > 0) %>%
    select(YEAR, VARIABLE_NAME, PLOT) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(PLOT, YEAR) %>%
    summarize(no.taxa = sum(no.taxa))
  
   # Summed number of unique taxa among all sites through time
  total.no.taxa <- EX %>%
    filter(VALUE > 0) %>%
    select(YEAR, VARIABLE_NAME) %>%
    unique() %>%
    mutate(no.taxa = 1) %>%
    group_by(YEAR) %>%
    summarize(no.taxa = sum(no.taxa))

ggplot(data=no.taxa, aes(x=YEAR, y=no.taxa)) +
  geom_point(aes(color = PLOT)) +
  geom_line(aes(color=PLOT)) +
  geom_point(data=total.no.taxa, aes(x=YEAR, y=no.taxa), color="black", size=3) +
  geom_line(data=total.no.taxa, aes(x=YEAR, y=no.taxa), color="black", size=1) +
  xlab("Year") +
  ylab("Number of taxa observed") +
  guides(color = guide_legend(title = paste(toupper(site), "plot", sep = " "))) +
  ylim(c(0, max(total.no.taxa$no.taxa))) +
  theme_bw() +
  theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.position = "right")

}
 

# SPECIES ACCUMULATION CURVE FUNCTION - OVER SPACE
# Make a function that returns the cumulative number of taxa observed for a given set of plots at a site
plot.cuml.taxa.space <- function(EX, site){
  taxa.s.list <- list() # Make empty list
  #prepare data frame
	colnames(EX) <- toupper(colnames(EX))
    EX <- subset(EX, SITE == site) #select site
    EX <- remove.non.taxa(EX) #remove Bare_Ground, Litter,etc.
    EX <- arrange(EX, PLOT) #make sure plots in increasing order
      
  plots <-unique(EX$PLOT)

  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(p in 1:length(unique(EX$PLOT))){
    tmp.dat <- subset(EX, EX$PLOT == plots[p])
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.s.list[[p]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }

  # Make cumulative list of taxa over space
  cuml.taxa.space <- list() # Empty list
  cuml.taxa.space[[1]] <- taxa.s.list[[1]] # Add the taxa from the first sites 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(p in 2:length(unique(EX$PLOT))){ 
    cuml.taxa.space[[p]] <- c(cuml.taxa.space[[p - 1]], taxa.s.list[[p]])
  }
    # Remove duplicates
  cuml.taxa.space <- lapply(cuml.taxa.space, function(x){unique(x)})
    # Return the number of total unique taxa over space
  cuml.no.taxa.space <- data.frame("Plot" = unique(EX$PLOT))
  cuml.no.taxa.space$no.taxa <- unlist(lapply(cuml.taxa.space, function(x){length(x)}))
  
   # Plot
plot(as.numeric(cuml.no.taxa.space$Plot), cuml.no.taxa.space$no.taxa, pch = 19, type = "o",  xaxt="n", bty="l", xlab = "Cumulative number of sites", ylab = "Cumulative number of taxa", cex=1.5, lwd=3, cex.lab=1, main = toupper(site))
axis(side=1, at = cuml.no.taxa.space$Plot, labels = seq(1,length(cuml.no.taxa.space$Plot),1))

}


# TEMPORAL SPECIES ACCUMULATION CURVES

#function for calculating cuml no. taxa each year:
cuml.taxa.fun <- function(EX){
  taxa.t.list <- list() # Make empty list
  years <- unique(EX$YEAR)
  # Loop over each year, creating a list that contains the unique taxa found in each year
  for(t in 1:length(unique(EX$YEAR))){
    tmp.dat <- subset(EX, EX$YEAR == years[t])
    tmp.dat.pres <- subset(tmp.dat, tmp.dat$VALUE > 0) 
    taxa.t.list[[t]] <- unique(tmp.dat.pres$VARIABLE_NAME)
  }  
  # Make cumulative list of taxa through time
  cuml.taxa <- list() # Empty list
  cuml.taxa[[1]] <- taxa.t.list[[1]] # Add the taxa from the first time step 
  
  # Run for-loop to create list of the cumulative taxa, with duplicates
  for(t in 2:length(unique(EX$YEAR))){ 
    cuml.taxa[[t]] <- c(cuml.taxa[[t - 1]], taxa.t.list[[t]])
  }
  
  # Remove duplicates
  cuml.taxa <- lapply(cuml.taxa, function(x){unique(x)})
  
  # Return the number of total unique taxa through time
  cuml.no.taxa <- data.frame("year" = unique(EX$YEAR))
  cuml.no.taxa$no.taxa <- unlist(lapply(cuml.taxa, function(x){length(x)}))
 
 return(cuml.no.taxa) 
}


#calculate and visualize the plot-specific and overall cumulative no. taxa
#this function calls the cuml.no.taxa function above
  
plot.cuml.taxa.time <- function(EX, site){
  #prepare data frame
	colnames(EX) <- toupper(colnames(EX))
    EX <- subset(EX, SITE == site) #select site
    EX <- remove.non.taxa(EX) #remove Bare_Ground, Litter,etc.

# Calculate cumulative taxa (i.e., species accumulation) across all sites pooled together
cuml.taxa.all.sites <- cuml.taxa.fun(EX)

# Examine site-level patterns of species accumulation
# First, sort the comm.dat dataframe to make sure its ordered by site
EX <- EX %>% 
  arrange(PLOT)

# Then, split the data frame, and apply the cuml.taxa.fun() for each site
X <- split(EX, EX$PLOT)
out <- lapply(X, cuml.taxa.fun)

# Make the lists a dataframe
output <- do.call("rbind", out)

# Extract rownames to create a SITE_ID column
output$rnames <- row.names(output)

# Clean up the SITE_ID column
cuml.taxa.by.site <- output %>%
  tbl_df() %>%
  separate(rnames, c("PLOT", "todrop"), sep = "\\.") %>%
  select(-todrop)

# Plot the cumulative number of taxa observed at each site, as well as across all sites together
# Note that the thick line indicates the total number of taxa among all sites

ggplot(data=cuml.taxa.by.site, aes(x = year, y = no.taxa)) +
  geom_point(aes(color = PLOT)) +
  geom_line(aes(color = PLOT)) +
  geom_point(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 3) +
  geom_line(data = cuml.taxa.all.sites, aes(x=year, y=no.taxa), size = 1.5) +
  xlab("Year") +
  ylab("Cumulative number of taxa") +
  guides(color = guide_legend(title = paste(toupper(site), "plot", sep = " "))) +
  ylim(c(0, max(cuml.taxa.all.sites$no.taxa))) +
  theme_bw()  +
  theme(axis.title = element_text(size=12), axis.text = element_text(size=12), legend.position = "right") #

}

############################
# PHENOLOGY FUNCTIONS
############################


#plot time series of percent cover for a given species within a given year

plot_phenology_ts <- function (dat, site, species, year){
	newdat <- merge(dat, plots, by.x = "Plot", by.y = "plot")
	temp1 <- subset(newdat, Site == site & Species == species & Year == year)
	plot(temp1$Julian_day, temp1$Cover, type = "n", xlim = c(min(temp1$Julian_day), max(temp1$Julian_day)), ylim = c(0,max(temp1$Cover)), xlab = "Julian day", ylab = "Percent cover", main = paste(site, " - ", species," - ", year)) 
	plot_id <- as.vector(unique(temp1$Plot))
for(p in 1:length(plot_id)){ 
	temp2 <- subset(temp1, Plot == plot_id[p])
	temp2 <- temp2[order(temp2$Julian_day),] 
	color_key <- ifelse(unique(temp2$state)=="warmed", "red", "blue") 	lines(temp2$Julian_day, temp2$Cover, col = color_key, type = "o")
	}
	legend("topleft", col = c("blue", "red"), legend = c("ambient", "warmed"), pch = 1, lty=1)	
}



