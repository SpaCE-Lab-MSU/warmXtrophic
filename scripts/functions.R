######################################
# functions for data checks
# functions for exploratory figures
# created by NKL May 9, 2019
#####################################

#data checks function
data_checks <- function(dat){
	#check for reasonable percent cover values
try(if(max(dat$Cover, na.rm=T) > 100) stop ('Percent cover values greater than 100'))
try(if(max(dat$Cover, na.rm=T) < 0) stop ('Percent cover values less than 0'))
	#check to be sure all taxa codes are on the taxa master list
try(if(length(setdiff(unique(dat$Species), unique(taxa$code))) > "0") stop ('These taxa are not on the taxa master list:'))
try(if(length(setdiff(unique(dat$Species), unique(taxa$code))) > "0") cat (setdiff(unique(dat$Species), unique(taxa$code))))
	#check to be sure all plot codes are on plot master list
try(if(length(setdiff(unique(dat$Plot), unique(plots$plot))) > "0") stop ('One or more plots not on the plot master list'))
	#check to be sure the site is either 'kbs' or 'umbs'
try(if(length(setdiff(unique(dat$Site), unique(events$Site))) > "0") stop ('Site other than kbs or umbs recorded'))
	#check to be sure all sampling events have been recorded in the event table
try(if(length(setdiff(unique(dat$Date), unique(events$Date))) > "0") stop ('These sampling events are not on the event master list'))
try(if(length(setdiff(unique(dat$Date), unique(events$Date))) > "0") cat (setdiff(unique(dat$Date), unique(events$Date))))
}