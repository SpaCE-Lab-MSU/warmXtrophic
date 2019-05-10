######################################
# functions for data checks
# functions for exploratory figures
# created by NKL May 9, 2019
#####################################

###############################
#data checks function
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


