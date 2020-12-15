## 2016 EMERGENCE
## KBS AND UMBS
## KBW 

#clear all existing data/graphics
rm(list=ls())
graphics.off()

#UMBS
#datau<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/UMBS_Greenup_2016.csv", strip.white=T, header = TRUE)
datau<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/UMBS_Greenup_2016.csv", strip.white=T, header = TRUE)

str(datau)
# change column headings to lower case
names(datau)[1:5] <- tolower(names(datau)[1:5])
#plots: okay
unique(unlist(datau$plot, use.names = FALSE))
#species: 
unique(unlist(datau$species, use.names = FALSE))
#days: okay
unique(unlist(datau$julian, use.names = FALSE))
#take out Brown and Bareground later

# Re-order columns in data.frame
datau <- datau[, c("plot", "species", "cover", "date", "julian")]

# Split data.frame
dataus <- split(x = datau, f = datau[, c("plot", "species")])

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dates <- unlist(lapply(X = dataus, FUN = function(x){x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]}))

# Save as data.frame
half_cover_dates_df <- data.frame("plot.species" = names(half_cover_dates), "half_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

half_cover_dates_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dates_df[["plot.species"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dates_df[["species"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species"]], split = ".", fixed = TRUE), FUN = `[`, 2L)

half_cover_dates_df$plot.species <- NULL

#determine first date of emergence for correlation with 'green-up' index
min_date <- aggregate(datau$julian,by=datau[,c("plot","species")],FUN=min)
colnames(min_date) <- c("plot", "species", "min_emerg_date")

#merge min date dateframe with "green-up index" df
combined <- merge(half_cover_dates_df, min_date, by=c("plot", "species"))

#calculate correlation
cor.test(combined$min_emerg_date, combined$half_cover_date)

#determine date of median cover
#y<-aggregate(x=datau$cover,by=datau[,c("plot","species")],FUN=median)

#merge back into original dataframe
#z <- merge(datau,y,by=c("plot","species"))

# get the numeric value of the first date where the variable "cover" is greater than or equal to 
#           "x" (the median cover for a given species in a given plot)
#list <- lapply(X=split(z,f=list(z$plot,z$species)), FUN=function(x){x[which.max(x$cover >= x$x),"julian"]})

#df <- as.data.frame(do.call(rbind, list))
#rn <- rownames(df)

#library(stringr)
#check <- strsplit(rn, ".", fixed=TRUE)
#str(check)

#turn list into dataframe
#mat  <- matrix(unlist(check), ncol=2, byrow=TRUE)
#newdf   <- as.data.frame(mat)
#combineddf   <- cbind(newdf, df)

#change column names
#colnames(combineddf) <- c("plot", "species", "sp_med_emerg_date")


#merge in plot level data
plotinfo<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/Setup_Table.csv", header = TRUE)
str(plotinfo)
dfa <- merge(half_cover_dates_df, plotinfo, by=c("plot"), all.x=T, all.y=T)

#merge in origin data
spinfo<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/UMBS_Species_Traits_Phen.csv", header = TRUE)
final<-merge(dfa, spinfo, by=c("species"), all.x=T, all.y=T)

#clean up dataframe
final$group <- NULL
final$growth_habit <- NULL
str(final)

#take out "brown" litter
final <- subset(final, species != "Brown")
final <- subset(final, species != "Bareground")
final


##MODEL COMPARISONS FOR MEDIAN COVER
library(lme4)
library(MuMIn)
library(lmerTest)

#include plot as a random effect?
moda <- lmer(half_cover_date ~ warmed*origin + insects + mammals + (1|species) + (1|plot), final, REML=FALSE)
modb <- lmer(half_cover_date ~ warmed*origin + insects + mammals + (1|species), final, REML=FALSE)
anova(modb, moda)
summary(moda) 
summary(modb)
anova(mod1)
anova(mod2)

#Do we need mammals?
mod1 <- lmer(half_cover_date ~ warmed*origin + insects + (1|species), final, REML=FALSE)
anova(mod1, modb)

#Do we need insects?
mod2 <- lmer(half_cover_date ~ warmed*origin + (1|species), final, REML=FALSE)
anova(mod2, mod1)

#Do we need an interaction term?
mod3 <- lmer(half_cover_date ~ warmed + origin + (1|species), final, REML=FALSE)
anova(mod3, mod2)
anova(mod3)
summary(mod3)
confint(mod3, method="boot", nsim=999)
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(half_cover_date ~ mammals + (1|species), final, REML=FALSE)
modb <- lmer(half_cover_date ~ (1|species), final, REML=FALSE)

anova(modb, moda)

############################ PERCENTILES NOT USED IN MS #################################

#determine date of 10th percentile
p<-aggregate(x=datau$cover,by=datau[,c("plot","species")],FUN='quantile', probs=50/100)

#merge back into original dataframe
z <- merge(datau,p,by=c("plot","species"))

# get the numeric value of the first date where the variable "cover" is greater than or equal to 
#           "x" (the median cover for a given species in a given plot)
list <- lapply(X=split(z,f=list(z$plot,z$species)), FUN=function(x){x[which.max(x$cover >= x$x),"julian"]})

df <- as.data.frame(do.call(rbind, list))
rn <- rownames(df)

library(stringr)
check <- strsplit(rn, ".", fixed=TRUE)
str(check)

#turn list into dataframe
mat  <- matrix(unlist(check), ncol=2, byrow=TRUE)
newdf   <- as.data.frame(mat)
combineddf   <- cbind(newdf, df)

#change column names
colnames(combineddf) <- c("plot", "species", "sp_med_emerg_date")

#merge in plot level data
plotinfo<-read.csv("~/Data/Summer2015/Setup_Table.csv", header = TRUE)
str(plotinfo)
dfa <- merge(combineddf, plotinfo, by=c("plot"), all.x=T, all.y=T)

#merge in origin data
spinfo<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/UMBS_Species_Traits_Phen.csv", header = TRUE)
final<-merge(dfa, spinfo, by=c("species"), all.x=T, all.y=T)

#clean up dataframe
final$group <- NULL
final$growth_habit <- NULL
str(final)

#take out "brown" litter
final <- subset(final, species != "Brown")
final <- subset(final, species != "Bareground")
final


##MODEL COMPARISONS FOR 10th PERCENTILE

#include plot as a random effect?
moda <- lmer(sp_med_emerg_date ~ warmed*origin + insects + mammals + (1|species) + (1|plot), final, REML=FALSE)
modb <- lmer(sp_med_emerg_date ~ warmed*origin + insects + mammals + (1|species), final, REML=FALSE)
anova(modb, moda)
summary(moda) 
summary(modb)


#Do we need mammals?
mod1 <- lmer(sp_med_emerg_date ~ warmed*origin + insects + (1|species), final, REML=FALSE)
anova(mod1, modb)

#Do we need insects?
mod2 <- lmer(sp_med_emerg_date ~ warmed*origin + (1|species), final, REML=FALSE)
anova(mod2, mod1)

#Do we need an interaction term?
mod3 <- lmer(sp_med_emerg_date ~ warmed + origin + (1|species), final, REML=FALSE)
anova(mod3, mod2)
anova(mod3)
summary(mod3)
confint(mod3, method="boot", nsim=999)
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")



############## FIGURE ###################
#Base R
boxplot(sp_med_emerg_date~warmed*origin, data=final, horizontal=TRUE, xlab="Julian Day", varwidth=TRUE)
#names=c("1", "6 cyl", "8 cyl"

#GGPLOT
library(ggplot2)
p_emerg <- ggplot(aes(y = half_cover_date, x = origin, fill = warmed), data = final) +
  geom_boxplot(position = position_dodge(width = 0.9))+ coord_flip() + ylim(117, 160) +
  theme(legend.position = "none", panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white")) +
  labs(x="", y="Julian Date")
  #ggtitle("Date of 50% Green-Up")

p_emerg
ggsave('emerg.png', p_emerg, height=1.75, width=3, dpi=600)



######################################### KBS ###############################################

#clear all existing data/graphics
rm(list=ls())
graphics.off()

datak<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/KBS_Greenup_2016.csv", strip.white=T, header = TRUE)
plot(datak$julian, datak$cover)

#plots: okay
unique(unlist(datak$plot, use.names = FALSE))
#species: 
unique(unlist(datak$species, use.names = FALSE))
datak$species <- as.character(datak$species)
datak$species[datak$species == "popr"] <- "Popr"
datak$species[datak$species == "Hisp"] <- "Hipr"
datak$species <- as.factor(datak$species)
#days: okay
unique(unlist(datak$julian, use.names = FALSE))

#check that cover increases over time and dormant/litter decreases over time
litter <- subset(datak, species == "Brown")
plot(litter$julian, litter$cover)

soca <- subset(datak, species == "Soca")
plot(soca$julian, soca$cover)

# Re-order columns in data.frame
datak <- datak[, c("plot", "species", "cover", "date", "julian")]

# Split data.frame
dataks <- split(x = datak, f = datak[, c("plot", "species")])

# Determine dates for each plot-species combination where the value of `cover` is at least half the max value
half_cover_dates <- unlist(lapply(X = dataks, FUN = function(x){x[which.max(x[["cover"]] >= max(x[["cover"]])/2), "julian"]}))

# Save as data.frame
half_cover_dates_df <- data.frame("plot.species" = names(half_cover_dates), "half_cover_date" = unname(half_cover_dates), stringsAsFactors = FALSE)

half_cover_dates_df[["plot"]] <- sapply(X = strsplit(x = half_cover_dates_df[["plot.species"]], split = ".", fixed = TRUE), FUN = `[`, 1L)
half_cover_dates_df[["species"]] <- sapply(X = strsplit(x =half_cover_dates_df[["plot.species"]], split = ".", fixed = TRUE), FUN = `[`, 2L)

half_cover_dates_df$plot.species <- NULL


#determine first date of emergence for correlation with 'green-up' index
min_date <- aggregate(datak$julian,by=datak[,c("plot","species")],FUN=min)
colnames(min_date) <- c("plot", "species", "min_emerg_date")

#merge min date dateframe with "green-up index" df
combined <- merge(half_cover_dates_df, min_date, by=c("plot", "species"))

#calculate correlation
cor.test(combined$min_emerg_date, combined$half_cover_date)

#determine date of median cover
#y<-aggregate(x=datak$cover,by=datak[,c("plot","species")],FUN=median)
#y<-aggregate(x=datak$cover,by=datak[,c("plot","species")],FUN='quantile', probs=50/100)
#merge back into original dataframe
#z <- merge(datak,y,by=c("plot","species"))

# get the numeric value of the first date where the variable "cover" is greater than or equal to 
#           "x" (the median cover for a given species in a given plot)
#list <- lapply(X=split(z,f=list(z$plot,z$species)), FUN=function(x){x[which.max(x$cover >= x$x),"julian"]})

#df <- as.data.frame(do.call(rbind, list))
#rn <- rownames(df)

#library(stringr)
#check <- strsplit(rn, ".", fixed=TRUE)
#str(check)

#turn list into dataframe
#mat  <- matrix(unlist(check), ncol=2, byrow=TRUE)
#newdf   <- as.data.frame(mat)
#combineddf   <- cbind(newdf, df)

#change column names
#colnames(combineddf) <- c("plot", "species", "sp_med_emerg_date")

#merge in plot level data
plotinfo<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/Setup_Table.csv", header = TRUE)
str(plotinfo)
dfa <- merge(half_cover_dates_df, plotinfo, by=c("plot"), all.x=T, all.y=T)

#merge in origin data
spinfo<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/KBS_Species_Traits_Phen.csv", header = TRUE)
final<-merge(dfa, spinfo, by=c("species"), all.x=T, all.y=T)

#clean up dataframe
final$group <- NULL
final$growth_habit <- NULL
str(final)

#take out "brown" litter
final <- subset(final, species != "Brown")


##MODEL COMPARISONS
library(lme4)
library(MuMIn)
library(lmerTest)

#include plot as a random effect?
moda <- lmer(half_cover_date ~ warmed*origin + insects + mammals + (1|species) + (1|plot), final, REML=FALSE)
modb <- lmer(half_cover_date ~ warmed*origin + insects + mammals + (1|species), final, REML=FALSE)
anova(modb, moda)
summary(moda) 
summary(modb)


#Do we need mammals?
mod1 <- lmer(half_cover_date ~ warmed*origin + insects + (1|species), final, REML=FALSE)
anova(mod1, modb)

#Do we need insects?
mod2 <- lmer(half_cover_date ~ warmed*origin + (1|species), final, REML=FALSE)
anova(mod2, mod1)

#Do we need an interaction term?
mod3 <- lmer(half_cover_date ~ warmed + origin + (1|species), final, REML=FALSE)
anova(mod3, mod2)
anova(mod3)
summary(mod3)
confint(mod3, method="boot", nsim=999)
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(half_cover_date ~ mammals + (1|species), final, REML=FALSE)
modb <- lmer(half_cover_date ~ (1|species), final, REML=FALSE)
anova(modb, moda)

#GGPLOT
library(ggplot2)
p_emerg <- ggplot(aes(y = half_cover_date, x = origin, fill = warmed), data = final) +
  geom_boxplot(position = position_dodge(width = .9))+ coord_flip() +
  theme(legend.position = "none", panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white"))+
  labs(x="", y="Julian Date") 
  #ggtitle("Date of 50% Green-Up")
p_emerg
ggsave('K_emerg.png', p_emerg, height=1.75, width=3, dpi=600)
