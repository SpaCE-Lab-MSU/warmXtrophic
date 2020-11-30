## 2015 Species Composition Data Using Raw Data
## KBW 

#clear all existing data/graphics
rm(list=ls())
graphics.off()

#install packages
library(reshape) 
library(car)

#KBS
k<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/KBS_2015_Plantcomp.csv", strip.white=T, na.strings="na", header = TRUE)
summary(k)

#compare only the first sp. cover values
kfirst <- k[which(k$Julian=='141'),]
names(kfirst)[1:7] <- tolower(names(kfirst)[1:7])

#look at data
unique(kfirst$species)

#Hisp to Hipr
kfirst$species <- as.character(kfirst$species)
kfirst$species[kfirst$species == "Hisp"] <- "Hipr"

#Rufl to Rual
kfirst$species <- as.character(kfirst$species)
kfirst$species[kfirst$species == "Rufl"] <- "Rual"

#check to see if fixed
unique(kfirst$species)


#average sub-quadrats for plots
quad.mn = aggregate(cover ~ plot*species, data=kfirst, FUN=mean, na.rm=T)
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"
head(quad.mn)
#k2<-merge(kfirst, quad.mn, by=c("plot", "species"), all.x=T, all.y=F)


#convert cover to relative abundance 
#first get summed cover for all plants per plot
cov.sum = aggregate(quad.mn ~ plot, data=quad.mn, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="quad.mn"]<-"cov.sum"
head(cov.sum)
k2<-merge(quad.mn,cov.sum, by=c("plot"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
k2$relab<-k2$quad.mn/k2$cov.sum
summary(k2)


#merge in species info
spinfo<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/KBS_Species_Traits_Comp.csv")
str(spinfo)
k3<-merge(k2, spinfo, by=c("species"), all.x=T, all.y=T)
str(k3)

#merge in plot info
plotinfo<-read.csv("~/Data/Summer2015/Setup_Table.csv", header = TRUE)
str(plotinfo)
k4<-merge(k3, plotinfo, by=c("plot"), all.x=T, all.y=T)
k4$site <- "kbs"

#make dataset for avg'd quadrats
kcover <- merge(quad.mn, spinfo, by=c("species"), all.x=T, all.y=T)
kcover2 <- merge(kcover, plotinfo, by=c("plot"), all.x=T, all.y=T)
kcover2$site <- "kbs"

write.csv(kcover2, file = "kbscomp15.csv")

#view data
hist(kcover2$quad.mn)
#skewed data, try ln() transformation
kcover2$lncov <- log(kcover2$quad.mn)
hist(kcover2$lncov)

#2015 Differences
library(lme4)
library(MuMIn)
library(lmerTest)

#Include mammals?
mod1 <- lmer(lncov~ warmed + origin+ warmed*origin + mammals + insects + (1|species), kcover2)
mod2 <- lmer(lncov~ warmed + origin+ warmed*origin + insects + (1|species), kcover2)
anova(mod2,mod1)

#Include insects?
mod3 <- lmer(lncov~ warmed + origin+ warmed*origin + (1|species), kcover2)
anova(mod3,mod2)

mod4<-lmer(lncov~ warmed + origin + (1|species), kcover2)
anova(mod4,mod3)
summary(mod4)
confint(mod5, method="boot", nsim=999)


############################################# UMBS #############################################
u<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/UMBS_2015_Plantcomp.csv", strip.white=T, na.strings="na", header = TRUE)
summary(u)

#compare only the first sp. cover values
ufirst <- u[which(u$Julian=='175'),]
names(ufirst)[1:7] <- tolower(names(ufirst)[1:7])

#look at data
unique(ufirst$species)

#average sub-quadrats for plots
quad.mn = aggregate(cover ~ plot*species, data=ufirst, FUN=mean, na.rm=T)
names(quad.mn)[names(quad.mn)=="cover"]<-"quad.mn"
head(quad.mn)

#convert cover to relative abundance 
#first get summed cover for all plants per plot
cov.sum = aggregate(quad.mn ~ plot, data=quad.mn, FUN=sum, na.rm=T)
names(cov.sum)[names(cov.sum)=="quad.mn"]<-"cov.sum"
head(cov.sum)
u2<-merge(quad.mn,cov.sum, by=c("plot"))

#calculate relative percent cover per species in each quadrat (="relative abundance")
u2$relab<-u2$quad.mn/u2$cov.sum
summary(u2)

#merge in species info
spinfo<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/UMBS_Species_Traits_Comp.csv")
str(spinfo)
u3<-merge(u2, spinfo, by=c("species"), all.x=T, all.y=T)
str(u3)

#merge in plot info
plotinfo<-read.csv("~/Data/Summer2015/Setup_Table.csv", header = TRUE)
str(plotinfo)
u4<-merge(u3, plotinfo, by=c("plot"), all.x=T, all.y=T)
u4$site <- "umbs"


#make dataset for avg'd quadrats
ucover <- merge(quad.mn, spinfo, by=c("species"), all.x=T, all.y=T)
ucover2 <- merge(ucover, plotinfo, by=c("plot"), all.x=T, all.y=T)
ucover2$site <- "umbs"

write.csv(ucover2, file = "umbscomp15.csv")

#view data
hist(ucover2$quad.mn)
#skewed data, try ln() transformation
ucover2$lncov <- log(ucover2$quad.mn)
hist(ucover2$lncov)

#2015 Differences
library(lme4)
library(MuMIn)
library(lmerTest)

#Include mammals?
mod1 <- lmer(lncov~ warmed + origin+ warmed*origin + mammals + insects + (1|species), ucover2)
mod2 <- lmer(lncov~ warmed + origin+ warmed*origin + insects + (1|species), ucover2)
anova(mod2,mod1)

#Include insects?
mod3 <- lmer(lncov~ warmed + origin+ warmed*origin + (1|species), ucover2)
anova(mod3,mod2)

mod4<-lmer(lncov~ warmed + origin + (1|species), ucover2)
anova(mod4,mod3)
summary(mod4)
confint(mod5, method="boot", nsim=999)




######################  COMBINING SITES  ######################
# combine sites cover
combinedsites <- rbind(kcover2, ucover2)
write.csv(combinedsites, file = "cover_combined_site.csv")

# combine sites relab
combinedsites <- rbind(k4, u4)

#view data
hist(combinedsites$relab)
#skewed data, try ln() transformation
lnrelab <- log(combinedsites$relab)
hist(lnrelab)

#now test for initial differences between plot relab
library(lme4)
fullmodel <- lmer(lnrelab~warmed + origin + (1|species) + (1|site), combinedsites)
anova(fullmodel)
summary(fullmodel)
confint(fullmodel, method="boot", nsim=999)

#now test for initial differences between plot cover
cov<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/cover_combined_site.csv", strip.white=T, na.strings="na", header = TRUE)
cov$quad.mn <- as.numeric(cov$quad.mn)
hist(cov$quad.mn)
cov$lncov <-log(cov$quad.mn)
hist(cov$lncov)

fullmodel <- lmer(quad.mn~warmed + origin + (1|species) + (1|site), cov)
anova(fullmodel)
summary(fullmodel)
confint(fullmodel, method="boot", nsim=999)

fullmodel <-lm(quad.mn~warmed, cov)
anova(fullmodel)

#all treatments
allmodel <- lmer(quad.mn~ all + (1|species) + (1|site), cov)
anova(allmodel)
summary(allmodel)
confint(allmodel, method="boot", nsim=999)

allmodel2 <- lm(quad.mn~all, cov)
anova(allmodel2)

nullmodel <- lmer(quad.mn~ 1 + (1|species) + (1|site), cov)
anova(nullmodel, allmodel)

#2015 Differences
mod5 <- lmer(lncov~ warmed + origin+ warmed*origin + mammals + insects + (1|species) + (1|site), cov)
anova(mod5)
confint(mod5, method="boot", nsim=999)


mod6 <- lmer(lncov~ warmed + origin + warmed*origin + mammals + insects + (1|species) + (1|site), cov)
modnull <- lmer(lncov~ 1 + (1|species) + (1|site), cov)
anova(modnull, mod6)

aves <- tapply(cov$lncov, list(cov$warmed, cov$origin), mean)
stdev <- tapply(cov$lncov, list(cov$warmed, cov$origin), sd)
smple <- tapply(cov$lncov, list(cov$warmed, cov$origin), length)
stderr <- (stdev/sqrt(smple)) 
stderr2 <- (stdev/sqrt(smple))*(-1) #make error bars negative
barcenters <- barplot(aves, beside=T, ylab="Percent Cover", ylim=c(0,3), cex.lab=1.5, col=c("azure3", "black"))
arrows(barcenters, aves, barcenters, aves+stderr, angle=90, length=0.1)
arrows(barcenters, aves, barcenters, aves+stderr2, angle=90, length=0.1)
par(xpd=TRUE)
