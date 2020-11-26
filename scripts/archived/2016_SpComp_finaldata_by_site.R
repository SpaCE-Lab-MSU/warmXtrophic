## 2016 Species Composition Data Using Final Data (August 2016)
## KBS AND UMBS
## KBW 

#2-part approach
#clear all existing data/graphics
rm(list=ls())
graphics.off()


########################################### SPECIES COVER ###########################################

data<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/cover_combined_site.csv", strip.white=T, header = TRUE)

######################## KBS ########################
datak <- subset(data, site == 'kbs')

library(lme4)
library(MuMIn)
library(lmerTest)

#include plot as a random effect?
moda <- lmer(lncover ~ warmed*origin + insects + mammals + (1|species) + (1|plot), datak, REML=FALSE)
modb <- lmer(lncover ~ warmed*origin + insects + mammals + (1|species), datak, REML=FALSE)
anova(modb, moda)


#Do we need mammals?
mod1 <- lmer(lncover ~ warmed*origin + insects + (1|species), datak, REML=FALSE)
anova(mod1, modb)

#Do we need insects?
mod2 <- lmer(lncover ~ warmed*origin + (1|species), datak, REML=FALSE)
anova(mod2, mod1)

#Do we need an interaction term?
mod3 <- lmer(lncover ~ warmed + origin + (1|species), datak, REML=FALSE)
anova(mod3, mod2)
anova(mod3)
summary(mod3)
confint(mod3, method="boot", nsim=999)
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(lncover ~ mammals + (1|species), datak, REML=FALSE)
modb <- lmer(lncover ~ (1|species), datak, REML=FALSE)
anova(modb, moda)


#warming effects each origin
exot.cov <- subset(datak, origin == 'Exotic')
test <- lm(lncover~warmed, data=exot.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(exot.cov$lncover)

nat.cov <- subset(datak, origin == 'Native')
test <- lm(lncover~warmed, data=nat.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(nat.cov$lncover)

#warming by species
unique(datak$species)
par(mfrow=c(1,2)) 
boxplot(cover~warmed*species, datak)

png(filename = "FigAppCa.png", width = 6, height = 8, units = "in", pointsize = 9, res=600)

par(mfrow=c(5,3), mar=c(2.5,2,2,1.5)) 
#Exotics at KBS arranged most abundant to least abundant

Popr <- subset(datak, species == 'Popr')
boxplot(cover~warmed,data=Popr, main=expression(italic('Poa pratensis')), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Popr, method = "jitter", add = TRUE, pch = 17, col = 'black')
#stripchart(cover~warmed, vertical = TRUE, data = Popr, method = "jitter", pch = 17, col = 'black', main="Poa pratensis-Exotic")

Hipr <- subset(datak, species == 'Hipr')
boxplot(cover~warmed,data=Hipr, main=expression(italic('Hieracium pratense')), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Hipr, method = "jitter", add = TRUE, pch = 17, col = 'black')

Phpr <- subset(datak, species == 'Phpr')
boxplot(cover~warmed,data=Phpr, main=expression(italic("Phleum pratense")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Phpr, method = "jitter", add = TRUE, pch = 17, col = 'black')

Trpr <- subset(datak, species == 'Trpr')
boxplot(cover~warmed,data=Trpr, main=expression(italic("Trifolium pratense")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Trpr, method = "jitter", add = TRUE, pch = 17, col = 'black')

Cest<- subset(datak, species == 'Cest')
boxplot(cover~warmed,data=Cest, main=expression(italic("Centaurea stoebe")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Cest, method = "jitter", add = TRUE, pch = 17, col = 'black')

Pore <- subset(datak, species == 'Pore')
boxplot(cover~warmed,data=Pore, main=expression(italic("Potentilla recta")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Pore, method = "jitter", add = TRUE, pch = 17, col = 'black')

Dagl <- subset(datak, species == 'Dagl')
boxplot(cover~warmed,data=Dagl, main=expression(italic("Dactylis glomerata")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Dagl, method = "jitter", add = TRUE, pch = 17, col = 'black')

Daca<- subset(datak, species == 'Daca')
boxplot(cover~warmed,data=Daca, main=expression(italic("Daucus carota")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Daca, method = "jitter", add = TRUE, pch = 17, col = 'black')

Trre <- subset(datak, species == 'Trre')
boxplot(cover~warmed,data=Trre, main=expression(italic("Trifolium repens")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Trre, method = "jitter", add = TRUE, pch = 17, col = 'black')



Alpe <- subset(datak, species == 'Alpe')
boxplot(cover~warmed,data=Alpe, main=expression(italic("Alliaria petiolata")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Alpe, method = "jitter", add = TRUE, pch = 17, col = 'black')

Ceor <- subset(datak, species == 'Ceor')
boxplot(cover~warmed,data=Ceor, main=expression(italic("Celastrus orbiculatus")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Ceor, method = "jitter", add = TRUE, pch = 17, col = 'black')

Arel <- subset(datak, species == 'Arel')
boxplot(cover~warmed,data=Arel, main=expression(italic("Arrhenatherum elatius")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Arel, method = "jitter", add = TRUE, pch = 17, col = 'black')
#stripchart(cover~warmed, vertical = TRUE, data = Arel, method = "jitter", pch = 17, col = 'black', main="Arrhenatherum elatius-Exotic")

Hype <- subset(datak, species == 'Hype')
boxplot(cover~warmed,data=Hype, main=expression(italic("Hypericum perforatum")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Hype, method = "jitter", add = TRUE, pch = 17, col = 'black')

Elre <- subset(datak, species == 'Elre')
boxplot(cover~warmed,data=Elre, main=expression(italic("Elymus repens")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Elre, method = "jitter", add = TRUE, pch = 17, col = 'black')

Taof <- subset(datak, species == 'Taof')
boxplot(cover~warmed,data=Taof, main=expression(italic("Taraxicum officinale")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Taof, method = "jitter", add = TRUE, pch = 17, col = 'black')

dev.off()

png(filename = "FigAppCb.png", width = 6, height = 8, units = "in", pointsize = 9, res=600)

par(mfrow=c(5,3), mar=c(2.5,2,2,1.5)) 
#Natives at KBS arranged most abundant to least abundant
Soca <- subset(datak, species == 'Soca')
boxplot(cover~warmed,data=Soca, main=expression(italic("Solidago canadensis")), names=c('Ambient', 'Warmed'))
stripchart(cover~warmed, vertical = TRUE, data = Soca, method = "jitter", add = TRUE, pch = 17, col = 'black')
Acmi <- subset(datak, species == 'Acmi')
boxplot(cover~warmed,data=Acmi, main=expression(italic("Achillea millefolium")), names=c('Ambient', 'Warmed'))
stripchart(cover~warmed, vertical = TRUE, data = Acmi, method = "jitter", add = TRUE, pch = 17, col = 'black')
#stripchart(cover~warmed, vertical = TRUE, data = Acmi, method = "jitter", pch = 17, col = 'black', main="Achillea millefolium-Native")
Eugr <- subset(datak, species == 'Eugr')
boxplot(cover~warmed,data=Eugr, main=expression(italic("Euthamia graminifolia")), names=c('Ambient', 'Warmed'))
stripchart(cover~warmed, vertical = TRUE, data = Eugr, method = "jitter", add = TRUE, pch = 17, col = 'black')
Rual <- subset(datak, species == 'Rual')
boxplot(cover~warmed,data=Rual, main=expression(italic("Rubus allegheniensis")), names=c('Ambient', 'Warmed'))
stripchart(cover~warmed, vertical = TRUE, data = Rual, method = "jitter", add = TRUE, pch = 17, col = 'black'  )
Assp <- subset(datak, species == 'Assp')
boxplot(cover~warmed,data=Assp, main=expression(italic("Asclepias sp.")), names=c('Ambient', 'Warmed'))
stripchart(cover~warmed, vertical = TRUE, data = Assp, method = "jitter", add = TRUE, pch = 17, col = 'black', main="Asclepias sp.")

dev.off()

######################## UMBS ########################

datau <- subset(data, site == 'umbs')
unique(datau$species)

#include plot as a random effect?
moda <- lmer(lncover ~ warmed*origin + insects + mammals + (1|species) + (1|plot), datau, REML=FALSE)
modb <- lmer(lncover ~ warmed*origin + insects + mammals + (1|species), datau, REML=FALSE)
anova(modb, moda)


#Do we need mammals?
mod1 <- lmer(lncover ~ warmed*origin + insects + (1|species), datau, REML=FALSE)
anova(mod1, modb)

#Do we need insects?
mod2 <- lmer(lncover ~ warmed*origin + (1|species), datau, REML=FALSE)
anova(mod2, mod1)

#Do we need an interaction term?
mod3 <- lmer(lncover ~ warmed + origin + (1|species), datau, REML=FALSE)
anova(mod3, mod2)
anova(mod2)
confint(mod2, method="boot", nsim=999)
mean(mod2)
summary(mod2)
difflsmeans(mod2, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(lncover ~ mammals + (1|species), datau, REML=FALSE)
modb <- lmer(lncover ~ (1|species), datau, REML=FALSE)
anova(modb, moda)


#warming effects each origin
exot.cov <- subset(datau, origin == 'Exotic')
test <- lm(lncover~warmed, data=exot.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(exot.cov$lncover)

nat.cov <- subset(datau, origin == 'Native')
test <- lm(lncover~warmed, data=nat.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(nat.cov$lncover)


#warming by species

Popr <- subset(datak, species == 'Popr')
boxplot(cover~warmed,data=Popr, main=expression(italic('Poa pratensis')), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Popr, method = "jitter", add = TRUE, pch = 17, col = 'black')


par(mfrow=c(1,1)) 
boxplot(cover~warmed*species, datau)
par(mfrow=c(3,3)) 

png(filename = "FigAppCc.png", width = 6, height = 8, units = "in", pointsize = 9, res=600)

par(mfrow=c(5,3), mar=c(2.5,2,2,1.5)) 
Cest <- subset(datau, species == 'Cest')
boxplot(cover~warmed,data=Cest, main=expression(italic("Centaurea stoebe")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Cest, method = "jitter", add = TRUE, pch = 17, col = 'black')
Popr <- subset(datau, species == 'Popr')
boxplot(cover~warmed,data=Popr, main=expression(italic("Poa pratensis")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Popr, method = "jitter", add = TRUE, pch = 17, col = 'black')
Ruac <- subset(datau, species == 'Ruac')
boxplot(cover~warmed,data=Ruac, main=expression(italic("Rumex acetosella")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Ruac, method = "jitter", add = TRUE, pch = 17, col = 'black')
Hype <- subset(datau, species == 'Hype')
boxplot(cover~warmed,data=Hype, main=expression(italic("Hypericum perforatum")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Hype, method = "jitter", add = TRUE, pch = 17, col = 'black')
Hipi <- subset(datau, species == 'Hipi')
boxplot(cover~warmed,data=Hipi, main=expression(italic("Hieracium pilosella")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Hipi, method = "jitter", add = TRUE, pch = 17, col = 'black')
Poco <- subset(datau, species == 'Poco')
boxplot(cover~warmed,data=Poco, main=expression(italic("Poa compressa")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Poco, method = "jitter", add = TRUE, pch = 17, col = 'black')
Piau <- subset(datau, species == 'Piau')
boxplot(cover~warmed,data=Piau, main=expression(italic("Pilosella aurantiaca")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Piau, method = "jitter", add = TRUE, pch = 17, col = 'black')
Trdu <- subset(datau, species == 'Trdu')
boxplot(cover~warmed,data=Trdu, main=expression(italic("Tragopogon dubius")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Trdu, method = "jitter", add = TRUE, pch = 17, col = 'black')

dev.off()

png(filename = "FigAppCd.png", width = 6, height = 8, units = "in", pointsize = 9, res=600)
par(mfrow=c(5,3), mar=c(2.5,2,2,1.5))

Dasp <- subset(datau, species == 'Dasp')
boxplot(cover~warmed,data=Dasp, main=expression(italic("Danthonia spicata")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Dasp, method = "jitter", add = TRUE, pch = 17, col = 'black')
Cape <- subset(datau, species == 'Cape')
boxplot(cover~warmed,data=Cape, main=expression(italic("Carex pensylvanica")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Cape, method = "jitter", add = TRUE, pch = 17, col = 'black')
Ptaq <- subset(datau, species == 'Ptaq')
boxplot(cover~warmed,data=Ptaq, main=expression(italic("Pteridium aquilinum")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Ptaq, method = "jitter", add = TRUE, pch = 17, col = 'black')
Quru <- subset(datau, species == 'Quru')
boxplot(cover~warmed,data=Quru, main=expression(italic("Quercus rubrum")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Quru, method = "jitter", add = TRUE, pch = 17, col = 'black')
Frve <- subset(datau, species == 'Frve')
boxplot(cover~warmed,data=Frve, main=expression(italic("Fragaria vesca")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Frve, method = "jitter", add = TRUE, pch = 17, col = 'black')
Vaan <- subset(datau, species == 'Vaan')
boxplot(cover~warmed,data=Vaan, main=expression(italic("Vaccinium angustifolium")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Vaan, method = "jitter", add = TRUE, pch = 17, col = 'black')
Sogi <- subset(datau, species == 'Sogi')
boxplot(cover~warmed,data=Sogi, main=expression(italic("Solidago gigantea")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Sogi, method = "jitter", add = TRUE, pch = 17, col = 'black')
Assy <- subset(datau, species == 'Assy')
boxplot(cover~warmed,data=Assy, main=expression(italic("Asclepias sp.")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Assy, method = "jitter", add = TRUE, pch = 17, col = 'black')
Besp <- subset(datau, species == 'Besp')
boxplot(cover~warmed,data=Besp, main=expression(italic("Betula sp.")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Besp, method = "jitter", add = TRUE, pch = 17, col = 'black')
Ansp <- subset(datau, species == 'Ansp')
boxplot(cover~warmed,data=Ansp, main=expression(italic("Antenarria sp.")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Ansp, method = "jitter", add = TRUE, pch = 17, col = 'black')
Sone <- subset(datau, species == 'Sone')
boxplot(cover~warmed,data=Sone, main=expression(italic("Solidago nemoralis")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Sone, method = "jitter", add = TRUE, pch = 17, col = 'black')
Acru <- subset(datau, species == 'Acru')
boxplot(cover~warmed,data=Acru, main=expression(italic("Acer rubrum")), names=c('Ambient', 'Warmed'), outline=FALSE)
stripchart(cover~warmed, vertical = TRUE, data = Acru, method = "jitter", add = TRUE, pch = 17, col = 'black')

dev.off()




####### FIGURES #######
#boxplot(cover~warmed*origin, datak)
png(filename = "Fig2axt.png", width = 2, height = 2, units = "in", pointsize = 12, res=800)
#par(mfrow=c(2,1), oma = c(1,2,5,1) + 0.1, mar = c(-15,4,4.5,4) + 0.1)
#par(mar=c(1,1,.5,0) + 0.1, oma = c(1,1,0,0) + 0.1)   
#Below for Y-Axis Label
par(mar=c(3,7,3,0) + 0.1, oma = c(1,1,0,0) + 0.1)   
##UMBS
aves <- tapply(datau$cover, list(datau$warmed, datau$origin), mean)
stdev <- tapply(datau$cover, list(datau$warmed, datau$origin), sd)
smple <- tapply(datau$cover, list(datau$warmed, datau$origin), length)
stderr <- (stdev/sqrt(smple)) 
stderr2 <- (stdev/sqrt(smple))*(-1) #make error bars negative
#barcenters <- barplot(aves, beside=T, ylab="Species percent cover", ylim=c(0,40), cex.lab=1.0, col=c("azure3", "white"), density=c(NA,NA))
barcenters <- barplot(aves, beside=T, ylab="Species cover (%)", yaxt="n", ylim=c(0,40), cex.lab=0.75, col=c("azure3", "white"))
yax <- (c(0,10,20,30,40))
axis(2, at=yax, las=2)
arrows(barcenters, aves, barcenters, aves+stderr, angle=90, length=0.05)
arrows(barcenters, aves, barcenters, aves+stderr2, angle=90, length=0.05)
#text(3.5, 50, "Northern Forest Clearing", cex=1.5)
#mtext("Northern Forest Clearing", side=3, line=1, cex=1.5)
dev.off()

##KBS
png(filename = "Fig2b.png", width = 2, height = 2, units = "in", pointsize = 12, res=800)
par(mar=c(1,1,.5,0) + 0.1, oma = c(1,1,0,0) + 0.1)   
aves <- tapply(datak$cover, list(datak$warmed, datak$origin), mean)
stdev <- tapply(datak$cover, list(datak$warmed, datak$origin), sd)
smple <- tapply(datak$cover, list(datak$warmed, datak$origin), length)
stderr <- (stdev/sqrt(smple)) 
stderr2 <- (stdev/sqrt(smple))*(-1) #make error bars negative
#barcenters <- barplot(aves, beside=T, ylim=c(0,45), cex.lab=1.0, col=c("azure3", "white"), density=c(NA,NA))
barcenters <- barplot(aves, beside=T, yaxt="n", ylim=c(0,40), cex.lab=1.0, col=c("azure3", "white"))
yax <- (c(0,10,20,30,40))
axis(2, at=yax, las=2)
arrows(barcenters, aves, barcenters, aves+stderr, angle=90, length=0.05)
arrows(barcenters, aves, barcenters, aves+stderr2, angle=90, length=0.05)
#text(3.5, 50, "Southern Old Agriculture Field", cex=1.5)
#mtext("Southern Old Agriculture Field", side=3, line=1, cex=1.5)
dev.off()

#difficulty putting legend in bottom outer margin. clipped in GIMP.
png(filename = "Fig2legend.png", width = 2, height = 3, units = "in", pointsize = 6, res=800)
par(mar=c(5,4,1,1) + 0.1, oma = c(10,1,3,1) + 0.1)   
aves <- tapply(datak$cover, list(datak$warmed, datak$origin), mean)
stdev <- tapply(datak$cover, list(datak$warmed, datak$origin), sd)
smple <- tapply(datak$cover, list(datak$warmed, datak$origin), length)
stderr <- (stdev/sqrt(smple)) 
stderr2 <- (stdev/sqrt(smple))*(-1) #make error bars negative
barcenters <- barplot(aves, beside=T, ylab="Species percent cover (%)", ylim=c(0,45), cex.lab=1.0, col=c("azure3", "white"), density=c(NA,NA))
arrows(barcenters, aves, barcenters, aves+stderr, angle=90, length=0.05)
arrows(barcenters, aves, barcenters, aves+stderr2, angle=90, length=0.05)
#text(3.5, 50, "Southern Old Agriculture Field", cex=1.5)
mtext("Southern Old Agriculture Field", side=3, line=1, cex=1.5)
par(xpd=TRUE)
legend(0.2, -5, legend=c("Ambient", "Warmed"), fill=c("azure3", "white"), density=c(NA,NA), horiz=T, cex=0.68)
par(xpd=FALSE)
dev.off()

########################################### TOTAL COVER BY ORIGIN 2016 ###########################################

k<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/kbscomp.csv", strip.white=T, na.strings="na", header = TRUE)
str(k)

hist(k$cover)
hist(k$lncover)

#first get summed cover for all plants in each plot 
#cov.sum = aggregate(cover ~ plot, data=k, FUN=sum, na.rm=T)
#names(cov.sum)[names(cov.sum)=="cover"]<-"cov.sum"
#head(cov.sum)

# merge back into data
#k<-merge(k,cov.sum, by=c("plot"))

#clean up dataframe
k$date <- NULL
k$julian <- NULL
k$X<- NULL


#merge in plot info
plotinfo<-read.csv("~/Data/Summer2015/Setup_Table.csv", header = TRUE)
str(plotinfo)
k2<-merge(k, plotinfo, by=c("plot"), all.x=T, all.y=T)
k2$all<-NULL

#merge in origin data
spinfo<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/KBS_Species_Traits_Comp.csv", header = TRUE)
final<-merge(k2, spinfo, by=c("species"), all.x=T, all.y=T)
str(final)

final$group <- NULL
final$family <- NULL
final$duration <- NULL
final$growth_habit <- NULL

#get summed cover by origin in each plot plot
exot.sum = aggregate(x=final$cover , by=final[,c("plot","origin")], FUN=sum, na.rm=T)
names(exot.sum)[names(exot.sum)=="cover"]<-"exot.sum"
head(exot.sum)
colnames(exot.sum) <- c("plot", "origin", "origin.sum")
# merge back with plot info
FINAL<-merge(plotinfo,exot.sum, by=c("plot"))
FINAL$all<-NULL

#only look at exotics
exot.cov <- subset(FINAL, origin == 'Exotic')
test <- lm(origin.sum~warmed, data=exot.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(exot.cov$origin.sum)

#means
warmed.exot <- subset(exot.cov, warmed == 'Y')
mean(warmed.exot$origin.sum)
se <- sd(warmed.exot$origin.sum, na.rm=TRUE) / sqrt(length(warmed.exot$origin.sum[!is.na(warmed.exot$origin.sum)])) 
amb.exot <- subset(exot.cov, warmed == 'N')
mean(amb.exot$origin.sum)

#look only at natives
nat.cov <- subset(FINAL, origin == 'Native')
test <- lm(origin.sum~warmed, data=nat.cov)
anova(test)
summary(test)


#####  UMBS 
u<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/umbscomp.csv", strip.white=T, na.strings="na", header = TRUE)
str(u)

#clean up dataframe
u$site <- NULL

#merge in plot info
plotinfo<-read.csv("~/Data/Summer2015/Setup_Table.csv", header = TRUE)
str(plotinfo)
u2<-merge(u, plotinfo, by=c("plot"), all.x=T, all.y=T)
u2$all<-NULL

#merge in origin data
spinfo<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/UMBS_Species_Traits_Comp.csv", header = TRUE)
final<-merge(u2, spinfo, by=c("species"), all.x=T, all.y=T)
str(final)

final$group <- NULL
final$family <- NULL
final$duration <- NULL
final$growth_habit <- NULL
final <- subset(final, species != 'Bareground')

#get summed cover by origin in each plot 
exot.sum = aggregate(x=final$cover , by=final[,c("plot","origin")], FUN=sum, na.rm=T)
names(exot.sum)[names(exot.sum)=="cover"]<-"exot.sum"
head(exot.sum)
colnames(exot.sum) <- c("plot", "origin", "origin.sum")
# merge back with plot info
FINAL<-merge(plotinfo,exot.sum, by=c("plot"))
FINAL$all<-NULL


#only look at exotics
exot.cov <- subset(FINAL, origin == 'Exotic')
test <- lm(origin.sum~warmed, data=exot.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(exot.cov$origin.sum)

#means
warmed.exot <- subset(exot.cov, warmed == 'Y')
mean(warmed.exot$origin.sum)
se <- sd(warmed.exot$origin.sum, na.rm=TRUE) / sqrt(length(warmed.exot$origin.sum[!is.na(warmed.exot$origin.sum)])) 
amb.exot <- subset(exot.cov, warmed == 'N')
mean(amb.exot$origin.sum)

#look only at natives
nat.cov <- subset(FINAL, origin == 'Native')
test <- lm(origin.sum~warmed, data=nat.cov)
anova(test)
summary(test)
########################################### TOTAL COVER BY ORIGIN 2015 ###########################################


###### KBS

all<-read.csv("~/Papers/Ch2_Herbivory_Paper/Data/combined_sites_spcomp15.csv", strip.white=T, na.strings="na", header = TRUE)
k <- subset(all, site=='kbs')
str(k)
#clean up dataframe
k$X <- NULL
k$group<- NULL
k$family<- NULL
k$duration<- NULL
k$growth_habit<- NULL
k$site <- NULL


#first get summed cover for all plants in each plot 
#cov.sum = aggregate(quad.mn ~ plot, data=k, FUN=sum, na.rm=T)
#names(cov.sum)[names(cov.sum)=="cover"]<-"quad.mn"
#head(cov.sum)
#colnames(cov.sum) <- c("plot","plot.cover")

# merge back into data
k<-merge(k,cov.sum, by=c("plot"))


#get summed cover by origin in each plot 
exot.sum = aggregate(x=k$quad.mn , by=k[,c("plot","origin")], FUN=sum, na.rm=T)
names(exot.sum)[names(exot.sum)=="cover"]<-"exot.sum"
head(exot.sum)
colnames(exot.sum) <- c("plot", "origin", "origin.sum")
# merge back with plot info
plotinfo<-read.csv("~/Data/Summer2015/Setup_Table.csv", header = TRUE)
FINAL<-merge(plotinfo,exot.sum, by=c("plot"))
FINAL$all<-NULL

#only look at exotics
exot.cov <- subset(FINAL, origin == 'Exotic')
test <- lm(origin.sum~warmed, data=exot.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(exot.cov$origin.sum)

#means
warmed.exot <- subset(exot.cov, warmed == 'Y')
mean(warmed.exot$origin.sum)
se <- sd(warmed.exot$origin.sum, na.rm=TRUE) / sqrt(length(warmed.exot$origin.sum[!is.na(warmed.exot$origin.sum)])) 
amb.exot <- subset(exot.cov, warmed == 'N')
mean(amb.exot$origin.sum)

#look only at natives
nat.cov <- subset(FINAL, origin == 'Native')
test <- lm(origin.sum~warmed, data=nat.cov)
anova(test)
summary(test)

#####  UMBS 
u <- subset(all, site=='umbs')
str(u)
#clean up dataframe
u$X <- NULL
u$group<- NULL
u$family<- NULL
u$duration<- NULL
u$growth_habit<- NULL
u$site <- NULL

str(u)



#get summed cover by origin in each plot 
exot.sum = aggregate(x=u$quad.mn , by=u[,c("plot","origin")], FUN=sum, na.rm=T)
names(exot.sum)[names(exot.sum)=="percent_cover"]<-"exot.sum"
head(exot.sum)
colnames(exot.sum) <- c("plot", "origin", "origin.sum")
# merge back with plot info
FINAL<-merge(plotinfo,exot.sum, by=c("plot"))
FINAL$all<-NULL


#only look at exotics
exot.cov <- subset(FINAL, origin == 'Exotic')
test <- lm(origin.sum~warmed, data=exot.cov)
anova(test)
summary(test)
confint(test, method="boot", nsim=999)
hist(exot.cov$origin.sum)

#means
warmed.exot <- subset(exot.cov, warmed == 'Y')
mean(warmed.exot$origin.sum)
se <- sd(warmed.exot$origin.sum, na.rm=TRUE) / sqrt(length(warmed.exot$origin.sum[!is.na(warmed.exot$origin.sum)])) 
amb.exot <- subset(exot.cov, warmed == 'N')
mean(amb.exot$origin.sum)

#look only at natives
nat.cov <- subset(FINAL, origin == 'Native')
test <- lm(origin.sum~warmed, data=nat.cov)
anova(test)
summary(test)
