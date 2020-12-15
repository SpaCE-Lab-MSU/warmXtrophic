## 2016 Leaf Herbivory Data Using Final Data (August 2016)
## KBS AND UMBS
## KBW 

#clear all existing data/graphics
rm(list=ls())
graphics.off()

########################################### MODELS ###########################################
data<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/herb_combined_site.csv", strip.white=T, header = TRUE)
data <- subset(data, insects == 'Y')


######################## KBS ########################
datak <- subset(data, site == 'kbs')

library(lme4)
library(MuMIn)
library(lmerTest)

#should plot be a random effect?
moda <- lmer(ln.eaten ~ warmed*origin + mammals + (1|species) + (1|plot), datak, REML=FALSE)
mod1 <- lmer(ln.eaten ~ warmed*origin + mammals + (1|species), datak, REML=FALSE)
anova(mod1,moda)

#Do we need mammals?
mod2 <- lmer(ln.eaten ~ warmed*origin + (1|species), datak, REML=FALSE)
anova(mod2,mod1)

#Do we need interaction term?
mod3 <- lmer(ln.eaten ~ warmed+origin + (1|species), datak, REML=FALSE)
anova(mod3, mod2)
anova(mod3)
summary(mod3)
confint(mod3, method="boot", nsim=999)
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(ln.eaten ~ origin + (1|species), datak, REML=FALSE)
modb <- lmer(ln.eaten ~ (1|species), datak, REML=FALSE)
anova(modb, moda)

######################## UMBS ########################
datau <- subset(data, site == 'umbs')

#should plot be a random effect?
moda <- lmer(ln.eaten ~ warmed*origin + mammals + (1|species) + (1|plot), datau, REML=FALSE)
mod1 <- lmer(ln.eaten ~ warmed*origin + mammals + (1|species), datau, REML=FALSE)
anova(mod1,moda)

#Do we need mammals?
mod1 <- lmer(ln.eaten ~ warmed*origin + mammals + (1|species) + (1|plot), datau, REML=FALSE)
mod2 <- lmer(ln.eaten ~ warmed*origin + (1|species) + (1|plot), datau, REML=FALSE)
anova(mod2,mod1)

#Do we need interaction term?
mod3 <- lmer(ln.eaten ~ warmed+origin + (1|species) + (1|plot), datau, REML=FALSE)
anova(mod3, mod2)
anova(mod2)
summary(mod3)
confint(mod2, method="boot", nsim=999)#set.seed=13
difflsmeans(mod3, test.effs=NULL, ddf="Satterthwaite")



########################################### FIGURE ###########################################
#par(mfrow=c(2,1), oma = c(1,1,1,1) + 0.1, mar = c(1,4,4,1) + 0.1)
#par(mfrow=c(2,1), oma = c(1,1,1,1) + 0.1, mar = c(1,4,4,1) + 0.1)

png(filename = "Fig3ax.png", width = 2, height = 2, units = "in", pointsize = 12, res=800)
#par(mfrow=c(2,1), oma = c(1,2,5,1) + 0.1, mar = c(-15,4,4.5,4) + 0.1)
#par(mar=c(1,1,.5,0) + 0.1, oma = c(1,1,0,0) + 0.1)  
#Below for Y-Axis Label
par(mar=c(3,7,3,0) + 0.1, oma = c(1,1,0,0) + 0.1)   
#UMBS
#boxplot(p_eaten~warmed*origin, data)
aves <- tapply(datau$p_eaten, list(datau$warmed, datau$origin), mean)
stdev <- tapply(datau$p_eaten, list(datau$warmed, datau$origin), sd)
smple <- tapply(datau$p_eaten, list(datau$warmed, datau$origin), length)
stderr <- (stdev/sqrt(smple)) #make error bars negative
stderr2 <- (stdev/sqrt(smple))*(-1)
barcenters <- barplot(aves, beside=T, ylab="Leaf eaten (%)", yaxt="n", ylim=c(0,12), cex.lab=0.75, col=c("azure3", "white"))
yax <- (c(0,2,4,6,8,10,12))
axis(2, at=yax, las=2)
arrows(barcenters, aves, barcenters, aves+stderr, angle=90, length=0.05)
arrows(barcenters, aves, barcenters, aves+stderr2, angle=90, length=0.05)
#text(3.5, 13, "Northern Forest Clearing", cex=1.5)
#mtext("Northern Forest Clearing", side=3, line=1, cex=1.5)
par(xpd=TRUE)
dev.off()
#legend(2.0, 14.0, legend=c("Ambient", "Warmed"), col=c("#4575b4","#d73027"), pch=19, horiz=T, seg.len=3)

#KBS
png(filename = "Fig3b.png", width = 2, height = 2, units = "in", pointsize = 12, res=800)
#par(mfrow=c(2,1), oma = c(1,2,5,1) + 0.1, mar = c(-15,4,4.5,4) + 0.1)
par(mar=c(1,1,.5,0) + 0.1, oma = c(1,1,0,0) + 0.1)   
aves <- tapply(datak$p_eaten, list(datak$warmed, datak$origin), mean)
stdev <- tapply(datak$p_eaten, list(datak$warmed, datak$origin), sd)
smple <- tapply(datak$p_eaten, list(datak$warmed, datak$origin), length)
stderr <- (stdev/sqrt(smple)) #make error bars negative
stderr2 <- (stdev/sqrt(smple))*(-1)
#barcenters <- barplot(aves, beside=T, ylab="Percent Leaf Eaten", ylim=c(0,8), cex.lab=1, col=c("azure3", "black"),density=c(NA,10))
barcenters <- barplot(aves, beside=T, ylab="Percent leaf eaten (%)", yaxt="n", ylim=c(0,8), cex.lab=0.75, col=c("azure3", "white"))
yax <- (c(0,2,4,6,8))
axis(2, at=yax, las=2)
arrows(barcenters, aves, barcenters, aves+stderr, angle=90, length=0.05)
arrows(barcenters, aves, barcenters, aves+stderr2, angle=90, length=0.05)
#text(3.5, 9, "Southern Old Agriculture Field", cex=1.5)
#mtext("Southern Old Agriculture Field", side=3, line=1, cex=1.5)
par(xpd=TRUE)
dev.off()
#legend(2.0, 14.0, legend=c("Ambient", "Warmed"), col=c("#4575b4","#d73027"), pch=19, horiz=T, seg.len=3)


dev.off()

########################################### Individual Histograms ###########################################

#what are species at kbs
unique(datak$species)

par(mfrow=c(3,2))

hipr<- subset(datak, species=='Hipr')
hist(hipr$p_eaten, breaks=15, main='Hieracium')
phpr<- subset(datak, species=='Phpr')
hist(phpr$p_eaten, breaks=15, main='Phleum')
popr<- subset(datak, species=='Popr')
hist(popr$p_eaten, breaks=15, main='Poa pratensis')
soca<- subset(datak, species=='Soca')
hist(soca$p_eaten, breaks=15, main='Solidago canadensis')
pore<- subset(datak, species=='Pore')
hist(pore$p_eaten, breaks=15, main='Potentilla recta')
trpr<- subset(datak, species=='Trpr')
hist(trpr$p_eaten, breaks=15, main='Trifolium pratense')
eugr<- subset(datak, species=='Eugr')
hist(eugr$p_eaten, breaks=15, main='Euthamia graminifolia')
alpe<- subset(datak, species=='Alpe')
hist(alpe$p_eaten, breaks=15, main='Alliaria')
cest<- subset(datak, species=='Cest')
hist(cest$p_eaten, breaks=15, main='Centauria')
hype<- subset(datak, species=='Hype')
hist(hype$p_eaten, breaks=15, main='Hypericum')
trre<- subset(datak, species=='Trre')
hist(trre$p_eaten, breaks=15, main='Trifolium recta')
ceor<- subset(datak, species=='Ceor')
hist(ceor$p_eaten, breaks=15, main='Celastrus')
rual<- subset(datak, species=='Rual')
hist(rual$p_eaten, breaks=15, main='Rubus')

#try taking out grasses
par(mfrow=c(2,1))
dataknograss <- subset(datak, species != 'Phpr')
dataknograss <- subset(dataknograss , species != 'Popr')

hist(dataknograss$p_eaten, breaks=45, xlim=c(0,10), main='No grasses- raw data')
hist(dataknograss$ln.eaten)

ln0.1<- log(dataknograss$p_eaten + 0.1)
hist(ln0.1, breaks=15, main='No grasses- ln(x+0.1)')

