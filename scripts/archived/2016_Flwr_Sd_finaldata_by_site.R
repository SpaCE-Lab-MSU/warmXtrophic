#clear all existing data/graphics
rm(list=ls())
graphics.off()

########################################### MODELS ###########################################

######################## KBS ########################
datak<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/KBS_flwr_sd_median_final.csv", strip.white=T, header = TRUE)
datak <- na.omit(datak)

library(lme4)
library(MuMIn)
library(lmerTest)

#flowerdate

#should plot be a random effect?
moda <- lmer(flowerdate ~ warmed*origin + mammals + insects + (1|species) + (1|plot), datak, REML=FALSE)
mod1 <- lmer(flowerdate ~ warmed*origin + mammals + insects+ (1|species), datak, REML=FALSE)
anova(mod1,moda)

#Do we need mammals?
mod2 <- lmer(flowerdate ~ warmed*origin + insects + (1|species), datak, REML=FALSE)
anova(mod2,mod1)

#Do we need insects?
mod3 <- lmer(flowerdate ~ warmed*origin + (1|species), datak, REML=FALSE)
anova(mod3,mod2)

#Do we need interaction term?
mod4 <- lmer(flowerdate ~ warmed+origin + (1|species), datak, REML=FALSE)
anova(mod4, mod3)
anova(mod4)
summary(mod4)
confint(mod4, method="boot", nsim=999)
difflsmeans(mod4, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(flowerdate ~ mammals + (1|species), datak, REML=FALSE)
modb <- lmer(flowerdate ~ (1|species), datak, REML=FALSE)
anova(modb, moda)

##FIGURE
library(ggplot2)
p_emerg <- ggplot(aes(y = flowerdate, x = origin, fill = warmed), data = datak) +
  geom_boxplot(position = position_dodge(width = .9))+ coord_flip() +
  theme(legend.position = "none", panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white"))+
  labs(x="", y="Julian Day") +
  ggtitle("Median Date of Flowering")
p_emerg
ggsave('U_flwr.png', p_emerg, height=2, width=3, dpi=600)


#seed date

#should plot be a random effect?
moda <- lmer(seeddate ~ warmed*origin + mammals + insects + (1|species) + (1|plot), datak, REML=FALSE)
mod1 <- lmer(seeddate ~ warmed*origin + mammals + insects + (1|species), datak, REML=FALSE)
anova(mod1,moda)

#Do we need mammals?
mod2 <- lmer(seeddate ~ warmed*origin + insects + (1|species), datak, REML=FALSE)
anova(mod2,mod1)

#Do we need insects?
mod3 <- lmer(seeddate ~ warmed*origin + (1|species), datak, REML=FALSE)
anova(mod3,mod2)

#Do we need interaction term?
mod4 <- lmer(seeddate ~ warmed+origin + (1|species), datak, REML=FALSE)
anova(mod4, mod3)
anova(mod4)
summary(mod4)
confint(mod4, method="boot", nsim=999)
difflsmeans(mod4, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(seeddate ~ mammals + (1|species), datak, REML=FALSE)
modb <- lmer(seeddate ~ (1|species), datak, REML=FALSE)
anova(modb, moda)

library(ggplot2)
p_emerg <- ggplot(aes(y = seeddate, x = origin, fill = warmed), data = datak) +
  geom_boxplot(position = position_dodge(width = .9))+ coord_flip() +
  theme(legend.position = "none", panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white"))+
  labs(x="", y="Julian Day") +
  ggtitle("Median Date of Seed Set")
p_emerg
ggsave('U_sd.png', p_emerg, height=2, width=3, dpi=600)


######################## UMBS ########################
datau<-read.csv("C:/Users/Garrett/Desktop/Ki_Oecologia_Paper/UMBS_flwr_sd_median_final.csv", strip.white=T, header = TRUE)


library(lme4)
library(MuMIn)
library(lmerTest)

#flowerdate

#should plot be a random effect?
moda <- lmer(flowerdate ~ warmed*origin + mammals + insects + (1|species) + (1|plot), datau, REML=FALSE)
mod1 <- lmer(flowerdate ~ warmed*origin + mammals + insects+ (1|species), datau, REML=FALSE)
anova(mod1,moda)

#Do we need mammals?
mod2 <- lmer(flowerdate ~ warmed*origin + insects + (1|species), datau, REML=FALSE)
anova(mod2,mod1)

#Do we need insects?
mod3 <- lmer(flowerdate ~ warmed*origin + (1|species), datau, REML=FALSE)
anova(mod3,mod2)

#Do we need interaction term?
mod4 <- lmer(flowerdate ~ warmed+origin + insects + (1|species), datau, REML=FALSE)
anova(mod4, mod2)
anova(mod4)
summary(mod4)
confint(mod4, method="boot", nsim=999)
difflsmeans(mod4, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(flowerdate ~ mammals + (1|species), datau, REML=FALSE)
modb <- lmer(flowerdate ~ (1|species), datau, REML=FALSE)
anova(modb, moda)


##FIGURE
library(ggplot2)
p_emerg <- ggplot(aes(y = flowerdate, x = origin, fill = warmed), data = datau) +
  geom_boxplot(position = position_dodge(width = .9))+ coord_flip() +
  theme(legend.position = "none", panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white"))+
  labs(x="", y="Julian Day") +
  ggtitle("Median Date of Flowering")
p_emerg
ggsave('U_flwr.png', p_emerg, height=2, width=3, dpi=600)

#seed date

#should plot be a random effect?
moda <- lmer(seeddate ~ warmed*origin + mammals + insects + (1|species) + (1|plot), datau, REML=FALSE)
mod1 <- lmer(seeddate ~ warmed*origin + mammals + insects + (1|species), datau, REML=FALSE)
anova(mod1,moda)

#Do we need mammals?
mod2 <- lmer(seeddate ~ warmed*origin + insects + (1|species), datau, REML=FALSE)
anova(mod2,mod1)

#Do we need insects?
mod3 <- lmer(seeddate ~ warmed*origin + (1|species), datau, REML=FALSE)
anova(mod3,mod2)

#Do we need interaction term?
mod4 <- lmer(seeddate ~ warmed+origin + (1|species), datau, REML=FALSE)
anova(mod4, mod3)
anova(mod4)
summary(mod4)
confint(mod4, method="boot", nsim=999)
difflsmeans(mod4, test.effs=NULL, ddf="Satterthwaite")

#Test for Phoebe Shock Experiment
moda <- lmer(seeddate ~ mammals + (1|species), datau, REML=FALSE)
modb <- lmer(seeddate ~ (1|species), datau, REML=FALSE)
anova(modb, moda)

library(ggplot2)
p_emerg <- ggplot(aes(y = seeddate, x = origin, fill = warmed), data = datau) +
  geom_boxplot(position = position_dodge(width = .9))+ coord_flip() +
  theme(legend.position = "none", panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white"))+
  labs(x="", y="Julian Day") +
  ggtitle("Median Date of Seed Set")
p_emerg
ggsave('U_sd.png', p_emerg, height=2, width=3, dpi=600)

#legend
p_emerg <- ggplot(aes(y = seeddate, x = origin, fill = warmed), data = datau) +
  geom_boxplot(position = position_dodge(width = .9))+ coord_flip() +
  theme(legend.position = "bottom", legend.title=element_blank(), panel.background=element_blank(),  strip.background = element_blank(), panel.border = element_rect(fill=NA, colour = "black"), plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("azure3", "white", "azure3","white"))+
  labs(x="", y="Julian Day") +
  ggtitle("Median Date of Seed Set")
p_emerg
ggsave('U_sd.png', p_emerg, height=2, width=3, dpi=600)
