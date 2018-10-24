# Updated Oct. 10, 2018 with merged data from 2015-2018
# Origin of input file: data_prep_2015_2018.R

rm(list = ls())

# set working directory (if you're not KS, change this to the correct path for yourself)
# Google Drive directory. It should point to where we have /final_data
setwd("/Volumes/GoogleDrive/My Drive/MIWarmHerb_FieldExperiment/data/final_data")

#read in ONLY the data of interest:
data <- read.csv("herb1518.csv")

#examine if insectide plot differ from control plots -chi-squared?

#use only data from plots NOT treated with insecticide
data <- subset(data, insects == "Y")
data <- droplevels(data)
str(data)
#a little exploration (almost all exotic spp at KBS!)
aggregate(data$origin, by = list(data$species), unique)

#exclude graminoids
data <- subset(data, growth_habit !='Graminoid')
                  
library(lme4)
library(MuMIn)
library(lmerTest)

#look at univariate distributions
hist(data$p_eaten, breaks = 100, right = F)
hist(log(data$p_eaten + 1), right = F, breaks = 20)
sqrtrnsfm <- function(x) {asin(sqrt(x/100))} 
hist(sqrtrnsfm(data$p_eaten), right = F)#no transformation will work...try using individual level random effect to account for overdispersion.

#make some handy columns for the analysis:
data$eaten_binary <- ifelse(data$p_eaten > 0, 1, 0)
data$plant_id <- paste(data$plot, data$species, data$plant_number, sep="_")
str(data)

########################################################
#########################################################
#analysis of individual leaves (amount eaten and prob attacked):
#### AMOUNT EATEN
#without individual Random Effect residuals look overdispersed:
m0 <- glmer(p_eaten ~ mammals + warmed * origin + (1|species), data=data, family = "poisson")
plot(residuals(m0))
summary(m0)
#with individual Random Effect they look OK (note one outlier in UMBS data):
m1 <- glmer(p_eaten ~ mammals + warmed * origin + (1|species) + (1|plant_id), data=data, family = "poisson")
plot(residuals(m1))
summary(m1)

anova(m0, m1) #individual-level Random Effect is better, but we knew that anyway.
#Does 'mammals' belong in the model?
m2 <- glmer(p_eaten ~ warmed * origin + (1|species) + (1|plant_id), data=data, family = "poisson")
plot(residuals(m2))
summary(m2)
anova(m2, m1) #No, mammals does not belong.

#testing interaction term
m3 <- glmer(p_eaten ~ warmed + origin + (1|species) + (1|plant_id), data=data, family = "poisson")
plot(residuals(m3))
summary(m3)
anova(m3, m2) #No, interaction does not belong at KBS or UMBS.
confint(m3, method="boot", nsim=999)


warnings()#interpreting model coefficients at KBS:
> exp(-0.7)
[1] 0.4965853  #warming decreases herbivory by almost half.
> exp(1.8)
[1] 6.049647 #native species experience 6-fold greater herbivory than exotic species

#interpreting model coefficients at UMBS:
> exp(0.6788)
[1] 1.97151  #warming increases herbivory by almost two-fold.

#Test for Phoebe Shock Experiment
moda <- glmer(p_eaten ~ mammals + (1|species) + (1|plant_id), data=data, family = "poisson")
modb <- glmer(p_eaten ~ (1|species)+ (1|plant_id), data=data, family = "poisson")
anova(modb, moda)

######################################################
#######################################################

#Alternative method not in manuscript:
#### PROBABILITY OF BEING ATTACKED
#without individual Random Effect residuals look slightly overdispersed:
M0 <- glmer(eaten_binary ~ mammals + warmed * origin  + (1|species), data=data, family = "binomial")
plot(residuals(M0))
summary(M0)

#with individual Random Effect they look OK:
M1 <- glmer(eaten_binary ~ mammals + warmed * origin + (1|species) + (1|plant_id), data=data, family = "binomial")
plot(residuals(M1))
summary(M1)
anova(M0, M1) #individual-level Random Effect is better, but we knew that anyway. At KBS and UMBS.

#Does 'mammals' belong in the model?
M2 <- glmer(eaten_binary ~ warmed * origin + (1|species) + (1|plant_id), data=data, family = "binomial")
plot(residuals(M2))
summary(M2)
anova(M2, M1) #No, mammals does not belong at KBS. Or UMBS

#testing interaction term
M3 <- glmer(eaten_binary ~ warmed + origin + (1|species) + (1|plant_id), data=data, family = "binomial")
plot(residuals(M3))
summary(M3)
anova(M3, M2) 
anova(M3)
confint(M3, method="boot", nsim=999)
#KBS: interaction does not belong but M3 (additive effects of herbovry and origin) is most parsimonious model. Native are more likely to be attacked, and herbivory decreases with warming.
#UMBS- no interaction. Intercept only is actually best.

#Test for Phoebe Shock Experiment
moda <- glmer(eaten_binary ~ mammals + (1|species) + (1|plant_id), data=data, family = "binomial")
modb <- glmer(eaten_binary ~ (1|species)+ (1|plant_id), data=data, family = "binomial")
anova(modb, moda)

#######################################################
#######################################################
#USING MEAN PERCENT EATEN FOR EACH PLANT.
#data prep
df2 <- aggregate(data$p_eaten, by = list(data$species, data$plot,data$plant_number), mean)
names(df2) <- c("species", "plot", "plant_num","eaten")
df3 <- aggregate(data$warmed, by = list(data$species, data$plot,data$plant_number), unique)
names(df3) <- c("species", "plot", "plant_num","warmed")
df4 <- aggregate(data$origin, by = list(data$species, data$plot,data$plant_number), unique)
names(df4) <- c("species", "plot", "plant_num","origin")
df2 <- merge(df2, df3, by = c("species", "plot","plant_num"), all=T)
df2 <- merge(df2, df4, by = c("species", "plot","plant_num"), all=T)
#make some handy columns for the analysis:
df2$eaten_binary <- ifelse(df2$eaten > 0, 1, 0)
df2$plant_id <- paste(df2$plot, df2$species, df2$plant_num, sep="_")

str(df2)

#Binary analysis - probability an individual plant is attacked:
plot(df2$eaten_binary)
hist(df2$eaten_binary)

m_0 <- glmer(eaten_binary ~  warmed * origin + (1|species), data=df2, family = "binomial") ##this error message means results are nonsensical and cannot be trusted. Note the wacky StDevs and p-values. 
plot(residuals(m_0))

m_1 <- glmer(eaten_binary ~ warmed * origin + (1|species) + (1|plot), data=df2, family = "binomial") ##this error message means results are nonsensical and cannot be trusted. Note the wacky StDevs and p-values. 
plot(residuals(m_1))

m_2 <- glmer(eaten_binary ~ warmed  + (1|species) + (1|plot), data=df2, family = "binomial")
plot(residuals(m_2))

m_3 <- glmer(eaten_binary ~ origin  + (1|species) + (1|plot), data=df2, family = "binomial")
plot(residuals(m_3))

m_4 <- glmer(eaten_binary ~ (1|plot) + (1|species), data=df2, family = "binomial")
plot(residuals(m_4))

anova(m_2, m_4) #this indicates no difference in probability of being attacked in warmed vs. ambient chambers.
anova(m_3, m_4) #this indicates that native plants are more likely to be attacked than exotic plants.

#############
#FROM http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html


overdisp_fun <- function(model) {
    ## number of variance parameters in an n-by-n variance-covariance matrix
    vpars <- function(m) {
        nrow(m) * (nrow(m) + 1)/2
    }
    # The next two lines calculate the residual degrees of freedom
    model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
    rdf <- nrow(model.frame(model)) - model.df
    # extracts the Pearson residuals
    rp <- residuals(model, type = "pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    # Generates a p-value. If less than 0.05, the data are overdispersed.
    pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
    c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

