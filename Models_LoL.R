#### FINAL MODELS ###
 # FIRST ENCOUNTER
setwd("")

library(lme4)
library(dplyr)
library(ggplot2)
#library(effects)


# First model -------------------------------------------------------------


#Model first encounter location vs. rest habitat inside 1745 m radius buffer
first<-read.csv("ModelFirst.csv", dec=".", sep=";")
first$bear<-as.factor(first$bear)
table(first$Habitat2)
# Include scaled variables in the dataframe instead of in the model structure
first$Buildings_scaled<-scale(first$Buildings, center= 0)
first$Settlement_scaled<-scale(first$Settlement, center= 0)
first$MajorRoads_scaled<-scale(first$MajorRoads, center= 0) 
first$MinorRoads_scaled<-scale(first$MinorRoads, center= 0)
first$DEM_scaled<-scale(first$DEM, center= 0)
first$Ruggedness_scaled<-scale(first$Ruggedness, center= 0)
first$Habitat2<-as.factor(first$Habitat2)

first$midagef  <- 0
first$midagef[first$Habitat2  == "amidage_forest" ]  <- 1
first$bog  <- 0
first$bog[first$Habitat2  == "bog" ]  <- 1
first$youngf  <- 0
first$youngf[first$Habitat2  == "young_forest" ]  <- 1
first$oldf  <- 0
first$oldf[first$Habitat2  == "old_forest" ]  <- 1
first$clearcut  <- 0
first$clearcut[first$Habitat2  == "clearcut" ]  <- 1
table(first$midagef)

#Summary

firstused<-subset(first, bear == 1)
firstavailable<-subset(first, bear == 0)


mfirst1<-glmer(bear~ (1) +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst2<-glmer(bear~ scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)   + scale(MinorRoads, center=0)+(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst3<-glmer(bear~ as.factor(youngf) + as.factor(midagef) + as.factor(oldf) + as.factor(bog) + as.factor(clearcut) +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst4<-glmer(bear~ scale(DEM, center=0) + scale(Ruggedness, center=0) +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst5<-glmer(bear~ as.factor(youngf) + as.factor(midagef) + as.factor(oldf) + as.factor(bog) + as.factor(clearcut) + scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0) + scale(MinorRoads, center=0) +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst6<-glmer(bear~ scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)  + scale(MinorRoads, center=0) + scale(DEM, center=0) + scale(Ruggedness, center=0) +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst7<-glmer(bear~ as.factor(youngf) + as.factor(midagef) + as.factor(oldf) + as.factor(bog) + as.factor(clearcut) + scale(DEM, center=0) + scale(Ruggedness, center=0) +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail",
               control = glmerControl(calc.derivs = FALSE))
mfirst8<-glmer(bear~as.factor(youngf) + as.factor(midagef) + as.factor(oldf) + as.factor(bog) + as.factor(clearcut) + scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)
               + scale(MinorRoads, center=0) + scale(DEM, center=0) + scale(Ruggedness, center=0)  +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail", 
               control = glmerControl(calc.derivs = FALSE))

mfirst8<-glmer(bear~as.factor(Habitat2) + scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)
               + scale(MinorRoads, center=0) + scale(DEM, center=0) + scale(Ruggedness, center=0)  +(1|enctid) + (1|male) + (1|female)
               ,data=first, family="binomial", na.action="na.fail", 
               control = glmerControl(calc.derivs = FALSE))

anova(mfirst1,mfirst2,mfirst3,mfirst4,mfirst5,mfirst6,mfirst7, mfirst8)

summary(mfirst8)


#### Path models ------------------------------------------------------------
setwd("C:/Users/aleja/Dropbox (SCENIC MNCN CSIC)/Alejandra/Universidad/Osos/Estancia Noruega/Manuscript/Submission")
# PATHS

first1<-read.csv("ModelPaths.csv", dec=".", sep=";")

first1$sex1[first1$sex  == "F"]  <- 0
first1$sex1[first1$sex  == "M"]  <- 1

first1$bear[first1$path  == "single" ]  <- 1
first1$bear[first1$path  == "mating" ]  <- 0



first1$midagef  <- 0
first1$midagef[first1$Habitat2  == "amidage_forest" ]  <- 1
first1$bog  <- 0
first1$bog[first1$Habitat2  == "bog" ]  <- 1
first1$youngf  <- 0
first1$youngf[first1$Habitat2  == "young_forest" ]  <- 1
first1$oldf  <- 0
first1$oldf[first1$Habitat2  == "old_forest" ]  <- 1
first1$clearcut  <- 0
first1$clearcut[first1$Habitat2  == "clearcut" ]  <- 1
table(first1$midagef)

first1$length <- scale(first1$interval, center = TRUE, scale = FALSE)


path<-na.omit(first1)

#ALL mating paths, single (1) vs. mating (0)
mpaths1<-glmer(bear~sex*(1) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))
mpaths2<-glmer(bear~sex*(scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)
                         + scale(MinorRoads, center=0)) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))
mpaths3<-glmer(bear~sex*(Habitat2) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))
mpaths4<-glmer(bear~sex*(scale(DEM, center=0) + scale(Ruggedness, center=0)) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))
mpaths5<-glmer(bear~sex*(Habitat2 + scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)
                         + scale(MinorRoads, center=0)) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))
mpaths6<-glmer(bear~sex*(scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0)
                         + scale(MinorRoads, center=0) + scale(DEM, center=0) + scale(Ruggedness, center=0)) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))
mpaths7<-glmer(bear~sex*(Habitat2 + scale(DEM, center=0) + scale(Ruggedness, center=0)) +(1|pathid) + (1|idindividual)
              ,data=path, family="binomial", na.action="na.fail",
              control = glmerControl(calc.derivs = FALSE))


mpaths8<-glmer(bear~sex*(as.factor(Habitat2) + scale(Buildings, center=0)+ scale(Settlement, center=0)+ scale(MajorRoads, center=0) + scale(MinorRoads, center=0) + scale(DEM, center=0) + scale(Ruggedness, center=0)) + length +(1|pathid) + (1|idindividual)
                  ,data=path, family="binomial", na.action="na.fail",
                  control = glmerControl(calc.derivs = FALSE))


anova(mpaths1,mpaths2,mpaths3,mpaths4,mpaths5,mpaths6,mpaths7, mpaths8)

summary(mpaths8)
b 

