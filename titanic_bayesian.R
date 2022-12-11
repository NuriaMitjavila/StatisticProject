library(tidyverse)
library(brms)
library(ggridges)
library(shinystan)
library(bayesplot)
library(tidybayes)
library(ggmcmc)
library(gmodels)
require(brms)

head(titanic)
summary(titanic)

table(titanic$Pclass) #  1   2   3 : 323 277 709
table(titanic$Sex) #  0   1 : 843 466 
table(titanic$Embarked) #  0   1   2 : 270 123 914 
table(titanic$X2urvived) # 0   1 : 967 342
table(titanic$Passengerid)

correlation = cor(titanic, titanic$X2urvived)
correlation

plot(correlation) #higher correlation: class, sex and fare

variables <- c("X2urvived","Pclass", "Sex", "Fare")

newdataset = titanic[variables]

head(newdataset)

densitysurvived <- density(newdataset$X2urvived)
plot(densitysurvived, main='Survival' )

densitysex <- density(newdataset$Sex)
plot(densitysex, main='Sex' )

densitypclass <- density(newdataset$Pclass)
plot(densitypclass, main='Pclass' )

densityfare <- density(newdataset$Fare)
plot(densityfare, main='Fare' )

tabsex <- table(newdataset$X2urvived,newdataset$Sex)
CrossTable(tabsex) #to have row total, column total and table total
CrossTable(tabsex, prop.chisq=FALSE, prop.r = FALSE, prop.t = FALSE) #using all the =FALSE for having only data of 


tabpclass <- table(newdataset$X2urvived,newdataset$Pclass)
CrossTable(tabpclass)
CrossTable(tabpclass, prop.chisq=FALSE, prop.r = FALSE, prop.t = FALSE)

#tabfare <- table(newdataset$X2urvived,newdataset$Fare) #need to be careful with its output!!
#CrossTable(tabfare)
CrossTable(tabfare, prop.chisq=FALSE, prop.r = FALSE, prop.t = FALSE)

fitted <- brm(X2urvived ~ Sex + Pclass + Fare, 
            family = bernoulli(), data=newdataset, 
            chains = 2, iter = 2000, refresh = 0)
plot(fitted)
summary(fitted)

