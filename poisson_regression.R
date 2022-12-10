# R SCRIPT 2. POISSON REGRESSION ANALYSIS
rm(list=ls())
titanic <- read.csv("~/Downloads/stats.project/titanic.csv")
attach(titanic)
model1 <- glm(X2urvived~Sex,family=poisson(link="log")) # check predictions one by one
model2 <- glm(X2urvived~Pclass,family=poisson(link="log")) # check predictions one by one
model3 <- glm(X2urvived~Embarked,family=poisson(link="log")) # check predictions one by one
final.model <- glm(X2urvived~.-Parch-Fare-Embarked, family=quasipoisson(link="log"), data=titanic)

# Deviance residuals according to the first model and sex
Man <- titanic[titanic$Sex==0,]
Woman <- titanic[titanic$Sex==1,]
Man <- Man[,-1]
Woman <- Woman[,-1]
model1.1 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Man)
summary(model1.1) # performance a little bit significant
model1.2 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Woman)
summary(model1.2) # performance a little bit significant
boxplot(Fare~Sex,data=titanic, col=c("green","red"))
n <- nrow(titanic)
color <- rep(NA, n)
color[titanic$Sex==0] <- "green"
color[titanic$Sex==1] <- "red"
plot(Age, Fare, col=color,main="Divided by sex") # we observe a strong effect for the school
legend("topright",c("Man","Woman"),col=c("green","red"),pch=1)

# Deviance residuals according to the first model and class
Upper <- titanic[titanic$Pclass==1,]
Middle <- titanic[titanic$Pclass==2,]
Lower <- titanic[titanic$Pclass==3,]
Upper <- Upper[,-1]
Middle <- Middle[,-1]
Lower <- Lower[,-1]
model2.1 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Upper)
summary(model2.1) # performance a little bit significant
model2.2 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Middle)
summary(model2.2) # performance a little bit significant
model2.3 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Lower)
summary(model2.3) # performance a little bit significant
boxplot(Fare~Pclass,data=titanic, col=c("green","red","blue"))
n <- nrow(titanic)
color <- rep(NA,n)
color[titanic$Pclass==1] <- "green"
color[titanic$Pclass==2] <- "red"
color[titanic$Pclass==3] <- "blue"
plot(Age, Fare, col=color,main="Divided by class") # we observe a strong effect for the school
legend("topright",c("Upper","Middle","Lower"),col=c("green","red","blue"),pch=1)

# Deviance residuals according to the first model and embarked
Cherbourg <- titanic[titanic$Embarked==0,]
Queenstown <- titanic[titanic$Embarked==1,]
Southampton <- titanic[titanic$Embarked==2,]
Cherbourg <- Cherbourg[,-1]
Queenstown <- Queenstown[,-1]
Southampton <- Southampton[,-1]
model3.1 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Cherbourg)
summary(model3.1) # performance a little bit significant
model3.2 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Queenstown)
summary(model3.2) # performance a little bit significant
model3.3 <- glm(Fare~Age, family=quasipoisson(link="log"), data=Southampton)
summary(model3.3) # performance a little bit significant
boxplot(Fare~Embarked,data=titanic, col=c("green","red","blue"))
n <- nrow(titanic)
color <- rep(NA,n)
color[titanic$Embarked==0] <- "green"
color[titanic$Embarked==1] <- "red"
color[titanic$Embarked==2] <- "blue"
plot(Age, Fare, col=color,main="Divided by emarked") # we observe a strong effect for the school
legend("topright",c("Cherbourg","Queenstown","Southampton"),col=c("green","red","blue"),pch=1)

# Deviance residuals according to the first model and embarked
no <- titanic[titanic$X2urvived==0,]
yes <- titanic[titanic$X2urvived==1,]
no <- no[,-1]
yes <- yes[,-1]
model4.1 <- glm(Fare~Age, family=quasipoisson(link="log"), data=no)
summary(model4.1) # performance a little bit significant
model4.2 <- glm(Fare~Age, family=quasipoisson(link="log"), data=yes)
summary(model4.2) # performance a little bit significant
boxplot(Fare~X2urvived,data=titanic, col=c("green","red"))
n <- nrow(titanic)
color <- rep(NA,n)
color[titanic$X2urvived==0] <- "green"
color[titanic$X2urvived==1] <- "red"
plot(Age, Fare, col=color,main="Divided by emarked") # we observe a strong effect for the school
legend("topright",c("no","yes"),col=c("green","red"),pch=1)







#----------------------DONE-------------------------

# Barplots of quantitative data, Age and Fare
barplot(table(Age), main="Barplot of the Age table", col="lightgreen")
barplot(table(Fare), main="Barplot of the Fare table", col="lightgreen")
pairs(titanic)  # observe the correlation

# Show the relationship between survived and age in a scatter plot.
quAge <- unique(quantile(Age,probs=seq(0.1,0.9,by=0.1)))
miAge <- min(Age) # to be sure that you have all the date range, find the min and max
maAge <- max(Age)
breakpoints <- c(miAge-1, quAge, maAge+1) # join all the data together
Age.cat <- cut(Age, breakpoints) # categorical variable for all the data
table(Age.cat) # we do a table to observe our categorical data
midpointsAge <- tapply(Age, Age.cat, mean) # do the function for the math grade
meandayabsAge <- tapply(X2urvived, Age.cat, mean) # do the function for the days absent
plot(midpointsAge, meandayabsAge) # plot both variables to observe the tendency

# Show the relationship between survived and fare in a scatter plot.
quFare <- quantile(Fare,probs=seq(0.1,0.9,by=0.1)) 
miFare <- min(Fare) # to be sure that you have all the date range, find the min and max
maFare <- max(Fare)
breakpoints <- c(miFare-1, quFare, maFare+1) # join all the data together
Fare.cat <- cut(Fare, breakpoints) # categorical variable for all the data
table(Fare.cat) # we do a table to observe our categorical data
midpointsFare <- tapply(Fare, Fare.cat, mean) # do the function for the math grade
meandayabsFare <- tapply(X2urvived, Fare.cat, mean) # do the function for the days absent
plot(midpointsFare, meandayabsFare) # plot both variables to observe the tendency

# Poisson regression of the number of survived on sex
model1 <- glm(X2urvived~Sex,family=poisson(link="log")) # check predictions one by one
summary(model1)
eq1 <- -2.04562 + 1.35247*Sex
anova(model1)

# Poisson regression of the number of survived on class
model2 <- glm(X2urvived~Pclass,family=poisson(link="log")) # check predictions one by one
summary(model2)
eq2 <- -0.37037 - 0.45725*Pclass
anova(model2)

# Poisson regression of the number of survived on embarked
model3 <- glm(X2urvived~Embarked,family=poisson(link="log")) # check predictions one by one
summary(model3)
eq3 <- -1.09063 - 0.17893*Embarked
anova(model3)

# Poisson regression with all predictors.
detach(titanic) # we want now to put all the variable predictors in the same model
final.model <- glm(X2urvived~., family=poisson(link="log"), data=titanic) # not significant maths
summary(final.model) # X2urvived~. pic all the variables from the dot to the end
final.model <- glm(X2urvived~., family=quasipoisson(link="log"), data=titanic) 
summary(final.model) # overdispersion in model1 is tried to be solved with quasipoisson
final.model <- glm(X2urvived~.-Parch-Fare-Embarked, family=quasipoisson(link="log"), data=titanic)
summary(final.model) # we change the model lefting Parch Fare and Embarked out because are not significant
attach(titanic)
final.eq <- 0.450957-0.0016257*Passengerid-0.0155141*Age+1.2324323*Sex-0.1558248*sibsp-0.48229*Pclass
anova(final.model)

# Interpret the first model by quantifying the effect of the predictor on the average of the response 
M1 <- summary(model1)$coefficients # extract the standard errors and coefficient from table
se1 <- M1[,2] # get the standard error of the first model
b1 <- coef(model1) # get the estimate of the first model
llb1 <- b1 - qnorm(0.975)*se1 # confidence intervals of our model
ulb1 <- b1 + qnorm(0.975)*se1 # confidence intervals of our model
c(llb1[2], ulb1[2])
c(exp(llb1[2]), exp(ulb1[2])) # Value 0 inside the interval? Value 1 inside the interval?

# Interpret the second model by quantifying the effect of the predictor on the average of the response 
M2 <- summary(model2)$coefficients # extract the standard errors and coefficient from table
se2 <- M2[,2] # get the standard error of the second model
b2 <- coef(model2) # get the estimate of the second model
llb2 <- b2 - qnorm(0.975)*se2 # confidence intervals of our model
ulb2 <- b2 + qnorm(0.975)*se2 # confidence intervals of our model
c(llb2[2], ulb2[2])
c(exp(llb2[2]), exp(ulb2[2])) # Value 0 inside the interval? Value 1 inside the interval?

# Interpret the third model by quantifying the effect of the predictor on the average of the response 
M3 <- summary(model3)$coefficients # extract the standard errors and coefficient from table
se3 <- M3[,2] # get the standard error of the third model
b3 <- coef(model3) # get the estimate of the third model
llb3 <- b3 - qnorm(0.975)*se3 # confidence intervals of our model
ulb3 <- b3 + qnorm(0.975)*se3 # confidence intervals of our model
c(llb3[2], ulb3[2])
c(exp(llb3[2]), exp(ulb3[2])) # Value 0 inside the interval? Value 1 inside the interval?

# Quantify the effect of the different predictors on the response, 95% CI
Mfinal <- summary(final.model)$coefficients # extract the standard errors and coefficient from table
sefinal <- Mfinal[,2] # get the standard error of the final model
bfinal <- coef(final.model) # get the estimate of the final model
llbfinal <- bfinal - qnorm(0.975)*sefinal # confidence intervals of our model
ulbfinal <- bfinal + qnorm(0.975)*sefinal # confidence intervals of our model
c(llbfinal[2], ulbfinal[2])
c(exp(llbfinal[2]), exp(ulbfinal[2])) # Value 0 inside the interval? Value 1 inside the interval?

# Is there any indication that overdispersion is a problem for you model?
library(AER)
dispersiontest(model1)
dispersiontest(model2)
dispersiontest(model3)
