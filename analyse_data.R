# R SCRIPT 1. ANALYSE OUR DATA SET

rm(list=ls())
titanic <- read.csv("~/Downloads/stats.project/titanic.csv")
class(titanic)
dim(titanic)
head(titanic)
summary(titanic)
attach(titanic)

boxplot(Passengerid~Age)
boxplot(Passengerid~Fare)
boxplot(Passengerid~Sex)
boxplot(Passengerid~sibsp)
boxplot(Passengerid~Parch)
boxplot(Passengerid~Pclass)
boxplot(Passengerid~Embarked)
boxplot(Passengerid~X2urvived)
