
rm(list = ls())

setwd("~/R/titanic")

library(randomForest)

library(party)
library(sandwich)
library(strucchange)
library(modeltools)
library(stats4)



load(url('http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.sav'))
dim(titanic3)
attributes(titanic3)
titanic3[1:5,]
set.seed(1234)
ind <- sample(2, nrow(titanic3), replace=TRUE, prob=c(0.50, 0.50))

#Break out the test and the trainign portions of the data
titanic.train <- titanic3[ind==1,]
titanic.test <- titanic3[ind==2,]



#GLM Model
titanic.train <- read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.csv")
titanic.survival.train = glm(survived ~ pclass + sex + pclass:sex + age + sibsp, family = binomial(logit), data = titanic.train, na.action = na.omit)

summary(titanic.survival.train)


#Random Forest Model

titanic.survival.train.rf = randomForest(as.factor(survived) ~ pclass + sex + age + sibsp, data=titanic.train,ntree=5000, importance=TRUE, na.action = na.omit)

titanic.survival.train.rf

importance(titanic.survival.train.rf)



#Conditional tree model

titanic.survival.train.ctree = ctree(as.factor(survived) ~ pclass + sex + age + sibsp, titanic.train)

titanic.survival.train.ctree

plot(titanic.survival.train.ctree)
