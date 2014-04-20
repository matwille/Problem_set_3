
rm(list = ls())

setwd("~/R/wine_HW3")


wine = read.csv (url('http://www.nd.edu/~mclark19/learn/data/goodwine.csv'))


library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(rattle)
library(NbClust)
library(flexclust)


data(wine, package="rattle")

head(wine)


df <- scale(wine[-1])     


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}


wssplot(df)                                               



set.seed(1234)

nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

table(nc$Best.n[1,])



barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")



set.seed(1234)

fit.km <- kmeans(df, 3, nstart=25)                           

fit.km$size



fit.km$centers

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)



ct.km <- table(wine$Type, fit.km$cluster)

ct.km


randIndex(ct.km)















