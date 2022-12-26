library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
install.packages("factoextra")
#importing dataset
setwd("C:/CANADA/industrial engineering/summer term/data analytics/dataset")
wine= read.csv("WineQT.csv")
head(wine)
str(wine)
dim(wine)
view(wine)

#cleaning dataset
colSums(is.na(wine))
cleanwine <- wine[complete.cases(wine),]
dim(cleanwine)
cleanwine$quality <- as.factor(cleanwine$quality)

library(GGally)
ggpairs(cleanwine, aes(colour = quality))

#1D datsaset
attach(cleanwine)
ggplot(data= cleanwine)+
  geom_histogram(aes(x=fixed.acidity),
                 position = position_dodge2(padding = .1,
                                            preserve = "single"))+
  ylab("Wine Count")+
  xlab("Fixed acidity Level")+
  ggtitle("Histogram on Fixed Acidity ")

#creating 1 d dataset
Selectivewine = dplyr::select(cleanwine, fixed.acidity)
m1 = kmeans(Selectivewine, 4, nstart=100)
m1$size
#histogram for fixed acidity
#k value is 4
ggplot(data = Selectivewine, aes(x = fixed.acidity, fill = factor(m1$cluster)))+
  geom_histogram(stat = "count")+xlab("Fixed acidity Level")+
  ylab("Wine Count")+ labs(colour = "Cluster Number")+
  ggtitle("Histogram on Fixed Acidity")+
  scale_colour_discrete(name = "Fixed Acidity Clusters", 
                        labels = c("cluster3", "cluster 2"
                                   ,"cluster 1", "cluster 4"))
#k value is 3
m1 = kmeans(Selectivewine, 3, nstart=10)
m1$size
#histogram for fixed acidity
ggplot(data = Selectivewine, aes(x = fixed.acidity, fill = factor(m1$cluster)))+
  geom_histogram(stat = "count")+xlab("Fixed acidity Level")+
  ylab("Wine Count")+ labs(colour = "Cluster Number")+
  ggtitle("Histogram on Fixed Acidity")+scale_colour_discrete(name = "Fixed Acidity Clusters", 
                                                              labels = c("less acidic", "moderately acidic"
                                                                         ,"highly acidic"))
#k value is 6
m1 = kmeans(Selectivewine, 6, nstart=10)
m1$size
#histogram for fixed acidity
ggplot(data = Selectivewine, aes(x = fixed.acidity, fill = factor(m1$cluster)))+
  geom_histogram(stat = "count")+xlab("Fixed acidity Level")+
  ylab("Wine Count")+ labs(colour = "Cluster Number")+
  ggtitle("Histogram on Fixed Acidity")+scale_colour_discrete(name = "Fixed Acidity Clusters", 
                                                              labels = c("less acidic", "moderately acidic"
                                                                         ,"highly acidic"))
#2 dimensional 
winescale = scale(wine)
winescale = data.frame(winescale)
attach(winescale)
ggplot(mapping = aes(x = alcohol, y = pH))+
  geom_point()
#k 6 pH~alcohol
attach(winescale)
Selectivewine1 = dplyr::select(winescale, alcohol,pH)
M2 = kmeans(Selectivewine1, 6, nstart = 3)
M2$size
M2$centers
ggplot(data = Selectivewine1, aes(x=alcohol, y = pH, colour = factor(M2$cluster)))+
  geom_point()+
  xlab("Alcohol level")+
  ylab("pH Value")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between pH~ Alcohol")+
  scale_colour_discrete(name = "pH & alcohol", 
                        labels = c("cluster 1", "cluster 2"
                                   ,"cluster 3", "cluster 4", "cluster 5" , "cluster 6"))
#k3 pH~alcohol
Selectivewine1 = dplyr::select(winescale, alcohol,pH)
M2 = kmeans(Selectivewine1, 3, nstart = 10)
M2$size
M2$centers
?kmeans
ggplot(data = Selectivewine1, aes(x=alcohol, y = pH, colour = factor(M2$cluster)))+
  geom_point()+
  xlab("Alcohol level")+
  ylab("pH Value")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between pH~ Alcohol")+
  scale_colour_discrete(name = "pH & alcohol", 
                        labels = c("cluster 1", "cluster 2"
                                   ,"cluster 3"))
#k4 pH~alcohol
Selectivewine1 = dplyr::select(winescale, alcohol,pH)
M2 = kmeans(Selectivewine1, 4, nstart = 100)
M2$centers
ggplot(data = Selectivewine1, aes(x=alcohol, y = pH, colour = factor(M2$cluster)))+
  geom_point()+
  xlab("Alcohol level")+
  ylab("pH Value")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between pH~ Alcohol")+
  scale_colour_discrete(name = "pH & alcohol", 
                        labels = c("cluster 1", "cluster2"
                                   ,"cluster 3","cluster 4"))

#clustering using 2 dimensional
#density~residual sugar
attach(winescale)
ggplot(mapping = aes(x = residual.sugar, y = density))+
  geom_point()
#k4
attach(winescale)
Selectivewine2 = dplyr::select(winescale, density, residual.sugar)
M3 = kmeans(Selectivewine2, 4, nstart = 100)
M3$centers
ggplot(data = Selectivewine2, aes(x = residual.sugar, y = density, 
                                  colour = factor(M3$cluster)))+
  geom_point()+
  xlab("Residual Sugar Level")+
  ylab("Density of the wine")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between Residual Sugar~ Density")+
  scale_colour_discrete(name = "Residual Sugar & Density ", 
                        labels = c("cluster 1","cluster 2","cluster 3"
                                   ,"cluster 4"))
#k3
attach(winescale)
Selectivewine2 = dplyr::select(winescale, density, residual.sugar)
M3 = kmeans(Selectivewine2, 3, nstart = 10)
M3$centers
ggplot(data = Selectivewine2, aes(x = residual.sugar, y = density, 
                                  colour = factor(M3$cluster)))+
  geom_point()+
  xlab("Residual Sugar Level")+
  ylab("Density of the wine")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between Residual Sugar~ Density")+
  scale_colour_discrete(name = "Residual Sugar & Density ", 
                        labels = c("cluster 1","cluster 2","cluster 3"
                                   ,"cluster 4"))
#k6 does not make sense
attach(winescale)
Selectivewine2 = dplyr::select(winescale, density, residual.sugar)
M3 = kmeans(Selectivewine2, 6, nstart = 10)
M3$size
M3$centers
ggplot(data = Selectivewine2, aes(x = residual.sugar, y = density, 
                                  colour = factor(M3$cluster)))+
  geom_point()+
  xlab("Residual Sugar Level")+
  ylab("Density of the wine")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between Residual Sugar~ Density")+
  scale_colour_discrete(name = "Residual Sugar & Density ", 
                        labels = c("cluster 1","cluster 2","cluster 3",
                                   "cluster 4","cluster 5","cluster 6"))


#clustering using 2 dimensional
#ph~ chlorides

winescale = scale(wine)
winescale = data.frame(winescale)
attach(winescale)
ggplot(mapping = aes(y = chlorides, x = pH))+
  geom_point()

#k4
attach(winescale)
Selectivewine3 = dplyr::select(winescale, pH,chlorides)
M3 = kmeans(Selectivewine3, 4, nstart = 100)
M3$centers
ggplot(data = Selectivewine3, aes(x = pH, y = chlorides, 
                                  colour = factor(M3$cluster)))+
  geom_point()+
  xlab("pH Value")+
  ylab("CHLORIDE LEVELS")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between pH~ Chlorides")+
  scale_colour_discrete(name = "pH~ Chlorides Clusters ", 
                        labels = c("cluster 1","cluster 2","cluster 3"
                                   ,"cluster 4"))
#k3
attach(winescale)
Selectivewine3 = dplyr::select(winescale, pH,chlorides)
M3 = kmeans(Selectivewine3, 3, nstart = 10)
M3$centers
ggplot(data = Selectivewine3, aes(x = pH, y = chlorides, 
                                  colour = factor(M3$cluster)))+
  geom_point()+
  xlab("pH Value")+
  ylab("CHLORIDE LEVELS")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between pH~ Chlorides")+
  scale_colour_discrete(name = "pH~ Chlorides Clusters ", 
                        labels = c("cluster 1","cluster 2","cluster 3"
                                   ,"cluster 4"))


#hierarchical clustering
library(factoextra)
#scale data set
scaledWine<- scale(wine)
attach(wine)
selectiveWINE = dplyr::select(winescale, pH, alcohol, residual.sugar, quality)
selectiveWINE= data.frame(selectiveWINE)
view(scaledWine)
m2h = hclust(dist(selectiveWINE), method="complete")
plot(m2h)
attach(selectiveWINE)
par(mar=c(1, 1, 1, 1))
plot(pH~density, col=(cutree(m2h, k=2)))
plot(pH~density, col=(cutree(m2h, h=5)))
hm1 = hclust(dist(selectiveWINE), method="average")
plot(hm1)

attach(selectiveWINE)
SF = scale(selectiveWINE)
SF = data.frame(SF)
m1 = kmeans(SF, 3, nstart = 10)
plot(chlorides~pH, col=m1$cluster, pch=20)
m1 = kmeans(SF, 3, nstart = 10)
plot(density~residual.sugar, col=m1$cluster, pch=20)
m1$tot.withinss
m1 = kmeans(SF, 3, nstart=100)
plot(pH~alcohol, col=m1$cluster, pch=20)
