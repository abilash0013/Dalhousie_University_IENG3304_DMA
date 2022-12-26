library(tidyverse)
library(dplyr)
library(scales)
library(ggplot2)
library(leaps)
#setting up working directory
setwd("C:/CANADA/industrial engineering/summer term/data analytics/dataset")
#importing the dataset
dataset= read.csv("spanish_wine.csv")
view(dataset)
dim(dataset)
str(dataset)
summary(dataset)
colSums(is.na(dataset))
cleandata <- dataset[complete.cases(dataset),]
dim(cleandata)
cleandata$year <- as.integer(cleandata$year)
view(cleandata)
#cleaning dataset
New_wine = drop_na(cleandata)
view(New_wine)
str(New_wine)
dim(New_wine)
#models to increase the r2value
wine= dplyr::select(New_wine, year,rating, num_reviews, body, acidity, price)
best = regsubsets(rating~., data=wine)
summary(best)
summary(best)$rsq
plot(summary(best)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model2
best2 = regsubsets(rating~poly(year,3)+poly(num_reviews,3)+poly(body,3)+
                     poly(acidity,2)+poly(price,3),
                   data= wine,nvmax = 12)
summary(best2)
summary(best2)$rsq
plot(summary(best2)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model3
best3 = regsubsets(rating~year+poly(num_reviews,4)+poly(body,3) 
                   +acidity+poly(price,3),data= wine,nvmax = 12)
summary(best3)
summary(best3)$rsq
plot(summary(best3)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model4
best4 = regsubsets(rating~year+num_reviews+body+acidity+log(price),data= wine,nvmax 
                   = 12)
summary(best4)
summary(best4)$rsq
plot(summary(best4)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model5
best5 = regsubsets(rating~year+log(num_reviews)+body+acidity+log(price),data= 
                     wine,nvmax = 12)
summary(best5)
summary(best5)$rsq
plot(summary(best5)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
ggplot(data=New_wine, aes(y=rating,x=price))+
  geom_point(position = position_jitter(width = 1, height = .5))+
  geom_smooth(method="lm")+
  geom_smooth(method="lm",formula= y~poly(x,2),colour= "red")+
  geom_smooth(method="lm",formula= y~poly(x,3),colour= "green")+
  geom_smooth(method="lm",formula= y~poly(x,4),colour= "yellow")+
  xlab("Price of Wine")+
  ylab("Wine Rating")+
  ggtitle("regression model")
rval= seq(1,10)
rvalx= seq(1,10)
for (i in 1:10) {
  model= lm(data=New_wine,rating~poly(price,i))
  rval[i]= summary(model)$r.squared
}
view(rval)
modelR2val= data.frame(rvalx,rval)
ggplot(data=modelR2val)+
  geom_point(aes(x=rvalx,y=rval))+
  geom_path(x=rvalx,y=rval)+
  xlab("model of complexity")+
  ylab("R Square Value")+
  ggtitle("R Square value vs model of complexity")
#regression model with dependent variable to be price
wines= dplyr::select(New_wine, year,rating, num_reviews, body, acidity, price)
wbest = regsubsets(price~., data=wines)
summary(wbest)
summary(wbest)$rsq
plot(summary(wbest)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model2
wbest2 = regsubsets(price~poly(year,3)+poly(num_reviews,3)+poly(body,3)+
                      poly(acidity,2)+poly(rating,3),
                    data= wines,nvmax = 12)
summary(wbest2)
summary(wbest2)$rsq
plot(summary(wbest2)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model3
wbest3 = regsubsets(price~year+poly(num_reviews,4)+poly(body,3) 
                    +acidity+poly(rating,3),data= wines,nvmax = 12)
summary(wbest3)
summary(wbest3)$rsq
plot(summary(wbest3)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model4
wbest4 = regsubsets(price~year+num_reviews+body+acidity+log(rating),data= 
                      wines,nvmax = 12)
summary(wbest4)
summary(wbest4)$rsq
plot(summary(wbest4)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#model5
wbest5 = regsubsets(price~year+log(num_reviews)+body+acidity+log(rating),data= 
                      wines,nvmax = 12)
summary(wbest5)
summary(wbest5)$rsq
plot(summary(best5)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#plotting the graph for various variables
#reviews vs ratings
ggplot(data=New_wine)+
  geom_bar(mapping = aes(y=body))+
  ylab("richness of the wine")+
  xlab("NUMBER OF REVIEWS")+
  ggtitle("quality of wine")
ggplot(data = New_wine)+
  geom_boxplot(mapping = aes(y = factor(rating), x = price, colour = rating))+
  ylab("Overall rating of the wine")+
  xlab("Price of Wine")+
  labs(color = "Rating Meter")+
  ggtitle("Impact of rating on the price of the wine")
ggplot(data = New_wine)+
  geom_boxplot(mapping = aes (y = factor(type), x = price, colour = type))+
  ylab("Type of Wine")+
  xlab("Price of Wine")+
  labs(color = "Type of Wine")+
  ggtitle("Relationship between Price of wine and Type of wine")
ggplot(data = New_wine)+
  geom_point(mapping = aes (x = rating, y = type, color = type))+
  ylab("Type of Wine")+
  xlab("Rating Range")+
  ggtitle("Relationshhip between Rating and Type of Wine")
ggplot(data = New_wine)+
  geom_point(mapping=aes(x=rating, y=num_reviews, color = rating), position= "jitter")+
  ylab("Number of reviews")+
  xlab("Wine Rating")+
  labs(color = "Wine Rating")+
  ggtitle("wine rating with respect to number of reviews"