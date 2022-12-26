library(tidyverse)
library(dplyr)
library(MASS)
library(ggplot2)
library(readr)
library(corrplot)

#importing dataset
setwd("C:/CANADA/industrial engineering/summer term/data analytics/dataset")
heartdata= read.csv("heart.csv")
head(heartdata)
str(heartdata)
dim(heartdata)

#cleaning dataset
colSums(is.na(heartdata))
cleandata <- heartdata[complete.cases(heartdata),]
dim(cleandata)
view(cleandata)
str(cleandata)
cleandata$HeartDisease <- as.factor(cleandata$HeartDisease)

view(cleandata)
heartMix = slice(cleandata, sample(1:n()))
View(heartMix)
summary(cleandata)
summary(heartMix)
id = seq(1, 918, by=1)
heartRando = mutate(heartMix, id)
view(heartRando)

k = 5
numRows = nrow(heartRando)
errors = rep(0, k)
totalError = 0
id = seq(1, 918, by=1)
heartMix = slice(cleandata, sample(1:n()))
heartRando = mutate(heartMix, id)
#number of models I want to try and their definitions
numModels = 6
modelnames = rep(0, numModels)
modelnames[1] = "HeartDisease~Cholesterol+RestingBP+FastingBS"
modelnames[2] = "HeartDisease~ Age+MaxHR"
modelnames[3] = "HeartDisease~ Age+MaxHR+FastingBS"
modelnames[4] = "HeartDisease~ Age+MaxHR+FastingBS+ChestPainType"
modelnames[5] = "HeartDisease~ Age+MaxHR+FastingBS+ChestPainType+RestingECG"
modelnames[6] = "HeartDisease~Age+Sex+Oldpeak+ExerciseAngina"
model = lda(eval(parse(text=paste(modelnames[j])), train))
model
heartMix = slice(cleandata, sample(1:n()))
heartRando = mutate(heartMix, id)
errors = matrix(1:numModels*k,
                dimnames= list(seq(1, numModels, by=1), seq(1, k, by=1)),
                nrow=numModels, ncol=k)
print(errors)
class(errors)
view(numModels)
k
avgError = rep(0,numModels)
for(j in 1: numModels){
  for(i in 1:k){
    
  }
}

for(j in 1:numModels){
  k = 5
  numRows = 918
  totalErrors = rep(0, k)
  for(i in 1:k){
    test = filter(heartRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
    train = anti_join(heartRando, test, by="id")
    model = lda(eval(parse(text=paste(modelnames[j])), train))
    modelGuesses = predict(model, test)
    errors[j,i] = 1-mean(modelGuesses$class == test$HeartDisease)
    totalErrors[i] = errors[j,i]+totalErrors[i]
  }
  avgError[j] = totalErrors[j]/k
}
avgError
view(totalErrors)
mean(errors[1,])
mean(errors[2,])
mean(errors[3,])
mean(errors[4,])
mean(errors[5,])
mean(errors[6,])
plot(c(mean(errors[1,]), mean(errors[2,]), mean(errors[3,]), mean(errors[4,]), mean(errors[5,]), mean(errors[6,])))
sqrt(var(errors[1,])/k)
x = seq(1, numModels, by=1) 
y = rep(0, numModels) 
bars = rep(0, numModels)
for(i in 1:numModels){
  y[i] = mean(errors[i, ])
  bars[i] = sqrt(var(errors[i,])/k)
}
allData = data.frame(x, y, bars)
View(allData)
ggplot(allData, aes(x=x, y=y)) +
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=y-bars, ymax=y+bars), width=.2,
                position=position_dodge(0.05))+
  xlab("model complexity")+
  ylab("average square error")
Regression model:
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

attach(New_wine)
#model3
wine= dplyr::select(New_wine, year,rating, num_reviews, body, acidity, price)
for (i in 8) {
  best3 = regsubsets(rating~year+num_reviews+body+acidity+poly(price,i, raw = TRUE),data= wine,nvmax = 12)
  summary(best3)
  summary(best3)$rsq
  plot(summary(best3)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
}

library(boot)
cross_validation_error = rep(0,8)
view(cross_validation_error)
for(i in 1:8){
  model=glm(rating~year+num_reviews+body+acidity+poly(price,i, raw = TRUE))
  cross_validation_error[i] = cv.glm(wine, model, K=5)$delta[1]
}
cross_validation_error
view(cross_validation_error)

x=seq(1,8, by=1)
cv = data.frame(x, y = cv.error)
cv
ggplot(data=cv)+geom_point(aes(x=x, y=y))+
  xlab("Model Complexity")+
  ylab("Model Error")


attach(wine)
k=5
adjustedrsqds = rep(0,8) 
adjustedrsqds
rsqVals =rep(0,8)
rsqVals
for(k in 1:8){
  model = lm(rating~year+num_reviews+body+acidity+poly(price,k, raw = TRUE))
  adjustedrsqds[k] = summary(model)$adj.r.squared
  rsqVals[k] = summary(model)$r.squared
}
adjustedrsqds
rsqVals
plot(adjustedrsqds)
plot(rsqVals)

#K-FOLD validation

k.fold.errors.10 = rep (0 ,8) 
for(k in 1:10){
  model = glm(rating~year+num_reviews+body+acidity+poly(price,i, raw = TRUE))
  k.fold.errors.10[k] = cv.glm(wine, model, K=5)$delta[1]
}
k.fold.errors.10 


ModelComplexity = seq(1, 8, by=1)
various_Errors = data.frame(ModelComplexity, k.fold.errors.10, cross_validation_error,rsqVals)
View(various_Errors)

ggplot(data = various_Errors)+
  geom_point(aes(x=ModelComplexity, y=k.fold.errors.10), col="red")+
  geom_point(aes(x=ModelComplexity, y=cross_validation_error), col="blue")

ggplot(data = various_Errors)+
  geom_point(aes(x=ModelComplexity, y=adjustedrsqds), col="green")+
  geom_point(aes(x=ModelComplexity, y=rsqVals), col="yellow")+
  geom_path(aes(x=ModelComplexity, y=adjustedrsqds), col="green")+
  geom_path(aes(x=ModelComplexity, y=rsqVals), col="yellow")

attach(wine)
numRows = nrow(wine)
id = seq(1, numRows, by =1)
wineShuffle = slice(wine, sample(1:n()))
wineShuffle = mutate(wineShuffle, id)
View(wineShuffle)
k = 5 

errors = matrix( nrow = 8, ncol = 5)
View(errors)
errors[1,2] = 0
View(errors)
for(j in 1:8){
  for(i in 1:5){
    errors[j,i] = 0
  }
}
View(errors)

library(dplyr)
library(MASS)
library(ggplot2)
totalError = 0
for(j in 1:8){ 
  for(i in 1:k){ 
    test= filter(wineShuffle, id >= (i-1)*numRows/k+1 & id <=i*numRows/k)
    train = anti_join(wineShuffle, test, by="id")
    model = lm(rating~year+num_reviews+body+acidity+poly(price,j, raw = TRUE), data = train)
    errors[j,i] = mean((test$rating - predict.lm(model, test))^2)
  }}
View(errors)

avgRegEr = rep(0,8)
avgRegEr
for(j in 1:8){
  for(i in 1:5){
    avgRegEr[j] = avgRegEr[j]+errors[j, i]
  }
}
avgRegEr
avgRegEr/k
cross_validation_error

se = rep(0, 8)
se
for (i in 1:8){
  se[i] = sqrt(var(errors[i,])/k)
}
se

x = seq(1,8, by = 1)
wineBest = data.frame(x,avgRegEr/k , se)
wineBest

library(ggplot2)
ggplot(data = wineBest, aes(x = x, y=avgRegEr.k))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = avgRegEr.k-se, ymax = avgRegEr.k +se))+
  xlab("model complexity")+
  ylab("average square error")+
  ggtitle("Error Bars attempt 5")
