library(tidyverse)
library(dplyr)
library(ggplot2)
library(stats)
library(scales)
library(leaps)
library(MASS)
#importing dataset
setwd("C:/CANADA/industrial engineering/summer term/data analytics/dataset")
rawwinedata= read.csv("wineQT.csv")
head(rawwinedata)
view(rawwinedata)
str(rawwinedata)
dim(rawwinedata)

#cleaning dataset
colSums(is.na(rawwinedata))
cleanwinedata <- rawwinedata[complete.cases(rawwinedata),]
dim(cleanwinedata)
view(cleanwinedata)

#regression model:

attach(cleanwinedata)
wine= dplyr::select(cleanwinedata, alcohol,quality, residual.sugar,density, volatile.acidity, pH, chlorides)
#model 1
best = regsubsets(alcohol~., data=wine)
summary(best)
summary(best)$rsq
plot(summary(best)$rsq, type = "o",xlab = "MODEL complexity",ylab = "R squared value")
#0.4604

#model2
best2 = regsubsets(alcohol~poly(density,3)+poly(residual.sugar,3),data= wine,nvmax = 12)
summary(best2)
summary(best2)$rsq
plot(summary(best2)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#0.45


#model3
best3 = regsubsets(alcohol~quality+poly(pH,4)+poly(volatile.acidity,3) +chlorides+poly(density,3),data= wine,nvmax = 12)
summary(best3)
summary(best3)$rsq
plot(summary(best3)$rsq, type = "o",xlab = "MODEL",ylab = "R squared value")
#0.51

#model4
best4 = regsubsets(alcohol~pH+poly(quality,4)+poly(volatile.acidity,3) 
                   +density+poly(chlorides,3)+poly(residual.sugar,3),data= wine,nvmax = 12)
summary(best4)
summary(best4)$rsq
plot(summary(best4)$rsq, type = "o",xlab = "MODEL compllexity",ylab = "R squared value")
#0.5348

#model5
wbest5 = regsubsets(alcohol~poly(residual.sugar,3)+poly(quality,3)+poly(density,3)+
                      poly(volatile.acidity,2)+poly(pH,3),
                    data= wine,nvmax = 12)
summary(wbest5)
summary(wbest5)$rsq
plot(summary(wbest5)$rsq, type = "o",xlab = "MODEL complexity",ylab = "R squared value")
#0.59

ggplot(data=wine, aes(y=alcohol,x=pH))+
  geom_point(position = position_jitter(width = 1, height = .5))+
  geom_smooth(method="lm", se = FALSE)+
  geom_smooth(method="lm",formula= y~poly(x,2),colour= "red", se = FALSE)+
  geom_smooth(method="lm",formula= y~poly(x,3),colour= "green", se = FALSE)+
  geom_smooth(method="lm",formula= y~poly(x,4),colour= "yellow", se = FALSE)+
  xlab("pH of wine")+
  ylab("alcohol level in wine")+
  ggtitle("regression model")+
  xlim(3, 5)


rval= seq(1,10)
rvalx= seq(1,10)
for (i in 1:10) {
  model= lm(data=wine,alcohol~poly(residual.sugar,3)+poly(quality,3)+poly(density,i)+
              poly(volatile.acidity,2)+poly(pH,i))
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


#group plot
library(GGally)
attach(cleanwinedata)
cleanwinedata$quality <- as.factor(cleanwinedata$quality)
ggpairs(cleanwinedata, aes(colour = quality))

#linear discriminant analysis
attach(cleanwinedata)
library(MASS)
#spliting training and test data

view(cleanwinedata)
IDwine = mutate(cleanwinedata, id=row_number())
View(IDwine)
winetrainDataSet = sample_frac(IDwine, .5)
View(winetrainDataSet)
winetestDataSet = anti_join(IDwine, winetrainDataSet, by = "id")
View(winetestDataSet)

#creating first model
view(winetestDataSet)
winemodel1 = lda(quality~alcohol+pH+residual.sugar+density, data = winetrainDataSet)
winemodel1
winetrainSetPrediction1 = predict(winemodel1, winetrainDataSet)
table(winetrainSetPrediction1$class, winetrainDataSet$quality)
mean(winetrainSetPrediction1$class== winetrainDataSet$quality)

#running model for test data
wineTestSetprediction1 = predict(winemodel1,winetestDataSet )
table(wineTestSetprediction1$class , winetestDataSet$quality)
mean(wineTestSetprediction1$class == winetestDataSet$quality)

#creating second model
winemodel2 = lda(quality~alcohol+pH+residual.sugar+density+chlorides, data = winetrainDataSet)
winemodel2
winetrainSetPrediction2 = predict(winemodel2, winetrainDataSet)
table(winetrainSetPrediction2$class, winetrainDataSet$quality)
mean(winetrainSetPrediction2$class== winetrainDataSet$quality)

#running model for test data
wineTestSetprediction2 = predict(winemodel2,winetestDataSet )
table(wineTestSetprediction2$class , winetestDataSet$quality)
mean(wineTestSetprediction2$class == winetestDataSet$quality)

#creating third model
winemodel3 = lda(quality~alcohol+pH+residual.sugar+density+chlorides+
                   volatile.acidity, data = winetrainDataSet)
winemodel3
winetrainSetPrediction3 = predict(winemodel3, winetrainDataSet)
table(winetrainSetPrediction3$class, winetrainDataSet$quality)
mean(winetrainSetPrediction3$class== winetrainDataSet$quality)

#running model for test data
wineTestSetprediction3 = predict(winemodel3,winetestDataSet )
table(wineTestSetprediction3$class , winetestDataSet$quality)
mean(wineTestSetprediction3$class == winetestDataSet$quality)
#0.57

#creating fourth model
winemodel4 = lda(quality~alcohol+pH+residual.sugar+density+chlorides+
                   volatile.acidity+fixed.acidity+total.sulfur.dioxide
                 , data = winetrainDataSet)
winemodel4
winetrainSetPrediction4 = predict(winemodel4, winetrainDataSet)
table(winetrainSetPrediction4$class, winetrainDataSet$quality)
mean(winetrainSetPrediction4$class== winetrainDataSet$quality)

#running model for test data
wineTestSetprediction4 = predict(winemodel4,winetestDataSet )
table(wineTestSetprediction4$class , winetestDataSet$quality)
mean(wineTestSetprediction4class == winetestDataSet$quality)
#0.60

#creating fifth model
winemodel5 = lda(quality~alcohol+pH+residual.sugar+density+chlorides+
                   volatile.acidity+fixed.acidity+total.sulfur.dioxide
                 +sulphates, data = winetrainDataSet)
winemodel5
winetrainSetPrediction5 = predict(winemodel5, winetrainDataSet)
table(winetrainSetPrediction5$class, winetrainDataSet$quality)
mean(winetrainSetPrediction5$class== winetrainDataSet$quality)

#running model for test data
wineTestSetprediction5 = predict(winemodel5,winetestDataSet )
table(wineTestSetprediction5$class , winetestDataSet$quality)
mean(wineTestSetprediction5$class == winetestDataSet$quality)
#0.58

#creating sixth model
winemodel6 = lda(quality~poly(alcohol,10)+poly(pH,10)+poly(residual.sugar,10)
                 +poly(density,10)+poly(chlorides,10)+poly(volatile.acidity,10)
                 +poly(fixed.acidity,10)+poly(total.sulfur.dioxide,10)
                 +poly(sulphates,10)+ poly(free.sulfur.dioxide,10)+poly(citric.acid,10), data = winetrainDataSet)
winemodel6
winetrainSetPrediction6 = predict(winemodel6, winetrainDataSet)
table(winetrainSetPrediction6$class, winetrainDataSet$quality)
mean(winetrainSetPrediction6$class== winetrainDataSet$quality)
#running model for test data
wineTestSetprediction6 = predict(winemodel6,winetestDataSet )
table(wineTestSetprediction6$class , winetestDataSet$quality)
mean(wineTestSetprediction6$class == winetestDataSet$quality)
#0.56

#creating seventh model
winemodel7 = lda(quality~poly(alcohol,8)+pH+poly(residual.sugar,4)
                 +density+poly(chlorides,6)
                 +poly(volatile.acidity,8)+fixed.acidity+total.sulfur.dioxide
                 +sulphates, data = winetrainDataSet)
winemodel7
winetrainSetPrediction7 = predict(winemodel7, winetrainDataSet)
table(winetrainSetPrediction7$class, winetrainDataSet$quality)
mean(winetrainSetPrediction7$class== winetrainDataSet$quality)
#running model for test data
wineTestSetprediction7 = predict(winemodel7,winetestDataSet )
table(wineTestSetprediction7$class , winetestDataSet$quality)
mean(wineTestSetprediction7$class == winetestDataSet$quality)

#creating conclusion graph to visualize the error.
attach(cleanwinedata)
model1D = lda(quality~poly(alcohol)+pH+residual.sugar+density+chlorides+
                volatile.acidity+fixed.acidity+total.sulfur.dioxide
              +sulphates+ free.sulfur.dioxide+citric.acid)
guess1D = predict(model1D, cleanwinedata)
winewith1Dguesses = mutate(cleanwinedata, guess=guess1D$class,
                           isError = ifelse(guess==quality, as.character(guess),
                                            "error"))
View(winewith1Dguesses)

ggplot(data= winewith1Dguesses )+geom_bar( aes(x= quality, fill=isError))+scale_fill_manual(values = c("yellow", "red", "blue","green","violet","black"))+
  ylab("number of wines")+
  xlab("quality of wine")+
  labs(color = "outcomes")+
  ggtitle("visualising error rate in classifying wines")

ggplot(data=winewith1Dguesses)+geom_bar(aes(x= quality,
                                            fill=as.factor(quality)))+scale_fill_manual(values = c( "orange", "red", "blue","green","yellow","violet"))+
  ylab("number of wines")+
  xlab("quality of wine")+
  labs(color = "outcomes")+
  ggtitle("categorising wines based on different quality")


#classification cross validation

wineMix = slice(cleanwinedata, sample(1:n()))

summary(cleanwinedata)

summary(wineMix)

id = seq(1, 1143, by=1)

wineRando = mutate(wineMix, id)


k = 5

numRows = nrow(cleanwinedata)


train = filter(wineRando, id <= 4*numRows/k)

test = anti_join(wineRando, train, by="id")

View(train)

View(test)


library(MASS)


attach(cleanwinedata)

k = 5

numRows = nrow(cleanwinedata)

errors_1 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~alcohol+pH+residual.sugar+density, train)
  
  modelGuesses = predict(model, test)
  
  errors_1[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_1[i]+totalError
  
}

errors_1

totalError


avgerror=totalError/k

avgerror

#second model
errors_2 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~alcohol+pH+residual.sugar+density+chlorides, train)
  
  modelGuesses = predict(model, test)
  
  errors_2[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_2[i]+totalError
  
}

errors_2

totalError


avgerror=totalError/k

avgerror

#third model

errors_3 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~alcohol+pH+residual.sugar+density+chlorides+
                volatile.acidity, train)
  
  modelGuesses = predict(model, test)
  
  errors_3[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_3[i]+totalError
  
}

errors_3

totalError

avgerror=totalError/k

avgerror

#fourth model

errors_4 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~alcohol+pH+residual.sugar+density+chlorides+
                volatile.acidity+fixed.acidity+total.sulfur.dioxide, train)
  
  modelGuesses = predict(model, test)
  
  errors_4[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_4[i]+totalError
  
}

errors_4

totalError

avgerror=totalError/k

avgerror

#fifth model

errors_5 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~alcohol+pH+residual.sugar+density+chlorides+
                volatile.acidity+fixed.acidity+total.sulfur.dioxide
              +sulphates, train)
  
  modelGuesses = predict(model, test)
  
  errors_5[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_5[i]+totalError
  
}

errors_5

totalError


avgerror=totalError/k

avgerror

#sixth model

errors_6 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~poly(alcohol,10)+poly(pH,10)+poly(residual.sugar,10)+poly(density,10)+poly(chlorides,10)+poly(volatile.acidity,10)
              +poly(fixed.acidity,10)+poly(total.sulfur.dioxide,10)+poly(sulphates,10)+ poly(free.sulfur.dioxide,10)+poly(citric.acid,10)
              , train)
  
  modelGuesses = predict(model, test)
  
  errors_6[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_6[i]+totalError
  
}

errors_6

totalError

avgerror=totalError/k

avgerror

#seventh model

errors_7 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality~poly(alcohol,8)+pH+poly(residual.sugar,4)+density+poly(chlorides,6)+poly(volatile.acidity,8)+fixed.acidity+total.sulfur.dioxide
              +sulphates
              , train)
  
  modelGuesses = predict(model, test)
  
  errors_7[i] = 1-mean(modelGuesses$class == test$quality)
  
  totalError = errors_7[i]+totalError
  
}

errors_7

totalError

avgerror=totalError/k

avgerror

errors_1

errors_2

errors_3

errors_4

errors_5

errors_6

errors_7


avgE=rep(0,7)

avgE

for(i in i:k){
  
  avgE[1]=errors1[i]+avgE[1]
  
  avgE[2]=errors2[i]+avgE[2]
  
  avgE[3]=errors3[i]+avgE[3]
  
  avgE[4]=errors4[i]+avgE[4]
  
  avgE[5]=errors5[i]+avgE[5]
  
  avgE[6]=errors6[i]+avgE[6]
  
  avgE[7]=errors7[i]+avgE[7]
  
}

avgE[1]

avgE[2]

avgE[3]

avgE[4]

avgE[5]

avgE[6]

avgE[7]


se=rep(0,k)

se[1]=sqrt(var(errors1)/k)

se[2]=sqrt(var(errors2)/k)

se[3]=sqrt(var(errors3)/k)

se[4]=sqrt(var(errors4)/k)

se[5]=sqrt(var(errors5)/k)

se[6]=sqrt(var(errors6)/k)

se[7]=sqrt(var(errors7)/k)


mn=seq(1,7, by=1)
length(avgE)
length(se)

cross_validation = data.frame(avgE, se)

View(cross_validation)



#plotting data

library(tidyverse)

ggplot(cross_validation, aes(x=mn,y=avgE))+
  
  geom_line()+
  
  geom_point()+
  
  geom_errorbar(aes(ymin=avgE-se, ymax=avgE+se))+
  
  xlab("Model Complexity")+
  
  ylab("Classification Error")+
  
  ggtitle("Classification Error vs Model Complexity ")

#Classification II  

cleanwinedata$quality_dummy <- ifelse(wine$quality > 6, 1, 0)

view(cleanwinedata)
idwine = mutate(cleanwinedata, id=row_number())
View(idwine)
winetrainDataSet = sample_frac(idwine, .5)
View(winetrainDataSet)
winetestDataSet = anti_join(idwine, winetrainDataSet, by = "id")
View(winetestDataSet)

#creating first model
view(winetestDataSet)
winemodel1 = lda(quality_dummy~alcohol+pH+residual.sugar+density, data = winetrainDataSet)
winemodel1
winetrainSetPrediction1 = predict(winemodel1, winetrainDataSet)
table(winetrainSetPrediction1$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction1$class== winetrainDataSet$quality_dummy)

#running model for test data
wineTestSetprediction1 = predict(winemodel1,winetestDataSet )
table(wineTestSetprediction1$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction1$class == winetestDataSet$quality_dummy)
#0.86

#creating second model
winemodel2 = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides, data = winetrainDataSet)
winemodel2
winetrainSetPrediction2 = predict(winemodel2, winetrainDataSet)
table(winetrainSetPrediction2$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction2$class== winetrainDataSet$quality_dummy)

#running model for test data
wineTestSetprediction2 = predict(winemodel2,winetestDataSet )
table(wineTestSetprediction2$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction2$class == winetestDataSet$quality_dummy)
#0.86

#creating third model
winemodel3 = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides+
                   volatile.acidity, data = winetrainDataSet)
winemodel3
winetrainSetPrediction3 = predict(winemodel3, winetrainDataSet)
table(winetrainSetPrediction3$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction3$class== winetrainDataSet$quality_dummy)

#running model for test data
wineTestSetprediction3 = predict(winemodel3,winetestDataSet )
table(wineTestSetprediction3$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction3$class == winetestDataSet$quality_dummy)
#0.88

#creating fourth model
winemodel4 = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides+
                   volatile.acidity+fixed.acidity+total.sulfur.dioxide
                 , data = winetrainDataSet)
winemodel4
winetrainSetPrediction4 = predict(winemodel4, winetrainDataSet)
table(winetrainSetPrediction4$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction4$class== winetrainDataSet$quality_dummy)

#running model for test data
wineTestSetprediction4 = predict(winemodel4,winetestDataSet )
table(wineTestSetprediction4$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction4$class == winetestDataSet$quality_dummy)
#0.86

#creating fifth model
winemodel5 = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides+
                   volatile.acidity+fixed.acidity+total.sulfur.dioxide
                 +sulphates, data = winetrainDataSet)
winemodel5
winetrainSetPrediction5 = predict(winemodel5, winetrainDataSet)
table(winetrainSetPrediction5$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction5$class== winetrainDataSet$quality_dummy)

#running model for test data
wineTestSetprediction5 = predict(winemodel5,winetestDataSet )
table(wineTestSetprediction5$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction5$class == winetestDataSet$quality_dummy)
#0.87

#creating sixth model
winemodel6 = lda(quality_dummy~poly(alcohol,10)+poly(pH,10)+poly(residual.sugar,10)
                 +poly(density,10)+poly(chlorides,10)+poly(volatile.acidity,10)
                 +poly(fixed.acidity,10)+poly(total.sulfur.dioxide,10)
                 +poly(sulphates,10)+ poly(free.sulfur.dioxide,10)+poly(citric.acid,10), data = winetrainDataSet)
winemodel6
winetrainSetPrediction6 = predict(winemodel6, winetrainDataSet)
table(winetrainSetPrediction6$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction6$class== winetrainDataSet$quality_dummy)
#running model for test data
wineTestSetprediction6 = predict(winemodel6,winetestDataSet )
table(wineTestSetprediction6$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction6$class == winetestDataSet$quality_dummy)
#0.84

#creating seventh model
winemodel7 = lda(quality_dummy~poly(alcohol,8)+pH+poly(residual.sugar,4)
                 +density+poly(chlorides,6)
                 +poly(volatile.acidity,8)+fixed.acidity+total.sulfur.dioxide
                 +sulphates, data = winetrainDataSet)
winemodel7
winetrainSetPrediction7 = predict(winemodel7, winetrainDataSet)
table(winetrainSetPrediction7$class, winetrainDataSet$quality_dummy)
mean(winetrainSetPrediction7$class== winetrainDataSet$quality_dummy)
#running model for test data
wineTestSetprediction7 = predict(winemodel7,winetestDataSet )
table(wineTestSetprediction7$class , winetestDataSet$quality_dummy)
mean(wineTestSetprediction7$class == winetestDataSet$quality_dummy)
#0.84

#creating conclusion graph to visualize the error.
attach(cleanwinedata)
model2D = lda(quality_dummy~poly(alcohol)+pH+residual.sugar+density+chlorides+
                volatile.acidity+fixed.acidity+total.sulfur.dioxide
              +sulphates+ free.sulfur.dioxide+citric.acid)
guess2D = predict(model2D, cleanwinedata)
winewith2Dguesses = mutate(cleanwinedata, guess=guess2D$class,
                           isError = ifelse(guess==quality_dummy, as.character(guess),
                                            "error"))
View(winewith2Dguesses)

ggplot(data= winewith2Dguesses )+geom_bar( aes(x= quality_dummy, fill=isError))+
  ylab("Number of wine")+
  xlab("Grouped quality of wine")+
  labs(color = "outcomes")+
  ggtitle("visualising error percentage in classifying wines")

ggplot(data=winewith2Dguesses)+
  geom_bar(aes(x= quality_dummy,
               fill=as.factor(quality_dummy)))+scale_fill_manual(values = c( "light green", "orange"))+
  ylab("Number of wine")+
  xlab("Grouped quality of wine")+
  labs(color = "outcomes")+
  ggtitle("visualising actual number of wines classified as group")


#cross validation for changed quality

wineMix = slice(cleanwinedata, sample(1:n()))

summary(cleanwinedata)

summary(wineMix)

id = seq(1, 1143, by=1)

wineRando = mutate(wineMix, id)

k = 5

numRows = nrow(cleanwinedata)

train = filter(wineRando, id <= 4*numRows/k)

test = anti_join(wineRando, train, by="id")

View(train)

View(test)

library(MASS)

attach(cleanwinedata)

k = 5

numRows = nrow(cleanwinedata)

errors_1 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~alcohol+pH+residual.sugar+density, train)
  
  modelGuesses = predict(model, test)
  
  errors_1[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_1[i]+totalError
  
}

errors_1

totalError

avgerror=totalError/k

avgerror

#second model
errors_2 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides, train)
  
  modelGuesses = predict(model, test)
  
  errors_2[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_2[i]+totalError
  
}

errors_2

totalError

avgerror=totalError/k

avgerror

#third model

errors_3 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides+
                volatile.acidity, train)
  
  modelGuesses = predict(model, test)
  
  errors_3[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_3[i]+totalError
  
}

errors_3

totalError

avgerror=totalError/k

avgerror

#fourth model

errors_4 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides+
                volatile.acidity+fixed.acidity+total.sulfur.dioxide, train)
  
  modelGuesses = predict(model, test)
  
  errors_4[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_4[i]+totalError
  
}

errors_4

totalError

avgerror=totalError/k

avgerror

#fifth model

errors_5 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~alcohol+pH+residual.sugar+density+chlorides+
                volatile.acidity+fixed.acidity+total.sulfur.dioxide
              +sulphates, train)
  
  modelGuesses = predict(model, test)
  
  errors_5[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_5[i]+totalError
  
}

errors_5

totalError

avgerror=totalError/k

avgerror

#sixth model

errors_6 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~poly(alcohol,10)+poly(pH,10)+poly(residual.sugar,10)+poly(density,10)+poly(chlorides,10)+poly(volatile.acidity,10)
              +poly(fixed.acidity,10)+poly(total.sulfur.dioxide,10)+poly(sulphates,10)+ poly(free.sulfur.dioxide,10)+poly(citric.acid,10)
              , train)
  
  modelGuesses = predict(model, test)
  
  errors_6[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_6[i]+totalError
  
}

errors_6

totalError

avgerror=totalError/k

avgerror

#seventh model

errors_7 = rep(0, k)

totalError = 0

for(i in 1:k){
  
  test = filter(wineRando, id >= (i-1)*numRows/k+1 & id <= i*numRows/k)
  
  train = anti_join(wineRando, test, by="id")
  
  model = lda(quality_dummy~poly(alcohol,8)+pH+poly(residual.sugar,4)+density+poly(chlorides,6)+poly(volatile.acidity,8)+fixed.acidity+total.sulfur.dioxide
              +sulphates
              , train)
  
  modelGuesses = predict(model, test)
  
  errors_7[i] = 1-mean(modelGuesses$class == test$quality_dummy)
  
  totalError = errors_7[i]+totalError
  
}

errors_7

totalError

avgerror=totalError/k

avgerror

errors_1

errors_2

errors_3

errors_4

errors_5

errors_6

errors_7


avgE=rep(0,7)

avgE

for(i in i:k){
  
  avgE[1]=errors1[i]+avgE[1]
  
  avgE[2]=errors2[i]+avgE[2]
  
  avgE[3]=errors3[i]+avgE[3]
  
  avgE[4]=errors4[i]+avgE[4]
  
  avgE[5]=errors5[i]+avgE[5]
  
  avgE[6]=errors6[i]+avgE[6]
  
  avgE[7]=errors7[i]+avgE[7]
  
}

avgE[1]

avgE[2]

avgE[3]

avgE[4]

avgE[5]

avgE[6]

avgE[7]


se=rep(0,k)

se[1]=sqrt(var(errors1)/k)

se[2]=sqrt(var(errors2)/k)

se[3]=sqrt(var(errors3)/k)

se[4]=sqrt(var(errors4)/k)

se[5]=sqrt(var(errors5)/k)

se[6]=sqrt(var(errors6)/k)

se[7]=sqrt(var(errors7)/k)


mn=seq(1,7, by=1)
length(avgE)
length(se)

cross_validation2 = data.frame(avgE, se)

View(cross_validation2)



#plotting data

library(tidyverse)

ggplot(cross_validation2, aes(x=mn,y=avgE))+
  
  geom_line()+
  
  geom_point()+
  
  geom_errorbar(aes(ymin=avgE-se, ymax=avgE+se))+
  
  xlab("Model Complexity")+
  
  ylab("Classification Error")+
  
  ggtitle("Classification Error vs Model Complexity ")

#clustering:
#1D datsaset
Selectivewine = dplyr::select(cleanwinedata, fixed.acidity)
m1 = kmeans(Selectivewine, 4, nstart=100)
m1$size
#histogram for fixed acidity
#k value is 3
m1 = kmeans(Selectivewine, 3, nstart=10)
m1$size

ggplot(data = Selectivewine, aes(x = fixed.acidity, fill = factor(m1$cluster)))+
  geom_histogram(stat = "count")+xlab("Fixed acidity Level")+
  ylab("Wine Count")+ labs(colour = "Cluster Number")+
  ggtitle("Histogram on Fixed Acidity")+scale_colour_discrete(name = "Fixed Acidity Clusters", 
                                                              labels = c("less acidic", "moderately acidic"
                                                                         ,"highly acidic"))

#2 dimensional 
#k3 pH~alcohol
winescale = scale(cleanwinedata)
winescale = data.frame(winescale)
Selectivewine1 = dplyr::select(winescale, alcohol,pH)
M2 = kmeans(Selectivewine1, 3, nstart = 10)
ggplot(data = Selectivewine1, aes(x=alcohol, y = pH, colour = factor(M2$cluster)))+
  geom_point()+
  xlab("Alcohol level")+
  ylab("pH Value")+
  labs(colour = "heart rate range clusters")+
  ggtitle("Relationship between pH~ Alcohol")+
  scale_colour_discrete(name = "pH & alcohol", 
                        labels = c("cluster 1", "cluster 2"
                                   ,"cluster 3"))


#clustering using 2 dimensional
#density~residual sugar
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

#clustering using 2 dimensional
#ph~ chlorides
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

######################PLOTTING GRAPHS FOR DATA VISUALISATION####################
#data visualisation
attach(cleanwinedata)
view(cleanwinedata)
ggplot(data= cleanwinedata)+
  geom_histogram(aes(x=fixed.acidity),
                 position = position_dodge2(padding = .1,
                                            preserve = "single"))+
  ylab("Wine Count")+
  xlab("Fixed acidity Level")+
  ggtitle("Histogram on Fixed Acidity ")

#quality vs alcohol level
ggplot(data = rawwinedata)+
  geom_point(mapping = aes (y = alcohol, x = citric.acid, color = (citric.acid)))+
  ylab("Alcohol level in of wine")+
  xlab("Citric acid level in wine")+
  ggtitle("Relationshhip between Citric acid and Alcohol level")

#point graph for chlorides and pH
ggplot(data = rawwinedata)+
  geom_point(mapping = aes (y = chlorides, x = pH))+
  xlab("pH of wine")+
  ylab("Chloride level in wine")+
  ggtitle("Relationshhip between pH and Chloride level")

#point graph for density and residual sugar
ggplot(data = rawwinedata)+
  geom_point(mapping = aes (x = density, y = residual.sugar))+
  ylab("Residual Sugar level of wine")+
  xlab("Density of wine")+
  ggtitle("Relationshhip between pH and Chloride level")

#boxplot quality and pH
ggplot(data = rawwinedata)+
  geom_boxplot(mapping = aes (x = as.factor(quality), y = pH, color =as.factor(quality)))+
  ylab("pH of wine")+
  xlab("Quality level of wine")+
  ggtitle("Relationshhip between pH level and quality")+
  scale_colour_discrete(name = "quality of wine")

#boxplot for quality and alcohol
ggplot(data = rawwinedata)+
  geom_boxplot(mapping = aes (x = as.factor(quality), y = alcohol, color =as.factor(quality)))+
  ylab("Alcohol level in wine")+
  xlab("quality of wine")+
  ggtitle("Relationshhip between quality and alcohol")+
  scale_colour_discrete(name = "quality of wine")
