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

library(GGally)
ggpairs(cleandata, aes(colour = HeartDisease))
attach(cleandata)
str(cleandata)

#linear discriminant analysis
# TRYING MODELS BEFORE SPLITTING THE DATASET INTO TEST AND TRAIN DATA
attach(cleandata)

lda1 = lda(HeartDisease~Cholesterol)
lda1
lda2 = lda(HeartDisease~Cholesterol+RestingBP)
lda2
lda3 = lda(HeartDisease~Cholesterol+RestingBP+FastingBS)
lda3

ldaPred = predict(lda1, cleandata)
ldaPred2 = predict(lda2, cleandata)
ldaPred3 = predict(lda3, cleandata)

#confusion matrix
table(ldaPred$class, cleandata$HeartDisease)
table(ldaPred2$class, cleandata$HeartDisease)
table(ldaPred3$class, cleandata$HeartDisease)

mean(ldaPred$class== cleandata$HeartDisease) 
mean(ldaPred2$class== cleandata$HeartDisease)
mean(ldaPred3$class== cleandata$HeartDisease)
view(cleandata)

#spliting training and test data

IDheart = mutate(cleandata, id=row_number())
View(IDheart)
trainDataSet = sample_frac(IDheart, .5)
View(trainDataSet)
testDataSet = anti_join(IDheart, trainDataSet, by = "id")
View(testDataSet)

#creating model
heartmodel = lda(HeartDisease~Cholesterol+RestingBP+FastingBS , data = trainDataSet)
heartmodel
hearttrainSetPrediction = predict(heartmodel, trainDataSet)
table(hearttrainSetPrediction$class, trainDataSet$HeartDisease)
mean(hearttrainSetPrediction$class== trainDataSet$HeartDisease) 

#running model for test data
heartTestSetprediction = predict(heartmodel,testDataSet )
View(heartTestSetprediction$class)
View(testDataSet$HeartDisease)
table(heartTestSetprediction$class , testDataSet$HeartDisease)
mean(heartTestSetprediction$class == testDataSet$HeartDisease) 

#creating second model
heartmodel2 = lda(HeartDisease~ Age+MaxHR, data = trainDataSet)
heartmodel2
hearttrainSetPrediction2 = predict(heartmodel2, trainDataSet)
table(hearttrainSetPrediction2$class, trainDataSet$HeartDisease)
mean(hearttrainSetPrediction2$class== trainDataSet$HeartDisease)

#running model for test data
heartTestSetprediction2 = predict(heartmodel2,testDataSet )
View(heartTestSetprediction2$class)
View(testDataSet$HeartDisease)
table(heartTestSetprediction2$class , testDataSet$HeartDisease)
mean(heartTestSetprediction2$class == testDataSet$HeartDisease)

#creating third model
heartmodel3 = lda(HeartDisease~ Age+MaxHR+FastingBS, data = trainDataSet)
heartmodel3
hearttrainSetPrediction3 = predict(heartmodel3, trainDataSet)
table(hearttrainSetPrediction3$class, trainDataSet$HeartDisease)
mean(hearttrainSetPrediction3$class== trainDataSet$HeartDisease)

#running model for test data
heartTestSetprediction3 = predict(heartmodel3,testDataSet )
View(heartTestSetprediction2$class)
View(testDataSet$HeartDisease)
table(heartTestSetprediction3$class , testDataSet$HeartDisease)
mean(heartTestSetprediction3$class == testDataSet$HeartDisease)

#creating fourth model
heartmodel4 = lda(HeartDisease~ Age+MaxHR+FastingBS+ChestPainType, data = trainDataSet)
heartmodel4
hearttrainSetPrediction4 = predict(heartmodel4, trainDataSet)
table(hearttrainSetPrediction4$class, trainDataSet$HeartDisease)
mean(hearttrainSetPrediction4$class== trainDataSet$HeartDisease)

#running model for test data
heartTestSetprediction4 = predict(heartmodel4,testDataSet )
View(heartTestSetprediction4$class)
View(testDataSet$HeartDisease)
table(heartTestSetprediction4$class , testDataSet$HeartDisease)
mean(heartTestSetprediction4$class == testDataSet$HeartDisease)

#creating fifth model
heartmodel5 = lda(HeartDisease~ Age+MaxHR+FastingBS+ChestPainType+RestingECG, data = trainDataSet)
heartmodel5
hearttrainSetPrediction5 = predict(heartmodel5, trainDataSet)
table(hearttrainSetPrediction5$class, trainDataSet$HeartDisease)
mean(hearttrainSetPrediction5$class== trainDataSet$HeartDisease)

#running model for test data
heartTestSetprediction5 = predict(heartmodel5,testDataSet )
View(heartTestSetprediction5$class)
View(testDataSet$HeartDisease)
table(heartTestSetprediction5$class , testDataSet$HeartDisease)
mean(heartTestSetprediction5$class == testDataSet$HeartDisease)

#creating sixth model
heartmodel6 = lda(HeartDisease~Age+Sex+Oldpeak+ExerciseAngina, data = trainDataSet)
heartmodel6
hearttrainSetPrediction6 = predict(heartmodel6, trainDataSet)
table(hearttrainSetPrediction6$class, trainDataSet$HeartDisease)
mean(hearttrainSetPrediction6$class== trainDataSet$HeartDisease) 

#running model for test data
heartTestSetprediction6 = predict(heartmodel6,testDataSet )
View(heartTestSetprediction6$class)
View(testDataSet$HeartDisease)
table(heartTestSetprediction6$class , testDataSet$HeartDisease)
mean(heartTestSetprediction6$class == testDataSet$HeartDisease)

#plotting relationships
#histogram for age
ggplot(data= cleandata)+
  geom_histogram(aes(x=Age),
                 position = position_dodge2(padding = .3,
                                            preserve = "single"))+
  ylab("number of patients")+
  xlab("Age group")+
  ggtitle("Age vs heart disease")

#box plot for sex vs cholesterol
ggplot(data = cleandata)+
  geom_boxplot(mapping = aes(y = factor(Sex), x = Cholesterol, colour = Sex))+
  ylab("Sex")+
  xlab("cholesterol in mm/dl")+
  labs(color = "Sex colour index")+
  ggtitle("Sex vs Cholesterol")

#bar plot for chest pain type
ggplot(data=cleandata)+
  geom_bar(mapping = aes(y=ChestPainType, fill = ChestPainType))+
  ylab("TYPE OF CHEST PAIN")+
  xlab("NUMBER OF PATIENTS")+
  ggtitle("type of chest pain")

#box plot for sex vs Resting Blood Pressure
ggplot(data = cleandata)+
  geom_boxplot(mapping = aes(y = factor(Sex), x = RestingBP, colour = Sex))+
  ylab("gender of the individual")+
  xlab("blood pressure in mm hg")+
  labs(color = "Sex colour index")+
  ggtitle("Sex vs Resting Blood Pressure")

#resting graph
ggplot(data=cleandata)+
  geom_bar(mapping = aes(x= RestingECG, fill = Sex))+
  ylab("Individual count")+
  xlab("Resting ECG type")+
  ggtitle("Type of resting ECG")

#creating conclusion graph to visualize the error.
attach(cleandata)
model1D = lda(HeartDisease~ Age+MaxHR+FastingBS+ChestPainType)
guess1D = predict(model1D, cleandata)
heartwith1Dguesses = mutate(cleandata, guess=guess1D$class,
                            isError = ifelse(guess==HeartDisease, as.character(guess),
                                             "error"))
View(heartwith1Dguesses)

ggplot(data= heartwith1Dguesses )+geom_bar( aes(x= ChestPainType, fill=isError))+scale_fill_manual(values = c("green", "black", "red", "blue"))+
  ylab("number of patients")+
  xlab("Types of chest pain")+
  labs(color = "outcomes")+
  ggtitle("visualising error percentage in chest pain type")

ggplot(data=cleandata)+geom_bar(aes(x= ChestPainType,
                                    fill=HeartDisease))+scale_fill_manual(values = c( "black", "red", "blue"))+
  ylab("number of patients")+
  xlab("Types of chest pain")+
  labs(color = " heart disease outcomes")+
  ggtitle("categorising patients based on heart disease 
          and chest pain type")
