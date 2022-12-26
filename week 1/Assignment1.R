library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(scales)
suicide <- read_csv("C:/Users/abila/OneDrive/Desktop/dataset.csv")
view(suicide)
# Exploring data
class(suicide) #data frame

dim(suicide) #view number of rows and columns

names(suicide) #view column names

head(suicide) #view first few observations

str(suicide) #view the structure of data

summary(suicide)
#clean dataset
colSums(is.na(suicide))
cleandata <- suicide[complete.cases(suicide),]

view(cleandata)

class(cleandata)

dim(cleandata)

###renaming column names to convinience###
cleandata <- cleandata %>% 
  rename(
    "homicides_rate" = "Intentional homicides (per 100,000 people)",
    "suicide_rate" = "Suicide mortality rate (per 100,000 population)",
    "GDP" = "GDP (current US$)",
    "GDP_perCapita" = "GDP per capita, PPP (current international $)")
  
str(cleandata)
head(cleandata)
###converting character to factor###
cleandata$adminregion <- as.factor(cleandata$adminregion)
cleandata$iso3c <- as.factor(cleandata$iso3c)
cleandata$iso2c <- as.factor(cleandata$iso2c)
cleandata$incomeLevel <- as.factor(cleandata$incomeLevel)

####question1###
subdata <-  cleandata[which(cleandata$adminregion == "East Asia & Pacific (excluding high income)" & 
                              cleandata$year == 2008),
                      
                      names(cleandata) %in% 
                        c("iso3c","suicide_rate")]
ggplot(data = subdata, aes(fill=iso3c,x=suicide_rate, y= round(max(suicide_rate)))) + 
  geom_line(position_dodge(width= 1), stat="identity") 
ggplot(data=cleandata)+
  geom_bar(aes(x=year, fill= "Intentional homicides (per 100,000 people)")
  )+
ggtitle("Intentional homicides (per 100,000 people)")
####question2### how each income type country differs by homicide rate
#low income
subdata <-  cleandata[which(cleandata$country == "Afghanistan"),
                      names(cleandata) %in% 
                        c("year","homicides_rate")]

ggplot(subdata, aes(x=year, y=homicides_rate,)) +
  geom_line() +
  ggtitle("Homicide rate in Afghanistan")


#lower middle
subdata <-  cleandata[which(cleandata$country == "India"),
                      names(cleandata) %in% 
                        c("year","homicides_rate")]

ggplot(subdata, aes(x=year, y=homicides_rate)) +
  geom_line() +
  ggtitle("Homicide rate in India")

#upper middle
subdata <-  cleandata[which(cleandata$country == "Turkey"),
                      names(cleandata) %in% 
                        c("year","homicides_rate")]


ggplot(subdata, aes(x=year, y=homicides_rate)) +
  geom_line()+
  ggtitle("Homicide rate in Turkey")


###question2### suicide rates corresponding to admin region
subdata <-  cleandata[which(cleandata$country == "Sri Lanka" | cleandata$country == "Kenya" | cleandata$country == "Lebanon" | cleandata$country == "Argentina" | cleandata$country == "North Macedonia" ),
                      names(cleandata) %in% 
                        c("iso3c","year","suicide_rate")]
subdata <- subdata[which(subdata$year == 2010 | subdata$year == 2011 | subdata$year == 2012 | subdata$year == 2013 | subdata$year == 2014 | subdata$year == 2015 |subdata$year == 2016 |subdata$year == 2017 | subdata$year == 2018 ),
                   names(subdata) %in% 
                     c("iso3c","year","suicide_rate")]

ggplot(data = subdata, aes(fill=c(iso3c), x=year, y=suicide_rate)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_discrete("Country and Admin Region", 
                      labels = c("Argentina - Latin America & Caribbean" , 
                                 "Kenya - Sub-Saharan Africa" , 
                                 "Lebanon - Middle East & North Africa" ,
                                 "Sri Lanka - South Asia" ,
                                 "North Macedonia - Europe & Central Asia")) +
  ggtitle("Suicide rate trend")


