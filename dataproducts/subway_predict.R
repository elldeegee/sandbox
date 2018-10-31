## Load packages
library(readr)
library(class)
library(caTools)
library(dplyr)

## set wd
setwd("~/Documents/Repositories/sandbox/dataproducts/")

## Load data
data <- read_csv("turnstile_weather_v2.csv") %>% ungroup()

RI_only <- data ## %>% filter(station == "ROOSEVELT IS")

## split train and test data
set.seed(123) 
sample<-sample.split(RI_only$rain, SplitRatio = .80)
train<-subset(RI_only, sample == TRUE)
test<-subset(RI_only, sample == FALSE)

## linear model
subway_LR<-lm(ENTRIESn_hourly~hour + weekday + rain + precipi + UNIT,data=RI_only)
summary(subway_LR)

## Decision tree
SubwayTree <- tree(ENTRIESn_hourly ~ hour + weekday,data=train)
TreePred<- predict(SubwayTree,test)
x <- data_frame(test = test$ENTRIESn_hourly, TreePred)
y <- x %>% mutate(difsq = (test-TreePred)^2)
sum(y$difsq)

## KNN
train_knn<-train %>% select(UNIT,ENTRIESn_hourly,EXITSn_hourly,hour,day_week,weekday,fog,rain) ## Specifying columns to select
test_knn<-test %>% select(UNIT,ENTRIESn_hourly,EXITSn_hourly,hour,day_week,weekday,fog,rain)
knn3<-knn(train_knn[-1],test_knn[-1],train_knn$ENTRIESn_hourly,k=7)
knn3
x <- data_frame(test = test_knn$ENTRIESn_hourly, knn3 = as.integer(knn3))
y <- x %>% mutate(difsq = (test-knn3)^2)
sum(y$difsq)