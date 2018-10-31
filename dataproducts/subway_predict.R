## Load packages
library(readr)
library(class)
library(caTools)
library(dplyr)

## set wd
setwd("~/Documents/Repositories/sandbox/dataproducts/")

## Load data
data <- read_csv("turnstile_weather_v2.csv") %>% ungroup()

RI_only <- data %>% filter(station == "ROOSEVELT IS")

## split train and test data
set.seed(123) 
sample<-sample.split(RI_only$rain, SplitRatio = .80)
train<-subset(RI_only, sample == TRUE)
test<-subset(RI_only, sample == FALSE)

## linear model
subway_LR<-lm(ENTRIESn_hourly~hour+rain+weekday,tempi,data=RI_only)
summary(subway_LR)
