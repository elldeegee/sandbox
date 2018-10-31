## Load packages
library(readr)
library(class)
library(caTools)

## Load data
Book <- read_csv("BookBindersAll.csv")
head(Book)
mean(Book$Purchase)

## Model
set.seed(123) 
sample<-sample.split(Book$Purchase, SplitRatio = .80)
train<-subset(Book, sample == TRUE)
test<-subset(Book, sample == FALSE)
knn3<-knn(train[-1],test[-1],train$Purchase,k=3) ## Removing purchase column because that's not an attribute
knn3
CF<-table(knn3,test$Purchase)
CF
Precision<-CF[2,2]/(CF[2,1]+CF[2,2])
Precision
Accuracy<-(CF[2,2]+CF[1,1])/nrow(test) ## Even though we input both train and test data, we are evaluating test
Accuracy
HitRate<-CF[2,2]/(CF[2,1]+CF[2,2])
HitRate