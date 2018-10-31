## Load packages
library(readr)
library(class)
library(caTools)
library(dplyr)
library(tree)

## Load data
boat <- read_csv("train.csv") %>% ungroup()

## clean age
avg_age <- mean(boat$Age, na.rm = TRUE)
boat <- boat %>% mutate(Age = if_else(is.na(Age), avg_age, Age))

## clean port embarked
boat <- boat %>% mutate(Embarked = if_else(is.na(Embarked), "S", Embarked))

## numerical gender
boat <- boat %>% mutate(Male = if_else(boat$Sex == "male", 1,0))

## clean fare
avg_fare <- mean(boat$Fare, na.rm = TRUE)
boat <- boat %>% mutate(Fare = if_else(is.na(Fare), avg_fare, Fare))

## Add child and alone variables
boat <- boat %>% mutate(child = if_else(Age < 16, 1, 0),
                        alone = if_else(SibSp == 0, 1, 0),
                        Price = if_else(Fare< 26,1,0))

## linear regression -- train
boat4LR <- boat
boat4LR$Survived<-as.factor(boat4LR$Survived)
boatLR<-glm(Survived~Sex+Pclass+child+alone+Price,family=binomial(),data=boat4LR)
boat4LR.prob<- predict(boatLR,boat4LR, type="response")
CF<-table(boat4LR$Survived,boat4LR.prob>.6)
Precision<-CF[2,2]/(CF[2,1]+CF[2,2])
Accuracy<-(CF[2,2]+CF[1,1])/nrow(boat)

## load and clean test data
boattest <- read_csv("test.csv") %>% ungroup()
avg_age_test <- mean(boattest$Age, na.rm = TRUE)
avg_fare_test <- mean(boattest$Fare, na.rm = TRUE)
boattest <- boattest %>% mutate(Age = if_else(is.na(Age), avg_age_test, Age),
                                Embarked = if_else(is.na(Embarked), "S", Embarked),
                                Fare = if_else(is.na(Fare), avg_fare, Fare),
                                Male = if_else(boattest$Sex == "male", 1,0))

## Add child and alone variables
boattest <- boattest %>% mutate(child = if_else(Age < 16, 1, 0),
                                alone = if_else(SibSp == 0, 1, 0),
                                Price = if_else(Fare< 26,1,0))

## linear regression -- test
boattest$survive.prod<-predict(boatLR,boattest,type="response")
boattest$Survived<-round(boattest$survive.prod)

## export linear results
linear_results <- boattest %>% select(PassengerId,Survived)
write_csv(linear_results,"~/Desktop/linear_results.csv")

## Decision trees
BoatTree <- tree(Survived ~ Pclass+Male+child+alone+Price,data=boat)
BoatPred<- predict(BoatTree,boattest)
tree_reults <- data_frame(PassengerID = boattest$PassengerId, lilklihood = BoatPred) %>%
  mutate(Survived = if_else(lilklihood > .5, 1,0)) %>% select(PassengerID,Survived)

## export decision tree model
write_csv(tree_reults,"~/Desktop/tree_results.csv")
