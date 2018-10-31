## Load packages
library(readr)
library(class)
library(caTools)
library(tree)

## Load data
Book <- read_csv("BookBindersAll.csv")
Book$Purchase<-as.factor(Book$Purchase)

## model
set.seed(123) 
sample<-sample.split(Book$Purchase, SplitRatio = .80)
train<-subset(Book, sample == TRUE)
test<-subset(Book, sample == FALSE)
BookTree <- tree(Purchase ~ Gender+P_Art+Frequency,data=train)

## Plot
plot(BookTree) ## Left is always no, right is always yes
text(BookTree, cex=.5)
summary(BookTree) ## The only purchasers according to this tree bought this had P_Art >= 1.5 (P_Art = c(0:5))

BookPred<-predict(BookTree,train,type="class")
CF<-table(BookPred,train$Purchase)
CF
Precision<-CF[2,2]/(CF[2,1]+CF[2,2])
Precision
Accuracy<-(CF[2,2]+CF[1,1])/nrow(train)
Accuracy
