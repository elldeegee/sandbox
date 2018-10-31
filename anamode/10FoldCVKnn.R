library(readr)
library(class)
library(caTools)
Book <- read_csv("BookBindersAll.csv")
set.seed(123)
Book$Foldid <- sample(1:10, nrow(Book), replace = TRUE)
FoldPre<-numeric(10)
for (i in 1:10){
train<-subset(Book,Book$Foldid != i)
test<-subset(Book, Book$Foldid == i)
knn3<-knn(train[-1],test[-1],train$Purchase,k=3)
CF<-table(knn3,test$Purchase)
Precision<-CF[2,2]/(CF[2,1]+CF[2,2])
FoldPre[i]<-Precision
}
FoldPre
