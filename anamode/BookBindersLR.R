library(readr)
Book <- read_csv("BookBindersTrain.csv")
head(Book)
mean(Book$Purchase)
BookReg<-lm(Purchase~Gender+`Amount purchased`+Frequency+`Last purchase`+P_Art,data=Book)
summary(BookReg)


Book$Purchase<-as.factor(Book$Purchase)
BookLR<-glm(Purchase~Gender+`Amount purchased`+Frequency+`Last purchase`+P_Art,family=binomial(),data=Book)
Book.prob<- predict(BookLR,Book, type="response")
CF<-table(Book$Purchase,Book.prob>.3)
CF
Precision<-CF[2,2]/(CF[1,2]+CF[2,2])
Precision
Accuracy<-(CF[2,2]+CF[1,1])/nrow(Book)
Accuracy
HitRate<-CF[2,2]/(CF[2,1]+CF[2,2])
HitRate

## Testing data
BookTest <- read_csv("BookBindersTest.csv")
Book.prob<- predict(BookLR,BookTest, type="response")
CF<-table(BookTest$Purchase,Book.prob>.3)
CF
Precision<-CF[2,2]/(CF[1,2]+CF[2,2])
Precision
Accuracy<-(CF[2,2]+CF[1,1])/nrow(BookTest)
Accuracy
