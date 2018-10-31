boat<- read_csv("train.csv")
mean(boat$Survived)
boat$Survived<-as.factor(boat$Survived)
boatLR<-glm(Survived~Sex+Pclass,family=binomial(),data=boat)
boat.prob<- predict(boatLR,boat, type="response")
CF<-table(boat$Survived,boat.prob>.5)
CF
Precision<-CF[2,2]/(CF[2,1]+CF[2,2])
Precision
Accuracy<-(CF[2,2]+CF[1,1])/nrow(boat)
Accuracy

boattest<- read_csv("test.csv")
boattest$survive.prod<-predict(boatLR,boattest,type="response")
boattest$survive<-round(boattest$survive.prod)
View(boattest)

submission$PassengerID <-boattest$PassengerId
submission$Survived <-boattest$survive
write.csv(submission, file = "submission.csv")

# other variables...?
sum(is.na(boat$Age))
boat$AgeAVG<-boat$Age
boat<-within(boat,AgeAVG[is.na(Age)]<-mean(Age,na.rm=TRUE))
sum(is.na(boat$AgeAVG))

sum(is.na(boat$Embarked))
table(boat$Embarked,boat$Embarked)
