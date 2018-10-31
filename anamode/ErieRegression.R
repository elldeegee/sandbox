library(readr)
#load data
ESData<- read_csv("ErieSteelDataOnly.csv")
View(ESData)
#Regression
ESData$Touches<-ESData$Pieces*ESData$Operations
ESReg<-lm(ESData$Interval~ESData$Operations+ESData$Normal+ESData$Touches+ESData$Pieces)
summary(ESReg)


#Summarize
head(ESData)
summary(ESData)
mean(ESData$Interval)
aggregate(Interval~Normal,ESData,mean)
cor(ESData$Interval,ESData$Pieces)
cor(ESData[1],ESData[4])
aggregate(Interval~Normal,ESData,mean)
#Plots
plot(ESData$Interval~ESData$Pieces)
plot(Interval~Pieces, data=ESData)
abline(lm(ESData$Interval~ESData$Pieces), col="darkgreen")
plot(Interval[which(Normal==1)]~Pieces[which(Normal==1)],data=ESData,     
     col="red",xlab="# Pieces", ylab="Completion time (mins)",main="Erie Steel",     
     cex.lab=.75,cex.axis=.5,xlim=c(0,1000),ylim=c(0,1000))
par(new=T)
plot(Interval[which(Normal==0)]~Pieces[which(Normal==0)],data=ESData,     xlab='', ylab='', axes=F, col="green",     cex.lab=.75,cex.axis=.5,xlim=c(0,1000),ylim=c(0,1000))
legend("topright",legend=c("Normal","Rush"),pch=c(1,1),
       col=c("green", "red"),cex=.75)
#Regression
ESData$Touches<-ESData$Pieces*ESData$Operations
ESReg<-lm(ESData$Interval~ESData$Pieces+ESData$Operations+ESData$Normal+ESData$Touches)
summary(ESReg)
cor(ESData[c(1,2,4,5)],ESData[c(1,2,4,5)])
ESReg<-lm(ESData$Interval~ESData$Operations+ESData$Normal+ESData$Touches)
summary(ESReg)

coef(ESReg)
ESRegCoef<-coef(ESReg)
round(ESRegCoef[1]+ESRegCoef[2]*7+ESRegCoef[3]*0+ESRegCoef[4]*500*7,2)

