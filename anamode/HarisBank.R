library(readr)
#load data
harrisData<- read_csv("HarrisBankDataOnly.csv")
harrisData$LN_SALARY<-log(harrisData$SALARY)

#summarize
summary(harrisData)
cor(harrisData, harrisData)
plot(harrisData$SALARY~harrisData$EDUCAT)

#regression
harrisReg<-lm(LN_SALARY~MALES+MONTHS+EXPER+EDUCAT, data = harrisData)
summary(harrisReg)
exp(coef(harrisReg))