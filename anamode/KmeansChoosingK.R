RFM <- read.table(file="RetailRFM.csv", header=TRUE,sep=",")
RFMs<-data.frame(scale(RFM[-1]))
RFMClust<-kmeans(RFMs,centers=2)
RFMClust$totss
RFMClust$withinss
RFMClust$betweenss
RFMClust$betweenss/RFMClust$totss

wss<-numeric(10)
for(i in 1:10){wss[i]<-sum(kmeans(RFMs,centers=i)$withinss)}
plot(wss,type="b")

RFMClust<-kmeans(RFMs,centers=4)
RFM$Segment<-RFMClust$cluster
head(RFM)
aggregate(RFM[c("RECENCY","FREQUENCY","MONETARY")],
          by=list(Segment=RFM$Segment),mean)
