library(readr)
BookBindersTrain <- read_csv("BookBindersTrain.csv")

BB_Reg <- lm(BookBindersTrain$Purchase~BookBindersTrain$Gender+BookBindersTrain$`Amount purchased`+
               BookBindersTrain$Frequency+BookBindersTrain$`Last purchase`+ BookBindersTrain$P_Art)

summary(BB_Reg)