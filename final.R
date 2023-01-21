library(tidyverse)
str(DataD)
DataD$Industry <-as.character(DataD$Industry)
DataD$Country <-as.factor(DataD$Country)

boxplot(DataD$Networth~DataD$Industry)
Networth.means = tapply(#tapply���ư���
  DataD$Networth, 
  DataD$Industry, 
  mean)
par(las = 1)
barplot(Networth.means, 
        ylab = "Industry", 
        xlab = "Mean of Networth",
        horiz = TRUE)
arrange(summarise(group_by(DataD,Industry),mean=mean(Networth)),desc(mean))

boxplot(DataD$Networth~DataD$Country)
Networth.means = tapply(#tapply���ư���
  DataD$Networth, 
  DataD$Country, 
  mean)
par(las = 1)
barplot(Networth.means, 
        xlab = "Country", 
        ylab = "Mean of Networth",
        horiz = TRUE)
arrange(summarise(group_by(DataD,Country),mean1=mean(Networth)),desc(mean1))

summary(DataD)



#data<-select(DataD,3,5,6,7)
#�i��ʪ�3D��
#colors <- c("darkorange", "hotpink", "limegreen")
#colors <- colors[as.numeric(DataD$Industry)]
#���ܮھڤ��PSPECIES�h�e���P�C��
#library(rgl)
#plot3d(DataD[,3],DataD[,5],DataD[,7],col=colors )
#plot3d(data[,1:3] )