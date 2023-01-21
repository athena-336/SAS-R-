library(datasets)
library(tidyverse)
library(magrittr)
library(cluster)
library(NbClust)
library(factoextra)

#data(train)

#2
newdata <- train %>%
  mutate(
    #type = ifelse(TypeofTravel=='Business travel',1,0),
    #age = (train$Age - min(train$Age)) / (max(train$Age) - min(train$Age)),
    online.boarding=((train$Online_boarding) - min(train$Online_boarding)) / (max(train$Online_boarding) - min(train$Online_boarding)),
    gender = ifelse(train$Gender=='Male',1,0)
  )

view(newdata)
newdata<-newdata[,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)]
ggplot(newdata, aes(x=Age, y=online.boarding)) +
  geom_point()

fviz_nbclust(newdata[,1:2], FUN = kmeans, method = "silhouette")

km <- kmeans(newdata[,1:2], centers=2,nstart=20)

install.packages("useful")
library(useful)

plot(km, data=newdata, class="gender")
