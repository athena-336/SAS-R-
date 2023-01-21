library(tidyverse)
library(readr)
library(ggplot2)

data1<-data[c(2,3,6)]
str(data1)
summary(data)

data %>%
  group_by(condition) %>%
  summarise(clicked_share_mean = mean(clicked_share))

### Ha:  mu_tips - mu_tools >0
t.test(data[data1$condition == 'tips', ]$clicked_share,
       data[data1$condition == 'tools', ]$clicked_share,
       alternative = "greater")

#reject H0, p-value is less than the significance level 0.05
#tips>tools 多寫建議的文章

ggplot(data, aes(x =visit_date, y = clicked_share,colour = as.factor(condition))) + 
  geom_point() + geom_line() +
  xlab("Date") + ylab("clicked_share") +
  ggtitle("Time Series Plot of clicked_share") +
  theme_bw()



