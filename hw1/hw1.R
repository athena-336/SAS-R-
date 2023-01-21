#install.packages("tidyverse")
#library(tidyverse)

#library(readr)
#product <- read_csv("hw1_product_list.csv")
#client <- read_csv("hw1_client_list.csv")
#sales <- read_csv("hw1_salesdata.csv")
#已用IMPORT方式輸入三筆CSV
install.packages("rmarkdown")
library(magrittr)
#1
test %<>% separate(Item,into=c("Product", "Item"), sep = "_")  

#2
str(sales)
str(test)
str(client)
sales$Product<-as.character(sales$Product)
sales %>% full_join(test) 
full.table <- sales %>% full_join(client)

#3
full.table%<>%mutate(spend=UnitPrice*Quantity)

#4
  group1<-filter(full.table,Membership=="diamond"|Membership=="gold")%>% 
  select(Membership,Age,UnitPrice,spend,Region)
  taiwan1<-filter(group1,Region=="Taiwan")
 
  group2<-filter(full.table,Membership=="silver"|Membership=="basic")%>% 
  select(Membership,Age,UnitPrice,spend,Region)
  taiwan2<-filter(group2,Region=="Taiwan")
group1%<>%
  summarise( 
    name="DiamondAndGold",
    count = n(),
    avg_age = mean(Age),
    avg_unitPrice=mean(UnitPrice),
    avg_spend=mean(spend),
    Taiwan=count(taiwan1),
  )
group2%<>%
  summarise( 
    name="BasicAndSilver",
    count = n(),
    avg_age = mean(Age),
    avg_unitPrice=mean(UnitPrice),
    avg_spend=mean(spend),
    Taiwan=count(taiwan2),
  )
compare<-group1%>%
  full_join(group2)

#5
female<-full.table%>%
  filter(Gender=="female")%>%
  select(Product,Age,UnitPrice,Quantity,Client,spend)%>%
  mutate(ts=UnitPrice*Client*Quantity)
  
  plot( x = female$Product, y = female$ts,
        xlab="Product",
        ylab="Total Spend of different Products",
        main="female consumer analysis",
        xlim=c(101,106),ylim=c(0,5000))
  



