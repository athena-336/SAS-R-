#1
install.packages("jiebaR")
library("jiebaR")
install.packages("tmcn")
library(tmcn)
library(tidyverse)
library(tidyr)
library(readr)

#推薦的
data1=data[which(data$`Recommended IND`==1),]
library(tm)
## Make a vector source and a corpus
x=Corpus(VectorSource(data1$`Review Text`))
##Clean text
x=tm_map(x, tolower) #convert to lower case換成小寫
x<-tm_map(x, content_transformer(tolower))
x=tm_map(x, removePunctuation) #remove punctuation標點符號

x=tm_map(x, removeWords, stopwords("english"))
x=tm_map(x, stemDocument)

x_tdm <- TermDocumentMatrix(x)
inspect(x_tdm)
review_m <- as.matrix(x_tdm)
freq_df <- rowSums(review_m)
freq_df <- sort(freq_df, decreasing = T)
freq_df[1:10]
barplot(freq_df[1:20], col = "yellow", las = 2)

freq_df <- data.frame(word = names(freq_df),
                      num = freq_df)
library(wordcloud2)
wordcloud2(freq_df,size=0.5)

#語意分析 正向負面
install.packages("tidytext")
library(tidytext)
get_sentiments("bing")
bing_word_counts <- freq_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

bing_word_counts

table(bing_word_counts$sentiment)
bing_word_counts %>% 
  filter(sentiment == "positive") %>% 
  select(word,n)%>% 
  wordcloud2()

#推薦者為留言較為正面族群 

#不推薦
data2 = data[which(data$`Recommended IND`==0),]
x2=Corpus(VectorSource(data2$`Review Text`))

x2=tm_map(x2, tolower)
x2<-tm_map(x2, content_transformer(tolower))

x2=tm_map(x2, removePunctuation)

x2=tm_map(x2, removeWords, stopwords("english"))
x2=tm_map(x2, stemDocument)

x2_tdm <- TermDocumentMatrix(x2)
inspect(x2_tdm)

review_m2 <- as.matrix(x2_tdm)

freq_df2 <- rowSums(review_m2)

freq_df2 <- sort(freq_df2, decreasing = T)

freq_df2[1:10]
barplot(freq_df2[1:20], col = "blue", las = 2)

freq_df2 <- data.frame(word = names(freq_df2),
                       num = freq_df2)
wordcloud2(freq_df2,size=0.5)


get_sentiments("bing")
bing_word_counts2 <- freq_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

bing_word_counts

table(bing_word_counts2$sentiment)
bing_word_counts2 %>% 
  filter(sentiment == "negative") %>% 
  select(word,n)%>% 
  wordcloud2()

#出現負面字詞abrupt bad err

#2
library(rvest)
library(magrittr)
library(httr)
library(jsonlite)
library(tidyverse)

options(stringsAsFactors = FALSE)#保留str型態 不拆開
options(encoding = "UTF-8")#設定編碼
dcardurl <- 'https://www.dcard.tw/_api/forums/'#設定URL
board<-'relationship'#看板設定
mainurl <- paste0(dcardurl,board,'/posts?popular=false')
resdata <- fromJSON(content(GET(mainurl), "text"))
head(resdata[,c(1,2)])
n <- 1800#抓1800篇文章
page <- (1800/30)-1#每幾筆抓一項
end <- resdata$id[length(resdata$id)]
end
for(i in 1:page){
  url <- paste0(mainurl,"&before=",end)
  print(url)
  tmpres <- fromJSON(content(GET(url), "text"))
  end <- tmpres$id[length(tmpres$id)]
  resdata <- bind_rows(resdata[,c(1:12)],tmpres[,c(1:12)])
}
rm(tmpres)
head(resdata)
count <-table(cc[resdata[,2]])#

newd = data.frame(count)#

head(newd[order(newd$Freq,decreasing = TRUE),],20)
newdd = newd[order(newd$Freq,decreasing = TRUE),]
wordcloud2(newdd)

word <- cc[resdata[,2]]
newd = data.frame(table(word))

newd %>%
  filter(!str_detect(word, "[a-zA-Z0-9]+")) %>%  #去掉english and number
  filter(nchar(as.character(word)) > 1) %>% #一個字的去掉
  filter( Freq > 2) ->temp  #可留下頻率>某數字

wordcloud2(temp,size=0.4)
