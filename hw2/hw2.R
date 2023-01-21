##1(a)
set.seed(2)
a <- sample(0:10,1)
e <- rnorm(20,0,2)# Generate 20 standard normal random numbers

X <-function(x1,x2){
  ans=x1+x2
  #max(ans, na.rm = TRUE)<=11
  #min(ans, na.rm = TRUE)>=0
  return(ans)
}
X(a,e)
#不知如何設定0到11
##1(b)

f <- function(n,θ){
  summ <- 1:n
  ans1=sum(θ-X(a,e)/1+(θ-X(a,e))^2)*(-2)
  return(ans1)
}

##1(c)
f(20,0.3)

##2(a)
houseprice %<>% mutate(year_type='')
houseprice[which(houseprice$Build_year <= 1899),'year_type']<-'centennial'
houseprice[which(houseprice$Build_year >= 1900 & houseprice$Build_year <= 1959),'year_type']<-'old'
houseprice[which(houseprice$Build_year >= 1960),'year_type']<-'new'
##2(b)
library(foreign)
attach(houseprice)
fit <- lm (Sale_amount ~ Beds*Baths*Sqft_home*Type*Town*University)
summary(fit)
print(fit)

#
library(broom) 

#先看每個numeric的variables跟y(sales amount)之間的關係
attach(houseprice) 
plot(Beds , Sale_amount , xlab =" beds" , ylab = "house price")
fit1 <- lm (Sale_amount ~ Beds)
summary(fit1)
print(fit1)
#beds對price有影響但不大

attach(houseprice) 
plot(Baths , Sale_amount , xlab =" baths" , ylab = "house price")
fit2 <- lm (Sale_amount ~ Baths)
summary(fit2)
print(fit2)
#baths對price沒太大影響

attach(houseprice) 
plot(Sqft_home , Sale_amount , xlab =" sqft home" , ylab = "house price")
fit3 <- lm (Sale_amount ~ Sqft_home)
summary(fit3)
print(fit3)
#sqft home對price有一些影響力

attach(houseprice) 
plot(Sqft_lot , Sale_amount , xlab =" sqft lot" , ylab = "house price")
fit4 <- lm (Sale_amount ~ Sqft_lot)
summary(fit4)
print(fit4)
#負相關 可不考慮

tidy(fit1)
tidy(fit2)
tidy(fit3)
tidy(fit4)
#每個的p value皆小,所以每個numeric的variables跟house price都相關


plot(Beds , Sale_amount , xlab =" beds" , ylab = "house price")
curve (coef(fit1)[1] + coef(fit1)[2]*x, add=TRUE,col="green") #or abline
abline(fit1)
#檢查coefficient line

predict(fit1 , newdata = houseprice, interval="predict", level= 0.95) %>% 
  plot
#預測房屋價格




