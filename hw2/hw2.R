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
#�����p��]�w0��11
##1(b)

f <- function(n,�c){
  summ <- 1:n
  ans1=sum(�c-X(a,e)/1+(�c-X(a,e))^2)*(-2)
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

#���ݨC��numeric��variables��y(sales amount)���������Y
attach(houseprice) 
plot(Beds , Sale_amount , xlab =" beds" , ylab = "house price")
fit1 <- lm (Sale_amount ~ Beds)
summary(fit1)
print(fit1)
#beds��price���v�T�����j

attach(houseprice) 
plot(Baths , Sale_amount , xlab =" baths" , ylab = "house price")
fit2 <- lm (Sale_amount ~ Baths)
summary(fit2)
print(fit2)
#baths��price�S�Ӥj�v�T

attach(houseprice) 
plot(Sqft_home , Sale_amount , xlab =" sqft home" , ylab = "house price")
fit3 <- lm (Sale_amount ~ Sqft_home)
summary(fit3)
print(fit3)
#sqft home��price���@�Ǽv�T�O

attach(houseprice) 
plot(Sqft_lot , Sale_amount , xlab =" sqft lot" , ylab = "house price")
fit4 <- lm (Sale_amount ~ Sqft_lot)
summary(fit4)
print(fit4)
#�t���� �i���Ҽ{

tidy(fit1)
tidy(fit2)
tidy(fit3)
tidy(fit4)
#�C�Ӫ�p value�Ҥp,�ҥH�C��numeric��variables��house price������


plot(Beds , Sale_amount , xlab =" beds" , ylab = "house price")
curve (coef(fit1)[1] + coef(fit1)[2]*x, add=TRUE,col="green") #or abline
abline(fit1)
#�ˬdcoefficient line

predict(fit1 , newdata = houseprice, interval="predict", level= 0.95) %>% 
  plot
#�w���Ыλ���



