options(scipen=10)

mydata=read.table("C:/Users/Taha/Documents/CSC 423/Hw2/HouseSales.txt", header = T)

#create dummy variables 
dhm=(mydata$Type=="T")*1 # SF-Single Family = 0 , T-Condominuim = 1
drg=(mydata$Region=="S")*1 # M-Midwest = 0, S-South = 1

mydata$dhm = dhm
mydata$drg = drg

#histogram of selling price
hist(mydata$Price, xlab="Selling Price", main = "Histogram")
x = mydata$Price
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit,mean=mean(x), sd=sd(x))

#frequency table for ASG factors
table(mydata$dhm)
table(mydata$drg)

#correlation
cor(mydata[c(3,4,5,6)])

#  Scatterplot Matrix
pairs(~(Price)+Cost+dhm+drg, data=mydata, main="Simple Scatterplot Matrix")
plot((Price)~Cost, data = mydata, xlab = "Cost", ylab = "Price", main = "Scatterplot")

# Box Plot
boxplot(Price~dhm, data=mydata, main = "Home Type")
boxplot(Price~drg, data=mydata, main = "Region Type")


# Multiple Linear Regression for claims
# the excluded level is used as reference level
fit <- lm(Price ~ Cost + dhm + drg, data=mydata)
summary(fit) # show results

fit1 <- lm(Price ~ Cost + dhm, data=mydata)
summary(fit1) # show results

fit2 <- lm(log(Price) ~ Cost + dhm, data=mydata)
summary(fit2) # show results

fit3 <- lm(log(Price) ~ Cost + dhm + drg, data=mydata)
summary(fit3) # show results

library(car)
library(corrgram)
#residuals vs fitted values plot
scatterplot(rstandard(fit1)~fitted(fit1)|mydata$dhm, main="Predicted vs residuals plot", smooth=F)
abline(a=0, b=0, col='red')


#residuals vs independent variables
plot(mydata$Cost, rstandard(fit1), main="Cost vs residuals plot")
abline(a=0, b=0,col='red')
plot(mydata$dhm, rstandard(fit1), main="Home vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit1))
qqline(rstandard(fit1), col = 2)

library(car)
library(corrgram)
#residuals vs fitted values plot
scatterplot(rstandard(fit2)~fitted(fit2)|mydata$dhm, main="Predicted vs residuals plot", smooth=F)
abline(a=0, b=0, col='red')


#residuals vs independent variables
plot(mydata$Cost, rstandard(fit2), main="Cost vs residuals plot")
abline(a=0, b=0,col='red')
plot(mydata$dhm, rstandard(fit2), main="Home vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit2))
qqline(rstandard(fit2), col = 2)