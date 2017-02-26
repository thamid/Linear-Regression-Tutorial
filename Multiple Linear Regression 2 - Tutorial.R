options(scipen = 10)
Pga <- read.csv("C:/Users/Taha/Documents/CSC 423/Hw3/pgatour2006_small.csv", header = T, sep =",")
library(psych)

#Basic Scatterplot
pairs(~PrizeMoney + DrivingAccuracy + GIR + PuttingAverage + BirdieConversion	+ PuttsPerRound, data=Pga, main = "Simple Scatterplot Matrix")

#Scatterplot of Balance vs Age
plot(PrizeMoney~DrivingAccuracy, data = Pga, xlab = "DrivingAccuracy", ylab = "PrizeMoney", main = "Scatterplot PrizeMoney vs DrivingAccuracy")
plot(PrizeMoney~PuttingAverage, data = Pga, xlab = "PuttingAverage", ylab = "PrizeMoney", main = "Scatterplot PrizeMoney vs PuttingAverage")
plot(PrizeMoney~BirdieConversion, data = Pga, xlab = "BirdieConversion", ylab = "PrizeMoney", main = "Scatterplot PrizeMoney vs BirdieConversion")
plot(PrizeMoney~PuttsPerRound, data = Pga, xlab = "PuttsPerRound", ylab = "PrizeMoney", main = "Scatterplot PrizeMoney vs PuttsPerRound")
plot(PrizeMoney~GIR, data = Pga, xlab = "GIR", ylab = "PrizeMoney", main = "Scatterplot PrizeMoney vs GIR")

# Correlation 
cor(Pga[c(2,3,4,5,6,7)])

# Histogram
hist(Pga$PrizeMoney, xlab="PrizeMoney", prob=TRUE, main = "Histogram of Prize")
x = Pga$PrizeMoney
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit,mean=mean(x), sd=sd(x))
lines(xfit, yfit, col="blue",lwd=2)

#Descriptive stats
summary(Pga$PrizeMoney)

# Log transformation
ln_Prize = log(Pga$PrizeMoney)

# Histogram of ln_Prize
hist(ln_Prize, xlab="logPrizeMoney", prob=TRUE, main = "Histogram of ln_Prize")
x = ln_Prize
xfit<-seq(min(x), max(x), length=40)
yfit<-dnorm(xfit,mean=mean(x), sd=sd(x))
lines(xfit, yfit, col="blue",lwd=2)

#Descriptive stats
summary(ln_Prize)

# Multiple Linear Regression (full model)
lnPr<- lm(ln_Prize ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion  + PuttsPerRound, data=Pga)
summary(lnPr)

# Multiple Linear Regression without driving accuracy
lnPr1<- lm(ln_Prize ~ GIR + PuttingAverage + BirdieConversion  + PuttsPerRound, data=Pga)
summary(lnPr1)

# Multiple Linear Regression without driving accuracy and putting average
lnPr2<- lm(ln_Prize ~ GIR + BirdieConversion  + PuttsPerRound, data=Pga)
summary(lnPr2)

library(corrgram)
# Standardized residual vs. Fitted value
plot(fitted(lnPr2),rstandard(lnPr2), main = "Predicted vs. Residuals plot")
abline(a=0, b=0, col='red')

#residual vs independent variables
plot(Pga$GIR, rstandard(lnPr2), main="GIR vs residuals plot")
abline(a=0, b=0,col='red')
plot(Pga$BirdieConversion, rstandard(lnPr2), main="BirdieConversion vs residuals plot")
abline(a=0, b=0,col='red')
plot(Pga$PuttsPerRound, rstandard(lnPr2), main="PuttsPerRound vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(lnPr2))
qqline(rstandard(lnPr2), col = 2)

# compute influential points
influence.measures(lnPr2)
# print out only observations that may be influential
summary(influence.measures(lnPr2))
# plot of deleted studentized residuals vs. hat values
plot(rstudent(lnPr2)~hatvalues(lnPr2))
#Hat value
hatvalues(lnPr2)

exp(0.2454)
