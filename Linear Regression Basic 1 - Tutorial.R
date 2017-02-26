options(scipen = 10)
Banking <- read.table("C:/Users/Taha/Documents/CSC 423/Hw1/Banking.txt", header = T)
library(psych)

#Descriptive Statistics
describe(Banking)
summary(Banking$Balance)
fivenum(Banking$Balance)
describe(Banking$Balance)


#Basic Scatterplot
pairs(~Balance + Age + Education + Income, data=Banking, main = "Simple Scatterplot Matrix")

#Scatterplot of Balance vs Age
plot(Balance~Age, data = Banking, xlab = "Age", ylab = "Balance", main = "Scatterplot Balance vs Age")
plot(Balance~Education, data = Banking, xlab = "Education", ylab = "Balance", main = "Scatterplot Balance vs Education")
plot(Balance~Income, data = Banking, xlab = "Income", ylab = "Balance", main = "Scatterplot Balance vs Income")


# Compute correlation values
cor(Banking)
cor(Banking$Balance, Banking$Age)
cor(Banking$Balance, Banking$Income)
cor(Banking$Balance, Banking$Education)


# Multiple Linear Regression
fit <- lm(Balance ~ Age + Education + Income, data=Banking)
summary(fit)
# Multiple Linear Regression without Education
fit1 <- lm(Balance ~ Age + Income, data=Banking)
summary(fit1)

library(car)
library(corrgram)
#residual vs independent variables
plot(Banking$Age, rstandard(fit1), main="Age vs residuals plot")
abline(a=0, b=0,col='red')
plot(Banking$Income, rstandard(fit1), main="Income vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(fit1))
qqline(rstandard(fit1), col = 2)



