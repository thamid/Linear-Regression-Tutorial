options(scipen = 10)
BFull <- read.table("C:/Users/Taha/Documents/CSC 423/Hw3/Bankingfull.txt", header = T)
library(psych)

#Basic Scatterplot
pairs(~Balance + Age + Education + Income + HomeVal + Wealth, data=BFull, main = "Simple Scatterplot Matrix")

#Scatterplot of Balance vs Age
plot(Balance~Age, data = BFull, xlab = "Age", ylab = "Balance", main = "Scatterplot Balance vs Age")
plot(Balance~Education, data = BFull, xlab = "Education", ylab = "Balance", main = "Scatterplot Balance vs Education")
plot(Balance~Income, data = BFull, xlab = "Income", ylab = "Balance", main = "Scatterplot Balance vs Income")
plot(Balance~HomeVal, data = BFull, xlab = "HomeVal", ylab = "Balance", main = "Scatterplot Balance vs HomeVal")
plot(Balance~Wealth, data = BFull, xlab = "Wealth", ylab = "Balance", main = "Scatterplot Balance vs Wealth")

# Compute correlation values
cor(BFull)

# Multiple Linear Regression (full model)
M1 <- lm(Balance ~ Age + Education + Income + HomeVal + Wealth, data=BFull)
summary(M1)

# Compute VIF using library CAR package
library(car)
vif(M1)

# Multiple Linear Regression without Income
M2 <- lm(Balance ~ Age + Education + HomeVal + Wealth, data=BFull)
summary(M2)

# Multiple Linear Regression without Wealth
M3 <- lm(Balance ~ Age + Education + HomeVal + Income, data=BFull)
summary(M3)

library(corrgram)
# Standardized residual vs. Fitted value
scatterplot(fitted(M2),rstandard(M2), main = "Predicted vs. Residuals plot", smooth=F)
abline(a=0, b=0, col='red')

#residual vs independent variables
plot(BFull$Age, rstandard(M2), main="Age vs residuals plot")
abline(a=0, b=0,col='red')
plot(BFull$Education, rstandard(M2), main="Education vs residuals plot")
abline(a=0, b=0,col='red')
plot(BFull$HomeVal, rstandard(M2), main="HomeVal vs residuals plot")
abline(a=0, b=0,col='red')
plot(BFull$Wealth, rstandard(M2), main="Wealth vs residuals plot")
abline(a=0, b=0,col='red')

#normal probability plot of residuals
qqnorm(rstandard(M2))
qqline(rstandard(M2), col = 2)

# compute influential points
influence.measures(M2)
# print out only observations that may be influential
summary(influence.measures(M2))
# plot of deleted studentized residuals vs. hat values
plot(rstudent(M2)~hatvalues(M2))

library(QuantPsyc)
# Multiple Linear Regression without Income
M2 <- lm(Balance ~ Age + Education + HomeVal + Wealth, data=BFull)
# compute standardized coefficients
lm.beta(M2)

#define prediction value
new=data.frame(Age=34, Education = 13, HomeVal = 140000, Wealth = 160000)
#compute prediction value
predict(M2, new)
#compute confidence interval 
predict(M2, new, interval ="confidence", level=0.95)



# create new data set without obs 85
NewBFull=BFull[c(1:84, 86:102),]

# compute influential points
influence.measures(M2)
# print out only observations that may be influential
summary(influence.measures(M2))


# Fit regression model on new data set without influential obs
M3 <- lm(Balance ~ Age + Education + Wealth, data=NewBFull)
summary(M3)

# compute influential points
influence.measures(M3)
# print out only observations that may be influential
summary(influence.measures(M3))