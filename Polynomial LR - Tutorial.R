options(scipen = 10)
Energy<- read.table("C:/Users/Taha/Documents/CSC 423/Hw5/energytemp.txt", header = T)

#create variables
energy=Energy[,1]
temp=Energy[,2]
temp2=temp^2 # quadractic term
temp3=temp^3

#Scatterplot of Energy vs Tempd
plot(energy~temp, xlab = "energy", ylab = "temp", main = "Scatterplot Energy vs Temp")
abline(lm(energy~temp), col="red") # regression line (y~x)

#fit a cubic regression model  
M1=lm(energy~temp+temp2+temp3, data=Energy)
summary(M1)

#Diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(M1)

#standardized residuals vs fitted values plot
plot( fitted(M1), rstandard(M1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#standardized residuals vs independent variables
plot(Energy$temp, rstandard(M1), main="temp vs residuals plot")
abline(a=0, b=0,col='red')


#normal probability plot of residuals
qqnorm(rstandard(M1))
qqline(rstandard(M1), col = 2)
layout(matrix(c(1),1,1)) # reset

#compute prediction for average energy consumption for difference in temperature = 10
new=data.frame(temp=c(10), temp2=c(100), temp3=c(1000))
predict(M1, new, interval="prediction")


