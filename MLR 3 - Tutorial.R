options(scipen = 10)
col <- read.csv("C:/Users/Taha/Documents/CSC 423/Hw4/College.csv", header = T, sep =",")
#remove first column
col = col[-1]

#create dummy variable 
dpr= (col$Private=="Yes")*1 # Private = 1, Public = 0
col$dpr = dpr

#remove Private column
col = col[-1]

library(psych)
describe(col$Grad.Rate)
hist(col$Grad.Rate)

#Basic Scatterplot

plot(cbind(col[,1:5],col$Grad.Rate),  main="Simple Scatterplot Matrix")
plot(cbind(col[,6:10],col$Grad.Rate),  main="Simple Scatterplot Matrix")
plot(cbind(col[,11:15],col$Grad.Rate),  main="Simple Scatterplot Matrix")

#Scatterplot of Grad.Rate
plot(Grad.Rate~Accept.pct, data = col, xlab = "Accept.pct", ylab = "Grad.Rate", main = "Scatterplot Balance vs Accept.pct")
plot(Grad.Rate~Elite10, data = col, xlab = "Elite10", ylab = "Grad.Rate", main = "Scatterplot Balance vs Elite10")
plot(Grad.Rate~F.Undergrad, data = col, xlab = "F.Undergrad", ylab = "Grad.Rate", main = "Scatterplot Balance vs F.Undergrad")
plot(Grad.Rate~P.Undergrad, data = col, xlab = "P.Undergrad", ylab = "Grad.Rate", main = "Scatterplot Balance vs P.Undergrad")
plot(Grad.Rate~Outstate, data = col, xlab = "Outstate", ylab = "Grad.Rate", main = "Scatterplot Balance vs Outstate")
plot(Grad.Rate~Room.Board, data = col, xlab = "Room.Board", ylab = "Grad.Rate", main = "Scatterplot Balance vs Room.Board")
plot(Grad.Rate~Books, data = col, xlab = "Books", ylab = "Grad.Rate", main = "Scatterplot Balance vs Books")
plot(Grad.Rate~Personal, data = col, xlab = "Personal", ylab = "Grad.Rate", main = "Scatterplot Balance vs Personal")
plot(Grad.Rate~PhD, data = col, xlab = "PhD", ylab = "Grad.Rate", main = "Scatterplot Balance vs PhD")
plot(Grad.Rate~S.F.Ratio, data = col, xlab = "S.F.Ratio", ylab = "Grad.Rate", main = "Scatterplot Balance vs S.F.Ratio")
plot(Grad.Rate~perc.alumni, data = col, xlab = "perc.alumni", ylab = "Grad.Rate", main = "Scatterplot Balance vs perc.alumni")
plot(Grad.Rate~Expend, data = col, xlab = "Expend", ylab = "Grad.Rate", main = "Scatterplot Balance vs Expend")
plot(Grad.Rate~Terminal, data = col, xlab = "Terminal", ylab = "Grad.Rate", main = "Scatterplot Balance vs Terminal")
plot(Grad.Rate~dpr, data = col, xlab = "dpr", ylab = "Grad.Rate", main = "Scatterplot Balance vs dpr")

# Compute correlation values
cor(col)

# create boxplots of gradrate vs type of university;
boxplot(col$Grad.Rate~col$dpr,xlab = "dpr", ylab = "Grad.Rate", main = "Boxplot Balance vs dpr")
# create boxplots of gradrate vs status;
boxplot(col$Grad.Rate~col$Elite10,xlab = "Elite10", ylab = "Grad.Rate", main = "Boxplot Balance vs Elite10")

#fit full model
Model1 = lm(Grad.Rate ~ Accept.pct+Elite10+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+S.F.Ratio+perc.alumni+Expend+Terminal+dpr, data=col)
summary(Model1)

# Compute VIF using library CAR package
library(car)
vif(Model1)

#Apply model selection methods using functions in library leaps
# load library leaps
library(leaps)
#define x-variables names
xvarlist = c("Accept.pct", "Elite10", "F.Undergrad", "P.Undergrad", "Outstate", "Room.Board", "Books", "Personal", "PhD", "S.F.Ratio", "perc.alumni", "Expend", "Terminal", "dpr")
# define matrix of predictors
xvars = col[xvarlist]
# check that all the xvariables are in xvars;
head(xvars)
# define response variable 
yvar=col$Grad.Rate

# best subset model selection according to Cp statistics
leapmodels=leaps(x=xvars, y=yvar, names=xvarlist, method="Cp")
npar=leapmodels$size
# CP statistics
Cp=leapmodels$Cp
mat=data.frame(cbind(npar,Cp, leapmodels$which), row.names=NULL)
#display results in increasing order of Cp
#the best subset of variables if for cp=var_size;
head(mat[order(mat$Cp),], n=15)


#fit full model and Backward Selection
Model1 = lm(Grad.Rate ~ Accept.pct+Elite10+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books+Personal+PhD+S.F.Ratio+perc.alumni+Expend+Terminal+dpr, data=col)
step (Model1, direction = "backward")

#fit model after selection method
FModel1 = lm(Grad.Rate ~ Accept.pct+Elite10+F.Undergrad+P.Undergrad+Outstate+Room.Board+Personal+PhD+perc.alumni+Expend+dpr, data=col)
summary(FModel1)

#residual plots
#residuals vs predicted values plot
plot( fitted(FModel1), rstandard(FModel1), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')

#normal probability plot of residuals
qqnorm(rstandard(FModel1))
qqline(rstandard(FModel1), col = 2)

library(QuantPsyc) #load package
lm.beta(FModel1) # compute standardized coefficients
plot(FModel1)

#run model diagnostics....

# print out only observations that may be influential 
summary(influence.measures(FModel1))
# plot of deleted studentized residuals vs hat values
plot(hatvalues(FModel1),rstudent(FModel1)) 
#add labels to points
text(hatvalues(FModel1),rstudent(FModel1), cex=0.7, pos=2)
#in car package
# plot to evaluate influential points
# bubble size represents cook's distance
# interactive plot: click in bubbles to identify obs. number of influential point
influencePlot(FModel1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

#fit model with interaction term Elite10
IntM1 = lm(Grad.Rate ~ Accept.pct+Elite10+Outstate+perc.alumni+Expend+Elite10*Accept.pct+Elite10*Outstate+Elite10*perc.alumni+Elite10*Expend, data=col)
summary(IntM1)

#fit model with interaction term Elite10 but without alumni
IntM2 = lm(Grad.Rate ~ Accept.pct+Elite10+Outstate+perc.alumni+Expend+Elite10*Accept.pct+Elite10*Outstate+Elite10*Expend, data=col)
summary(IntM2)

library(QuantPsyc) #load package
lm.beta(IntM2) # standardized coefiicients


# Create training and testing set
# split samples (75% for training and 25% for testing)
select.col <- sample(1:nrow(col), 0.75*nrow(col))
train.col <- col[select.col,]  #Selecting 75% of the data for training purpose
test.col <- col[-select.col,]  #Selecting 25% (remaining) of the data for testing purpose

# fit model using training set
# Model 1: 
# Selected model
FinalM1 = lm(Grad.Rate ~ Accept.pct+Elite10+F.Undergrad+P.Undergrad+Outstate+Room.Board+Personal+PhD+perc.alumni+Expend+dpr, data=train.col)
summary(FinalM1)
#Create fitted values using test.col data
y_pred <- predict.glm(FinalM1, test.col)
y_obs<-test.col[,"Grad.Rate"]

# Compute mean percentage absolute error
mape_m1<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m1

#fit model with interaction term Elite10 but without alumni
FinalM2 = lm(Grad.Rate ~ Accept.pct+Elite10+Outstate+perc.alumni+Expend+Elite10*Accept.pct+Elite10*Outstate+Elite10*Expend, na.action=na.omit, data=train.col)
summary(FinalM2)

#Create fitted values using test.col data
y_pred <- predict.glm(FinalM2, test.col)
y_obs<-test.col[,"Grad.Rate"]

# Compute mean percentage absolute error
mape_m2<-mean(abs((y_obs - y_pred)/y_obs))*100
mape_m2
