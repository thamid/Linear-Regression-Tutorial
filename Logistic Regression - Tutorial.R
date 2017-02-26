options(scipen = 10)
train.chn <- read.csv("C:/Users/Taha/Documents/CSC 423/Hw5/churn_train.csv", header = T)
train.chn[1,]

# create dummy variables for gender
DM=(train.chn$GENDER=="M")*1 # M-Male = 1 , F-Female = 0
train.chn$DM = DM

#drop gender variable (first column);
train.chn=train.chn[-1]

#boxplots of yvar vs predictors
#par(mfrow=c(2,2))
boxplot(AGE~CHURN, ylab="Age", data=train.chn )
boxplot(PCT_CHNG_BILL_AMT~CHURN, ylab="% Change in Bill Amt", data=train.chn)
#par(mfrow=c(1,1))

# Full logistic regression model fitted using glm() function with family=binomial
Model1<- glm(CHURN~DM+EDUCATION+LAST_PRICE_PLAN_CHNG_DAY_CNT+TOT_ACTV_SRV_CNT+AGE+PCT_CHNG_IB_SMS_CNT+PCT_CHNG_BILL_AMT+COMPLAINT, data=train.chn, family=binomial())
summary(Model1) # display results


# Run backward selection procedure for variable selection
# step() function works for both lm and glm objects
step.mod=step(Model1, direction=c("backward"), k= 2) #AIC criterion 

# Final fitted model fter backward selection using glm() function with family=binomial
FModel<- glm(CHURN~TOT_ACTV_SRV_CNT+AGE+PCT_CHNG_IB_SMS_CNT+PCT_CHNG_BILL_AMT+COMPLAINT, data=train.chn, family=binomial())
summary(FModel) # display results

# Final fitted model without BILL
FModel1<- glm(CHURN~TOT_ACTV_SRV_CNT+AGE+PCT_CHNG_IB_SMS_CNT+COMPLAINT, data=train.chn, family=binomial())
summary(FModel1) # display results

# LIKELIHOOD RATIO TEST
# likelihood ratio test to check logistic regression model goodness of fit.
# fit is the logistic regression model defined by glm() function

#library for likelihood ratio test (you also need to install zoo package)
library(lmtest) 
#likelihood ratio test
lrtest(FModel1)

#Compute predicted probability based on information in problem 1d 
NewD = data.frame(TOT_ACTV_SRV_CNT=c(4),AGE=c(43),PCT_CHNG_IB_SMS_CNT=c(1.04),COMPLAINT=c(1))
predict(FModel1,newdata=NewD,type="response", se.fit=T) #type="response" for probabilities

#Compute predicted probability based on information in problem 1d 
Newd = data.frame(TOT_ACTV_SRV_CNT=c(4),AGE=c(43),PCT_CHNG_IB_SMS_CNT=c(4),COMPLAINT=c(1))
predict(FModel1,newdata=Newd,type="response", se.fit=T) #type="response" for probabilities

#boxplot of predicted probabilities by Yvariable
# useful visualization for classification purposes
boxplot(fitted(FModel1)~CHURN, data=train.chn,
        names=c("No Switch", "Switch"))

####################################################################
#functions for classification
# classify(plist, t) computes predicted Y using a list of predicted probabilities
# plist and a threshold t, as plist[1]>t --> y[i]=1
classify = function(plist, t){
  yclass=c()
  for (prob in plist)
    if (prob < t) yclass=c(yclass, 0)
  else yclass=c(yclass, 1)
  yclass}

#compare(plist, yvar) compares two binary lists (predicted Y and actual Y), and returns the
# classification matrix
compare=function(plist, yvar){
  i=1
  tp=0
  tn=0
  fp=0
  fn=0
  for (pred in plist) {
    if (pred==yvar[i])
      if (yvar[i]==1)
        tp=tp+1
    else
      tn=tn+1
    else
      if (yvar[i]==1)
        fp=fp+1
    else
      fn=fn+1
    i=i+1}
  matrix(c(tp, fp, fn, tn), nrow=2, ncol=2, dimnames=list(c("Actual 1", "Actual 0"),c("Predict 1", "Predict 0")))
}
#compute classification metrics using classification matrix m
# proportion of correctly classified positives TP/(TP+FN) (recall)
sensitivity= function(m){
  (m[1,1])/(m[1,1]+m[1,2])
}

# proportion of correctly classified positives and negatives
#(TP+TN)/(TP+TN+FP+FN)
accuracy=function(m){ 
  (m[1,1]+m[2,2])/sum(m)} 

# proportion of correctly classified negatives
#specificity TN/(TN+FP)
specificity =function(m){ m[2,2]/(m[2,2]+m[2,1])}

# proportion of correctly classified positives
#recall TN/(TN+FP)
precision =function(m){ m[1,1]/(m[1,1]+m[2,1])}

##########################################################################

# Final fitted model without BILL
FModel1<- glm(CHURN~TOT_ACTV_SRV_CNT+AGE+PCT_CHNG_IB_SMS_CNT+COMPLAINT, data=train.chn, family=binomial())

#using functions above to compute classification metrics
# create variable Yobs = observed values of Y in training set
Yobs=train.chn$CHURN

###################################################
# the following code creates a list of thresholds and 
# computes classification metrics for each threshold
###################################################
#list of thresholds
probs=seq(0.4, 0.8, by= 0.05)
#list of predicted Y for each threshold in probs
predlist=lapply(probs, classify, plist=fitted(FModel1))
#list of classification matrices
listmat=lapply(predlist, compare, yvar=Yobs)
#list of sensitivity measures
mprecision=as.vector(lapply(listmat, precision), mode="numeric")
mrecall=as.vector(lapply(listmat, sensitivity), mode="numeric")
maccuracy=as.vector(lapply(listmat, accuracy), mode="numeric")
fmetric=2*mrecall*mprecision/(mrecall+mprecision)
cmat=cbind(probs,mprecision, mrecall, fmetric, maccuracy) 
colnames(cmat)=c("probs", "sensitivity", "precision", "f-metric","accuracy")
#summary of classification metrics by threshold values
cmat
#plot fmetric vs probability values
plot(cmat[,1], fmetric)

##############################################
## analysis suggests threshold equal to 0.45##
##############################################
# Apply classification techniques on testing set
################################################
test.chn <- read.csv("C:/Users/Taha/Documents/CSC 423/Hw5/churn_test.csv", header = T)
test.chn[1,]

# create dummy variables for gender
DM=(test.chn$GENDER=="M")*1 # M-Male = 1 , F-Female = 0
test.chn$DM = DM

#drop gender variable (first column);
test.chn=test.chn[-1]

#predicted outcomes in testing set
preds=as.vector(predict(FModel1,test.chn, type="response"))
#compute predicted outcome based on probability threshold equal to 0.45
Ypred = classify(preds, 0.45)

# define ytest= observed values of Y in test set
Ytest = test.chn$CHURN

# compares predicted oucomes with actual values in test set
m=compare(Ypred, Ytest)
#classification matrix
m
#classification metrics
sensitivity(m)
accuracy(m)
precision(m)
Fmeasure = (2*precision(m)*sensitivity(m))/(sensitivity(m)+precision(m))
