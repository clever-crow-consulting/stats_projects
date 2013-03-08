#Question 1
#Which of the following (pick one) is not a step in building a prediction model?
#Picking features YY
#Obtaining the right data  YY
#Selecting features with the test set. N
#*Applying to the validation data set one time Y

#Q1v1
#Applying to the validation data set one time  Wrong!!!
#Selecting features with the test set. N

#Explanation: you don't use the TEST set for feature selection.


#==========================
#Question 2
#If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? 
# If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 
# Is K large or small in leave one out cross validation?

#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to one.
#*The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.
#The bias is smaller and the variance is smaller. Under leave one out cross validation K is equal to the sample size.
#The bias is smaller and the variance is bigger. Under leave one out cross validation K is equal to the sample size,

# Q2v1
#The bias is larger and the variance is smaller. 
# Under leave one out cross validation K is equal to the sample size.


#==========================
#Question 3
#Load the South Africa Heart Disease Data and create training and test sets with the following code:
#install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

#Then fit a logistic regression model with Coronary Heart Disease (chd) as the outcome and 
# age at onset, 
# current alcohol consumption, 
# obesity levels, 
# cumulative tobacco, 
# type-A behavior, and 
# low density lipoprotein cholesterol as predictors. 
# Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:

missClass = function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}

lm1 <- glm(chd ~ age + 
             alcohol + 
             obesity + 
             tobacco + 
             typea + 
             ldl, 
           data=trainSA,
           family="binomial",
           )
#print(summary(lm1))

#fitted <- predict(lm1,testSA)
train_fitted <- predict(lm1, newdata = trainSA, 
                           type = 'response')

fitted <- predict(lm1, newdata = testSA, 
                  type = 'response')

print(missClass(trainSA$chd,train_fitted))
print(missClass(testSA$chd,fitted))

 
#  

#What is the misclassification rate on the training set? What is the misclassification rate on the test set?
#Training set misclassification: 0.2597 
#Test set misclassification: 0.2944

#Training set misclassification: 0.2597 
#Test set misclassification: 0.3117

#*Training set misclassification: 0.2727 
#*Test set misclassification: 0.3117*

#Training set misclassification: 0.3117 
#Test set misclassification: 0.2727

#Q3v1:
#0.2727; 0.3117

#==========================
#Question 4
#Load the olive oil data using the commands:
#  
#install.packages("pgmm")
library(pgmm)
library(tree)
data(olive)
olive = olive[,-1]
#These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
# Fit a classification tree where Area is the outcome variable. 
# Then predict the value of area for the following data frame using the tree command with all defaults
#

tree1 <- tree(factor(Area) ~ .,data=olive)
#plot(tree1)
#text(tree1)

newdata <- as.data.frame(t(colMeans(olive)))
#print(newdata)

#
#What is the resulting prediction? Is the resulting prediction strange? Why or why not?
#0.005291005 0 0.994709 0 0 0 0 0 0. The result is strange because Area is a numeric variable and we should get the average within each leaf.
#0.005291005 0 0.994709 0 0 0 0 0 0. There is no reason why the result is strange.
#4.59965. There is no reason why the result is strange.
#* 2.875. It is strange because Region should be a qualitative variable - but tree is reporting the average value of Region as a numeric variable in the leaf predicted for newdata.

#Q4v1
#Region should be qualitative



#==========================
#Question 5
#Load the olive oil data using the commands:
#  
#  library(pgmm)
#data(olive)
#olive = olive[,-1]
#Suppose that I fit and prune a tree to get the following diagram. What area would I predict for a new value of:
#  
newData = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, Linolenic = 32, Arachidic=60,Eicosenoic=6)

#
#Area 3*  Assuming True is on the right side
#Area 8
#Area 9
#Area 2

#Q5v1 
print(predict(tree1,newData))

# Q5v1
# Area 3 
# Wrong!!!

# Q5v2
# Area 8
      