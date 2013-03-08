
# ===================================
#Question 1
#Load the weaving data with the commands:
#  data(warpbreaks)
#Fit an ANOVA model where the outcome is the number of breaks. Fit an ANOVA model including the wool and tension variables as covariates. What are the dgrees of freedom for the tension variable and what is the F-statistic for tension after accounting for the variation due to wool?
#The degrees of freedom for tension is 1 and the F-statistic is 7.537.
#The degrees of freedom for tension is 2 and the F-statistic is 7.537.
#The degrees of freedom for tension is 2 and the F-statistic is 7.206.
#The degrees of freedom for tension is 1 and the F-statistic is 3.339.

data(warpbreaks)
lm1 <- lm(warpbreaks$breaks ~ .,data=warpbreaks) 
r <- anova(lm1)
print(r)

#Q1v1: 

# ===================================
#Question 2
#Suppose that the probability an event is true is 0.2. What are the log odds of that event?
#0.2500
#-1.3863*
#-1.6094
#0.8000

# http://en.wikipedia.org/wiki/Logit
print(log(.2/(1-.2)))

#Q2v1: -1.3863

# ===================================
#Question 3
#Load the horseshoe crab data using the commands:
#  library(glm2)
#data(crabs)
#Fit a Poisson regression model with the number of Satellites 
# as the outcome and the width of the female as the covariate. 
# What is the multiplicative change in the expected number of crabs
# for each additional centimeter of width?
#0.5074
#1.1782*
#-0.6785
#2.7587

#install.packages("glm2")
#library(glm2)
#data(crabs)
#lm3 <- glm(crabs$Satellites ~ crabs$Width, data=crabs, family="poisson") 
#print(lm3)
#print(exp(0.164))

#Q1v1 1.1782*


# ===================================
#Question 4
#Load the horseshoe crab data using the commands:
#  library(glm2)
#data(crabs)
#What is the expected number of Satellites for a female of width 22cm?
#0.1640
#-3.1407
#1.3556*
#26.5998

#lm4 <- glm(Satellites ~ Width, data=crabs, family="poisson")
#print(exp(predict(lm4,data.frame(Width=c(22)))))

# Q4v1:1.3556 

# ===================================
#x <- c(1,2,3)
#y <- x*2
#slm <- lm(y ~ x)
#print(slm)
#print(predict(slm,data.frame(x=c(3,4))))

# ===================================
#Question 5
#Load the school absenteeism data set and fit a linear model relating the log of the number of days absent to the other variables with the commands:
#  data(quine) 
#lm1 = lm(log(Days + 2.5) ~.,data=quine)
#Use the step() function in R to perform model selection using default parameters. What variables remain in the model after model selection?
#Eth, Sex, Age, Lrn
#Eth, Age
#Sex, Lrn
#Eth  

library(MASS)
data(quine)
print(quine)
lm5 <- lm(log(Days + 2.5) ~.,data=quine)
step(lm5)

#lm1 = lm(log(Days + 2.5) ~.,data=quine)



