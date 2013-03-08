movies <- read.csv("C:\\movies.txt",sep="\t")
print(colnames(movies))

#Q1: Fit a linear regression model by least squares where the Rotten Tomatoes score is the outcome and the box office gross is the only covariate. What is the regression coefficient for the slope and it's interpretation?

#Fit a linear regression model by least squares where the Rotten Tomatoes score is the outcome and the box office gross is the only covariate. What is the regression coefficient for the slope and it's interpretation?
lm1 <- lm(movies$score ~ movies$box.office)
print(summary(lm1))

#A1: The regression coefficient is 0.09676. The interpretation is that an increase of one million dollars in box office gross is associated with a 0.09676 increase in Rotten Tomatoes Score.

# ===================

#Q2: Fit a linear regression model by least squares where the Rotten Tomatoes score is the outcome and the box office gross is the only covariate. What is the 90% confidence interval for the intercept term and what can you deduce from the 90% confidence interval?

print(confint(lm1,level=.90))
#5 %       95 %
#  (Intercept)       47.52438287 52.6267892
#movies$box.office  0.06761911  0.1258989
#The 90% confidence interval for the intercept is (47.52, 52.63). If we repeated this study 100 times, we would expect our calculated interval to cover the true value on average 90% of the time.
#A2: The 90% confidence interval for the intercept is (47.52, 52.63). If we repeated this study 100 times, we would expect our calculated interval to cover the true value on average 90% of the time.

# ===================

#Q3: it a linear regression model by least squares where the Rotten Tomatoes score is the outcome and box office gross and running time are the covariates. What is the value for the regression coefficient for running time? How is it interpreted?
lm3 <- lm(movies$score ~ movies$box.office + movies$running.time)
print(summary(lm3))

#A3: The coefficient is 0.12752. That means that if two movies have the same box office gross, an increase of one minute in running time is associated with an average increase of 0.12752 in score.

# ===================
#Q4: Fit a linear regression model by least squares where the Rotten Tomatoes score is
# the outcome and box office gross and running time are the covariates. 
# Is running time a confounder for the relationship between Rotten Tomatoes score and box office gross? Why or why not?
  
print(anova(lm3))
stop()

#A4v1: Yes running time is a confounder. It is correlated with the Rotten Tomatoes score.
# FAIL!
#A4v2: Yes running time is a confounder. It is correlated with the box office gross.
# FAIL! 
#A4v3: Yes running time is a confounder. It is correlated both with the Rotten Tomatoes score and the box office gross.

# ===================
# Q5
#Make a plot of the movie running times versus movie score. Do you see any outliers? If you do, remove those data points and refit the same regression (Rotten Tomatoes score is the outcome and box office gross and running time are the covariates). What do you observe?
par( mfcol = c( 2, 1 ) )
plot(movies$running.time,movies$score)

print(movies$running.time[movies$running.time > 190])

movies[movies$running.time > 190,"running.time"] <- c(NA,NA)

print(movies$running.time[movies$running.time > 190])

plot(movies$running.time,movies$score)

lm5 <- lm(movies$score ~ movies$box.office + movies$running.time)
print(summary(lm5))
# A5: Yes there are two outliers. After removing them and refitting the regression line, 
# the running time coefficient has a larger magnitude and is more statistically significant.

# ===================
#Q6: Fit a linear regression model by least squares where the Rotten Tomatoes score is the outcome and running time and box office gross are covariates. What is the P-value for running time and how is it interpreted?
movies <- read.csv("C:\\movies.txt",sep="\t")
lm6 <- lm(movies$score ~ movies$box.office + movies$running.time)
print(summary(lm6))



#A6v1: The P-value is 0.0187. It is the probability there is no relationship 
# between Rotten Tomatoes score and running time for a fixed box office gross.


#A6v2: The P-value is 0.0187. It is the probability of observing a t-statistic as big as, 
# or larger than, the one we saw, if there was no relationship between Rotten Tomatoes score and
# running time for a fixed box office gross.


# ===================
#Q7: Fit a linear model by least squares where Rotten Tomatoes score is the outcome and the covariates are movie rating, running time, and an interaction between running time and rating are the covariates. What is the coefficient for the interaction between running time and the indicator/dummy variable for PG rating?
movies <- read.csv("C:\\movies.txt",sep="\t")
lm7 <- lm(movies$score ~ movies$rating +
                         movies$running.time +
                         movies$rating*movies$running.time)
print(summary(lm7))
#A7: The coefficient is -0.6901.

# ===================
#Q8: Fit a linear model by least squares where Rotten Tomatoes score is the outcome and the covariates are
# movie rating, running time, and an interaction between running time and rating are the covariates. 
# What is the estimated average change in score for a PG movie for a one minute increase in running time?
movies <- read.csv("C:\\movies.txt",sep="\t")
lm8 <- lm(movies$score ~ movies$running.time +
                         movies$rating +
                         movies$rating*movies$running.time)
print(summary(lm8))
#A8v1: 0.690
# FAIL!
stop()
#A8v2: 0.4951 (movies$running.time 1.1852 + movies$running.time:movies$ratingPG -0.690)

# ===================
# Q9: Fit a linear model where the outcome is the number of breaks and the covariate is tension. What is a 95% confidence interval for the average difference in number of breaks between medium and high tension?

data(warpbreaks)
print(colnames(warpbreaks))

print(table(warpbreaks$tension))

lm9 <- lm(warpbreaks$breaks ~    
          relevel(warpbreaks$tension,ref="H"))
print(summary(lm9))
print(confint(lm9,level=.95))

#A9: (-3.23, 12.67)

# ===================
#Q10: Based on this graph is there a significant association between organic food sales and autism rates? Would you believe this association could be used to reduce autism rates? Why or why not?

#A10: There is a statistically significant association. We may be skeptical this association could be used to reduce autism rates, since there are many possible explanations for the association that do not involve a direct relationship between organic foods and autism.
