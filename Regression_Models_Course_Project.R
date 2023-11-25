#Summary
#Load packages
library(ggplot2)
library(GGally)
library(car)

#First, plot pair plot to see the relationship between all variables
x <- mtcars
ggpairs(x)


#And, exclude one of two corr over 0.8(strong correlation)
#So, we need to exclude following variables : disp, cyl

#And from this, Let's test first model!
model1 <- lm(mpg ~ am+hp+drat+wt+qsec+vs+gear+carb, x)
summary(model1)

#only one variable among 9 is siginificant - surely problem!!!
#Presumably Because, we include too many variables, so they decrease the accuracy of estimation.
#We need to exclude a few more variables.
#Which variable will we exclude?
#variable that has the strongest correlation with other variables.
#we can see it through vif

vif(model1)
#Let's exclude the largest of vif : wt

#And make second model excluding wt variable
model2 <- lm(mpg ~ am+hp+drat+qsec+vs+gear+carb, x)
summary(model2)
#still only one variable is significant - excluding an additional variable with large vif : gear
vif(model2)

model3 <- lm(mpg ~ am+hp+drat+qsec+vs+carb, x)
summary(model3)

#repeat it since all variables are significant.
#final model is it.
model4 <- lm(mpg ~ am+vs+carb, x)
#Here, each coefficients is tested with t test.

#Let's anova to check one more each variables are significant.
#anova is method that compare variance that an variable can explain to variance that can't.
model41 <- lm(mpg ~ am, x)
model42 <- lm(mpg ~ am+vs, x)

anova(model41, model42, model4)
#All coefficient are significant so, It seems better that we use this model4.

summary(model4)
#Let's see our final model again.
#Because all coefficients are significant, our model's coefficients are non-zero, and we can say 4 thing
#from our conclusion of 4 coefficients.

#First, all regressor are zero('automatic'/'V-shaped Engine'/'0 carburetors), the expected value of mpg is
#19.5174 Miles/gallon
#Second, When our am variable(Transmisson) goes automatic to manual holding other variables constant, 
#the expected change
#in mpg value is increase 6.7980 Miles/gallon
#Third, When our vs variable(Engine) goes 'V-shaped' to 'straight' holding other variables constant, 
#the expected change in mpg value is increase 4.1957 Miles/gallon.
#Finally, One more carburetor holding other variables constant, 
#the expected change in mpg value is decrease -1.4308 Miles/gallon.

#And our model's R Squared value is '', which means that our regression model is explaining
#significant proportion of the Total variance of observed data.


#Our coefficients's confidence interval are following.
confint(model4)



#So, we know that 'am' and 'vs' variable have positive relationship with mpg, and 'carb' variable has
# negative relationship with mpg


#Resudual plot
plot(model4, 1)
#It seems that the residuals have mean about 0 and don't have specific pattern.
#It meets the assumption of the linear model

#Residual qqplot
plot(model4, 2)
#It seems that out residuals are almost normal.

#Residual vs Leverage plot
plot(model4, 5)
#the largest leverage point has residual near zero. So, that data point conform to regression line.

#Although there are datapoints that have large influence on regression model, but they are not
#data points that has error when they were collected, so we leave them in dataset we use.





#remain
#Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?
#detail why the question(s) is (are) not answerable?

