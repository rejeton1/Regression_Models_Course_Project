#package load
library(ggplot2)


#Data load
data <- mtcars

#Data exploration
?mtcars

#Variables we are interested in are mpg and am
data <- data[,c('mpg', 'am')]
data[,'am'] <- factor(data[,'am'])

#Plot graph about this data
qplot(am, mpg, data=data, color=am)

#It seems that the difference exists between the impacts 
#of two factors to mpg value.

#Let's make a linear model.
fit <- lm(mpg ~ am, data=data)
fit

#The coefficients of model say that the expected mpg value is
#7miles larger when the gear is manual than auto.
#Let's look at p value

summary(fit)

#First, the p-value of intercept is significant, intercept value
#is not zero. And also p-value is significant in coefficient of x,
#the expected difference value of mpg is not zero between two gear.


#Let's diagnose several things about our model.







#We have to add some additional variables in our model to 
#develop the most efficient model.





##Again
library(GGally)
library(car)
x <- mtcars

#plot the relationships between all variables
ggpairs(x)
inter_col <- c(1,4:11)
ggpairs(x[,inter_col])
#corr over 0.8 is excluded.

#Let's try first model
model1 <- lm(mpg ~ am+hp+drat+wt+qsec+vs+gear+carb, x)

#vif
vif(model1)

#check the p-value
summary(model1)
#only one variable among 9 is siginificant - surely problem!!!
#Because, we include too many variables, so they decrease the accuracy of estimation


