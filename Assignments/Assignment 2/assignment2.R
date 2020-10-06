# Title: STAT 485 Assignment 2
# Author: Injun Son
# Date: October 4, 2020

#Q1. The dataset gold gives a time series of the daily price of gold (in $ per troy ounce) for the 252 trading days of the year 2005.
#(a) Read in this dataset using the function data() in the TSA package.
library(TSA)
data(gold)

#(b)Plot the dataset. Remember to label the x- and y-axes. Briefly describe what you see.
plot(gold, ylab="price of gold", xlab="Time", type="o")

#The graph shows that the price of daily fold has been raised, even though it has oscillated

#(c) Fit a linear time trend to this dataset. What are the estimates of the intercept and the slope?
model = lm(gold ~ time(gold))
summary(model)
#Intercept: 406.3537 Slope: 0.3054 

#(d) Add the fitted line to the plot using the function abline(). Show the plot.
plot(gold, type="o", ylab="price of gold", xlab = "Time")
abline(model)

#(e) Using the estimates of the intercept and the slope, calculate the estimated mean price of gold at day 100.
# y = 0.3054x + 406.3537 (x=100)
# 30.54 +  406.3537 = 436.8937

# (f) Plot the (studentized) residuals of the model against time. Describe what you see in the plot.
plot(y=rstudent(model), x=as.vector(time(gold)), xlab = "Time", ylab="Standardized Residuals", type="l")
# The graph looks like it's not independent because each value is quite close.

#(g) The residual plot is telling us that the linear fit may not be appropriate. To fit a quadratic model, use the code
#my.model <- lm(gold ~ time(gold) + I(time(gold)^2))
#Note: The I( ) part of the code is necessary, because otherwise the ^ symbol willnot do what we want it to.)
#What are the three parameter estimates?

quadratic_model = lm(gold ~ time(gold) + I(time(gold)^2))
summary(quadratic_model)

#Intercept : 4.346e+02 time(gold): -3.618e-01 I(time(gold)^2): 2.637e-03 

#(h) Plot the (studentized) residuals of the quadratic model against time. What do you see now?
plot(y=rstudent(quadratic_model), x=as.vector(time(gold)), xlab = "Time", ylab="Standardized Residuals", type="l")
#Now it looks like the pattern does not exist




#################################################
#################################################
#Q2: (2 marks) The retail dataset gives the monthly total retail sales (in billions of pounds) in the UK, from 1986 to 2007. The plot is shown below: Time


# (a) Fit the model using the code above. Show the table of parameter estimates you get as a result.
data(retail)
month. = season(retail)
month.retail = lm(retail ~ month.-1 + time(retail))
summary(month.retail)


# (b) What is the estimate of the mean trend at t = 1987:167? (Hint: First determine which month this corresponds to.)
# 1987.167 - 1986.000 = 1.167
# 1.167 / 0.083 = 14.0xx -> 14 months later from 1986 January -> 1987 March 
#y = -7249 + 3.67 * 1987.167
#  = 43.90289
