rm(list=ls()) # Clear the Environment / History

set.seed(135343466) # <- fix the seed so the results are reproducible!
library(TSA)

# 1. (4 marks) The gold dataset in the TSA package gives the daily price of gold (in $ per
# troy ounce) for the 252 trading days of 2005.
data(gold)

data(hare)
hare.ar1.model = arima(sqrt(hare), order = c(1, 0, 0))
hare.ar1.model

# (a) Suppose we were to try fitting an AR(1) model to this dataset. Fit this model
# using the arima() function in R. Give the estimates of ¥õ and ¥ì.
# (Note: As we saw in Video 32, the coefficient named ¡°intercept¡± in the arima( )
#   output is actually referring to the mean ¥ì, NOT the intercept ¥è0.)
gold.ar1.model = arima(gold, order = c(1, 0, 0))
gold.ar1.model #PI = 0.9947 MU = 458.5493

# (b) Create a plot of the (standardized) residuals vs. time for this model. Interpret
# what you see in the plot.
plot(rstandard(gold.ar1.model), type='o', ylab = 'Standardized Residuals')

# (c) Create a Q-Q plot of the (standardized) residuals for this model. Interpret what
# you see in the plot.
qqnorm(rstandard(gold.ar1.model))
qqline(rstandard(gold.ar1.model))

# (d) Create the sample ACF plot of the (standardized) residuals for this model. Interpret what you see in the plot.
acf(rstandard(gold.ar1.model))


# 2. (6 marks) The units dataset in the TSA package gives the annual sales of certain large
# equipment, 1983-2005.
data(units)
# (a) Fit an MA(2) model (with a potentially non-zero constant mean) to this data
# using the arima() function in R. Give the estimates of the parameters ¥è1, ¥è2 and ¥ì.
# (IMPORTANT: The way the arima( ) function defines the MA model is by placing plus signs, instead of minus signs, 
#in front of the MA parameters. Therefore,
#   the values of the MA parameters given in this output are actually ???¥è1 and ???¥è2!)
# (Note: As we saw in Video 32, the coefficient named ¡°intercept¡± in the arima( )
#   output is actually referring to the mean ¥ì, NOT the intercept ¥è0.)
units.ma2.model = arima(units, order = c(0, 0, 2))
units.ma2.model
# (b) Using the methods practiced in Video 33, derive the equation for the forecast of
# Yt+` at any lead time `. Make sure to replace any parameters with the estimates
# you obtained in part (a).


# (c) Derive the equation for the forecast error variance for Yt+`
# , denoted by V ar(et(`)).
# Make sure to include each possible case of values that ` can take on.

# (d) Using your equation in part (b), obtain the forecast of Yt+1. Show your calculations.
# (Note: You can use R¡¯s estimates of the noise terms to help you out. The estimates
#   of e1, . . . , et can be found in the object name_ of_ your_ ma2_ model $residuals .)
units.ar2.model$residuals

# (e) Using your forecast in part (d), and the equation in part (c), calculate the the
# 95% prediction limits for Yt+1.
# (Note: You can use R¡¯s estimate of the white noise variance if you need it. It can
#   be found in the object name_ of_ your_ ma2_ model $sigma2 .)
units.ar2.model$sigma2

# (f) Create a plot of the predictions of Yt+` out to 20 time points in the future. Does
# the forecast for ` = 1 match your results above?
#   (Hint: You can directly read the values off the plot or, if you¡¯d like exact values,
#    you can extract them by adding $pred , $lpi or $upi after the plot( ) function. This will give you the forecasts, and lower and upper 95% prediction limits,
#    respectively.)
units.ar2.model$pred
units.ar2.model$lpi
units.ar2.model$upi
