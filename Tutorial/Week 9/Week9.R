rm(list=ls()) # Clear the Environment / History

library(TSA)


# ---------------------------------------------------------------------
#   1.   "larain" dataset
#
# larain: Annual precipitation (in inches) in Los Angeles, 1878-1992
#
# Goal: Specify an ARIMA(p,d,q) model
# ---------------------------------------------------------------------

data(larain)
larain

# Let Y_{t-1877} denote the value of precipitation (in inches) in Los Angeles 
#                 in year t,
#
#   We observe {Y_1, Y_2, ... Y_115}.



plot(larain,
     main = "LA Annual Rainfall",
     ylab='Inches',
     xlab='Year',
     type='o')


# From Exercise 5.14, we know that the data doesn't appear to be normal

shapiro.test(larain)

qqnorm(larain)
qqline(larain)

# Look to see if a power transformation helps!

BoxCox.ar(larain)

# Proceed with a square-root transformation
sqrt.larain = sqrt(larain)

mean(sqrt.larain) ; sd(sqrt.larain)

qqnorm(sqrt(larain))
qqline(sqrt(larain))

# Appears to be normal

shapiro.test(sqrt(larain))


plot(sqrt(larain),
     main = "Square-Root of LA Annual Rainfall",
     ylab="Inches",
     xlab="Year",
     type="o")

# Appears to be like white noise.

# ------ Let's look at the acf, pacf, and eacf

acf(sqrt.larain,
    ci.type = "ma") # appears to be white noise
pacf(sqrt.larain) # appears to be white noise

eacf(sqrt.larain) 
# top left corner of the table shows that if we fix p=0, q should also be 0.
      

# Propose an ARIMA(0,0,0) model.
#  Ie, view the square-root of annual rainfall as independent normal random variables
#       with mean 3.76 and standard deviation 0.87.

# -----------------------------------------------------------------------
#  2.   "wages" dataset
#
# wages: Average hourly wages in the apparel industry, from 07/1981 - 06/1987
#   Goal: Estimate the mean function (linear trend)
#
# Goal: Specify an ARIMA(p,d,q) model
# -----------------------------------------------------------------------


data(wages)
wages

# Plot the time series
plot(wages,
     type='o')

# Process is not stationary!
#   Since the trend is linear, we take a difference

diff.wages = diff(wages)

plot(diff.wages,
     type='o')

# Appears to be stationary!

shapiro.test(diff.wages)

qqnorm(diff.wages)
qqline(diff.wages)

# Will a transformation help with normality?

BoxCox.ar(diff.wages + 0.1)

# proceed to compute the sample ACF, PACF, and EACF

acf(diff.wages,
    ci.type = "ma") 
# Cuts off after lag 1, but this could be white noise since it is barely significant

pacf(diff.wages) 
# Looks like it cuts off after lag 1, however it is barely significant

eacf(diff.wages) 
# Based on the PACF plot, if we fix p=0 or p=1, this suggests q=1.

# We therefore propose an ARIMA(1,1,0), 
#                         ARIMA(0,1,1), 
#                      or ARIMA(1,1,1) model for this dataset
