rm(list=ls()) # Clear the Environment / History

library(TSA)

# ---------------------------------------------------------------------
# electricity: Monthly U.S. electricity generation 
#               (in millions of kilowatt hours) of all types: coal, 
#               natural gas, nuclear, petroleum, and wind, 
#               from 01/1973 - 12/2005.
#   Goal: Examine the data and assess if a power transformation is 
#         required
# ---------------------------------------------------------------------

data(electricity)
electricity

plot(electricity,
     main = "Monthly U.S. Electricity Generation",
     ylab = "Electricity (in millions of kilowatt hours)")
abline( coef(lm(electricity ~ time(electricity))),
        lty = 2,
        col = "red")

summary(lm(electricity~time(electricity)))

# This plot shows us that both the mean and variance are increasing over time.
#     The data is clearly not stationary!

# Since the mean increases linearly over time, consider taking a difference 
#     Use the "diff" function to compute Y_t - Y_{t-1}

plot(diff(electricity),
     main = "Difference of Monthly U.S. Electricity Generation",
     ylab = "Difference (in millions of kilowatt hours)")

abline( coef( lm(diff(electricity) ~ time(diff(electricity))) ),
        lty = 2,
        col = "red")

summary(lm(diff(electricity)~time(diff(electricity))))

# We see that the variance is still increasing over time.

# Try a Box-Cox power transformation!

BoxCox.ar(electricity)

# We see that "0" lies in the confidence interval, 
#   this power transformation requires us to work with "log(electricity)"

log.electricity = log(electricity)

plot(log.electricity,
     main = "Monthly Log U.S. Electricity Generation",
     ylab = "Log-Electricity (in millions of kilowatt hours)")
abline( coef(lm(log.electricity ~ time(log.electricity))),
        lty = 2,
        col = "red")

summary(lm(log.electricity ~ time(log.electricity)))

# We see that the variance is now constant over time, but the mean is increasing.
#     Therefore, consider taking a difference!

plot(diff(log.electricity),
     main = "Difference of Monthly Log U.S. Electricity Generation",
     ylab = "Difference (in millions of kilowatt hours)")
abline( coef(lm(diff(log.electricity) ~ time(diff(log.electricity)))),
        lty = 2,
        col = "red")

summary(lm(diff(log.electricity) ~ time(diff(log.electricity))))

# The mean and variance are constant over time!

# What if we apply "BoxCox.ar" to diff(log.electricity)?

BoxCox.ar(diff(log.electricity)+0.25) # <- all of the values have to be positive

# Don't need to do anything!

# Let's check the acf:
acf(diff(log.electricity))

# Let's perform the runs test
runs(diff(log.electricity))

# Let's check to see if it's normal
shapiro.test(diff(log.electricity))

qqnorm(diff(log.electricity))
qqline(diff(log.electricity))

# Data doesn't appear to be normally distributed!

# ------------------------------------------------------------------------------------
# Exercise 5.14:
#
# larain: Annual precipitation (in inches) in Los Angeles, 1878-1992
#
# A quantile-quantile normal plot of these data shows that this data is not
#   normally distributed.
# (a) Use software to produce a plot similar to Exhibit 5.11, on page 102, 
#       and determine the “best” value of λ for a power transformation of the data.
# (b) Display a quantile-quantile plot of the transformed data. Are they more normal?
# (c) Produce a time series plot of the transformed values.
# (d) Use the transformed values to display a plot of Yt versus Yt − 1 as in 
#       Exhibit 1.2, on page 2. Should we expect the transformation to change the 
#       dependence or lack of dependence in the series?
# -----------------------------------------------------------------------------------

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

shapiro.test(larain)

qqnorm(larain)
qqline(larain)


# ----------------------------------------------------------------------------------
# (a) Use software to produce a plot similar to Exhibit 5.11, on page 102, 
#       and determine the “best” value of λ for a power transformation of the data.
# ----------------------------------------------------------------------------------

BoxCox.ar(larain)

# Proceed with a square-root transformation

# ----------------------------------------------------------------------------------
# (b) Display a quantile-quantile plot of the transformed data. Are they more normal?
# ----------------------------------------------------------------------------------

qqnorm(sqrt(larain))
qqline(sqrt(larain))

# Appears to be normal

shapiro.test(sqrt(larain))


# ----------------------------------------------------------------------------------
# (c) Produce a time series plot of the transformed values.
# ----------------------------------------------------------------------------------


plot(sqrt(larain),
     main = "Square-Root of LA Annual Rainfall",
     ylab="Inches",
     xlab="Year",
     type="o")

# Appears to be like white noise.


# ----------------------------------------------------------------------------------
# (d) Use the transformed values to display a plot of Yt versus Yt − 1 as in 
#       Exhibit 1.2, on page 2. Should we expect the transformation to change the 
#       dependence or lack of dependence in the series?
# ----------------------------------------------------------------------------------

# Recall from Tutorial 1:
plot(x=zlag((larain)),
     y=larain,
     main = "Rainfall in Successive Years",
     xlab="Previous Year",
     ylab="Inches")

# There doesn't appear to be any patterns in the (original) data.

plot(x=zlag(sqrt(larain)),
     y=sqrt(larain),
     main = "Square-Root of Rainfall in Successive Years",
     xlab="Previous Year",
     ylab="Inches")

# Doesn't appear to be any apparent patterns within the data.

# Since the original pattern doesn't show any dependencies, 
#   we would expect the transformed data to not show any dependencies either.
#   Transforming the data shouldn't induce any form of autocorrelation!
