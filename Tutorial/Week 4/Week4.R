rm(list=ls()) # Clear the Environment / History

library(TSA)

# ---------------------------------------------------------------------
# larain: Annual precipitation (in inches) in Los Angeles, 1878-1992
#   Goal: Estimate the mean function (constant)
# ---------------------------------------------------------------------
data(larain)
larain

# Let Y_{t-1877} denote the value of precipitation (in inches) in Los Angeles 
#                 in year t,
#
#   We observe {Y_1, Y_2, ... Y_115}.

plot(larain,
     ylab='Inches',
     xlab='Year',
     type='o')

# Based on the plot, we propose the model Y_t = mu + X_t, where E(X_t) = 0.


model.const = lm(larain ~ 1 ) 
summary(model.const)

abline(model.const, col="red")

# Note:
mean(larain) ; sd(larain) / sqrt(length(larain)) 
# What do you notice?



# ----------------------------------------------------------------------------
# wages: Average hourly wages in the apparel industry, from 07/1981 - 06/1987
#   Goal: Estimate the mean function (linear trend)
# ----------------------------------------------------------------------------

data(wages)
wages

# Let Y_t denote the time average hourly wage in the apparel industry at time t
#   ****    NOTE    *****
#   {Y_t : t ∈ I} is the process, but I is not {1,2,...72}
#         I = [1981.5, 1987.417]
# This means that the formulae presented in the textbook/slides aren't "technically correct!"


plot(wages,
     type='o')

# Based on the plot, we propose the model Y_t = beta_0 + beta_1 * t + X_t, where E(X_t) = 0.

mean.linear = lm(wages~time(wages))  # time(wages) corresponds to the "time" of the observation
summary(mean.linear)

abline(mean.linear, col="red")

# Note 1:
beta1hat = sum( (wages - mean(wages)) * (time(wages) - mean(time(wages))) ) / sum( (time(wages) - mean(time(wages)) )^2 ) ; beta1hat
beta0hat = mean(wages) - beta1hat * mean(time(wages)) ; beta0hat
# What do you notice?

# Note 2:
X = data.frame(1, time(wages))
X = as.matrix(X)
solve(t(X) %*% X) %*% t(X) %*% wages
# What do you notice?


# ----------------------------------------------------------------------------
# tempdub: Monthly average temperature (in degrees Fahrenheit) recorded in
#           Dubuque, Iowa, USA from January 1964 to December 1975.
# Goal: Fit seasonal trends (seasonal mean)
# ----------------------------------------------------------------------------

data(tempdub)
tempdub

# Let Y_t denote the monthly average temperature between January 1964 to December 1975
#   ****    NOTE    *****
#   {Y_t : t ∈ I} is the process, but I is not {1,2,...144}
#         I = [1964.0, 1975.917]
# This means that the formulae presented in the textbook/slides aren't "technically correct!"


plot(tempdub,
     type='o',
     xlab = "Time",
     ylab = "Temperature",
     main = "Average Monthly Temperatures in Dubuque, Iowa")


# Based on the plot, we propose the model Y_t = mu_t + X_t, 
#                       where mu_t = mu_{t+12} and E(X_t) = 0

month. = season(tempdub) 
mean.seasonal1 = lm(tempdub~month.-1) 
summary(mean.seasonal1) # What can we say about the estimates?
# Note that there is no intercept term. 

fitted.vals1 = ts(fitted(mean.seasonal1), freq=12, start=c(1964,1)) 
lines(fitted.vals1, col="red")

mean.seasonal2 = lm(tempdub~month.)
summary(mean.seasonal2)
# Note that there is an intercept term, but the effect of January is "dropped".

fitted.vals2 = ts(fitted(mean.seasonal2), freq=12, start=c(1964,1)) 
lines(fitted.vals2, col="blue")

# How do the coefficients compare between "mean.seasonal1" and "mean.seasonal2"

data.frame(NoInterceptModel_Beta = as.numeric( coef(mean.seasonal1) ),
           InterceptModel_Alpha = as.numeric( coef(mean.seasonal2) ) )

alpha.hat = as.numeric( coef(mean.seasonal1) )  - as.numeric( coef(mean.seasonal1)[1] ) ; alpha.hat


# Computing estimates from "mean.seasonal1" by hand...
# Note: tempdub already has each year in a row, each month is a column
tempdub.mat = matrix(tempdub, nrow = 12, ncol = 12, byrow =T) # specify the data as a matrix so that we can take the mean of the columns
apply(tempdub.mat,2,mean)
# What do you notice?




# ----------------------------------------------------------------------------
# tempdub: Monthly average temperature (in degrees Fahrenheit) recorded in
#           Dubuque, Iowa, USA from January 1964 to December 1975.
# Goal: Fit seasonal trends (cosine trend model)
# ----------------------------------------------------------------------------


har. = harmonic(tempdub, 1) # creates values of the "harmonic function"
mean.cosine = lm(tempdub~har.) 
summary(mean.cosine)

plot(tempdub,
     type='o',
     xlab = "Time",
     ylab = "Temperature",
     main = "Average Monthly Temperatures in Dubuque, Iowa")

fitted.vals3 = ts(fitted(mean.cosine), freq=12, start=c(1964,1)) 
lines(fitted.vals3, col="green")


# ----------------------------------------------------------------------------
# tempdub: Monthly average temperature (in degrees Fahrenheit) recorded in
#           Dubuque, Iowa, USA from January 1964 to December 1975.
# Goal: Conduct residual diagnostics (under the model "mean.cosine")
# ----------------------------------------------------------------------------

# Recall mean.cosine
summary(mean.cosine)

# What is s? 
#         R^2? 
#         Adjusted-R^2?

# Note 1:
s = sqrt( 1/(length(tempdub)- length(coef(mean.cosine)) ) * 
  sum((tempdub - mean.cosine$fitted.values)^2) ) ; s


# Note 2:
rss = sum((tempdub - mean.cosine$fitted.values)^2)  # residual sum of squares
tss = sum((tempdub - mean(tempdub))^2)  # total sum of squares
rsq = 1 - (rss/tss) ; rsq

# **** NOTE: R reports the WRONG R^2 value for 
#         mean.seasonal1, "summary(mean.seasonal1)"
#       but right value for 
#        mean.seasonal2, "summary(mean.seasonal2)"
#       (Has to do if an intercept term is included or not!)

# Note 3:
rsq.adj = 1 - (1 - rsq) * ( length(tempdub) - 1 ) /
  (length(tempdub) - length(coef(mean.cosine))); rsq.adj



# Plot the residuals
plot(x=as.vector(time(tempdub)),
     y=tempdub - mean.cosine$fitted.values,
     type="o", 
     xlab="Time", 
     ylab="Studentized Residuals", 
     pch=as.vector(season(tempdub)))

# Plot the studentized residuals
plot(x=as.vector(time(tempdub)),
     y=rstudent(mean.cosine),
     type="o", 
     xlab="Time", 
     ylab="Studentized Residuals", 
     pch=as.vector(season(tempdub)))



# Plot the histogram of the studentized residuals
hist(rstudent(mean.cosine), 
     xlab="Studentized Residuals",
     breaks = "scott")

# What do you notice about the distribution of the residuals?

# Plot the Normal Q-Q Plot for the studentized residuals
qqnorm(rstudent(mean.cosine))
qqline(rstudent(mean.cosine))

# Conduct a Shapiro-Wilks test
shapiro.test(rstudent(mean.cosine))

# Conduct a runs test
runs(rstudent(mean.cosine))

# Plot the ACF of the studentized residuals
acf(rstudent(mean.cosine),
    main = "ACF of the Studentized Residuals")



# ------------------------------------------------------------------------------------
# Exercise 3.5:
#
# wages: Average hourly wages in the apparel industry, from 07/1981 - 06/1987
#
# (a) Display and interpret the time series plot for these data.
# (b) Use least squares to fit a linear time trend to this time series. Interpret the
#     regression output. Save the standardized residuals from the fit for further 
#     analysis.
# (c) Construct and interpret the time series plot of the standardized residuals from
#     part (b).
# (d) Use least squares to fit a quadratic time trend to the wages time series. 
#     Interpret the regression output. Save the standardized residuals from the 
#     fit for further analysis.
# (e) Construct and interpret the time series plot of the standardized residuals from
#     part (d).
# -----------------------------------------------------------------------------------


data(wages)

# ----------------------------------------------------------------------------------
# (a) Display and interpret the time series plot for these data.
#            Note that we did this already!
# ----------------------------------------------------------------------------------

plot(wages,
     type='o',
     main = "Average hourly wages ($) in the apparel industry: 07/1981 - 06/1987")


# ----------------------------------------------------------------------------------
# (b) Use least squares to fit a linear time trend to this time series. 
#     Interpret the regression output. Save the standardized residuals 
#     from the fit for further analysis.
#            Note that we did this already!
# ----------------------------------------------------------------------------------

mean.linear = lm(wages~time(wages))  # time(wages) corresponds to the "time" of the observation
summary(mean.linear)

# The average wage is expected to increase by $0.28 per year.


# ----------------------------------------------------------------------------------
# (c) Construct and interpret the time series plot of the standardized 
#     residuals from part (b).
# ----------------------------------------------------------------------------------

plot(rstudent(mean.linear),
     type='o',
     main = "Studentized Residuals of the Linear Fit")

# Values don't apperar to be a random scatter around 0.


# ----------------------------------------------------------------------------------
# (d) Use least squares to fit a quadratic time trend to the wages time series. 
#     Interpret the regression output. Save the standardized residuals from the 
#     fit for further analysis.
# ----------------------------------------------------------------------------------

time.wages.2 = (time(wages))^2
mean.quadratic = lm(wages~ time(wages) + time.wages.2)
summary(mean.quadratic)

# All estimates are highly significant

plot(wages,
     type='o',
     main = "Average hourly wages ($) in the apparel industry: 07/1981 - 06/1987")

# The increase of wage is more in the early 1980's compared to the late 1980's

points(time(wages),
       mean.quadratic$fitted.values, 
       col="blue",
       type = "l")




# ----------------------------------------------------------------------------------
# (e) Construct and interpret the time series plot of the standardized 
#     residuals from part (d).
# ----------------------------------------------------------------------------------

plot(rstudent(mean.quadratic),
     type='o',
     main = "Studentized Residuals of the Quadratic Fit")

# Values don't apperar to be a random scatter around 0.



# ------------------------------------------------------------------------------------
# Exercise 3.11:
#
# wages: Average hourly wages in the apparel industry, from 07/1981 - 06/1987
#
# (a) Consider the residuals from a least squares fit of a quadratic time trend.
# (b) Perform a runs test on the standardized residuals and interpret the results.
# (c) Calculate and interpret the sample autocorrelations for the standardized 
#     residuals.
# (d) Investigate the normality of the standardized residuals (error terms). 
#     Consider histograms and normal probability plots. Interpret the plots.
# -----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# (a) Consider the residuals from a least squares fit of a quadratic time trend.
# ----------------------------------------------------------------------------------

residuals.quadratic = rstudent(mean.quadratic)

# ----------------------------------------------------------------------------------
# (b) Perform a runs test on the standardized residuals and interpret the results.
# ----------------------------------------------------------------------------------

runs(residuals.quadratic)

# p-value < 0.05


# ----------------------------------------------------------------------------------
# (c) Calculate and interpret the sample autocorrelations for the standardized 
#     residuals.
# ----------------------------------------------------------------------------------

acf(residuals.quadratic,
    main = "ACF of the Studentized Residuals")

# The residuals appear to not be independent from each other

# ----------------------------------------------------------------------------------
# (d) Investigate the normality of the standardized residuals (error terms). 
#     Consider histograms and normal probability plots. Interpret the plots.
# ----------------------------------------------------------------------------------

hist(residuals.quadratic, 
     xlab="Studentized Residuals",
     main="Histogram of Studentized Residuals") # Distribution appears symmetric...

qqnorm(residuals.quadratic)
qqline(residuals.quadratic) # Distribution appears to be normal...

shapiro.test(residuals.quadratic)

# p-value > 0.05, fail to reject that the distribution of the residuals is Normal


# ------------------------------------------------------------------------------------
# We have shown that the residuals do not behave as white noise, so this model is
# inadequate to capture the autocorrelation within the data.
# ------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------
# Exercise 3.6:
#
# beersales: Monthly beer sales in millions of barrels, 01/1975 - 12/1990.
#
# (a) Display and interpret the plot the time series plot for these data.
# (b) Now construct a time series plot that uses separate plotting symbols for the
#     various months. Does your interpretation change from that in part (a)?
# (c) Use least squares to fit a seasonal-means trend to this time series. 
#     Interpret the regression output. Save the standardized residuals from the 
#     fit for further analysis.
# (d) Construct and interpret the time series plot of the standardized residuals from
#     part (c). Be sure to use proper plotting symbols to check on seasonality in the
#     standardized residuals.
# (e) Use least squares to fit a seasonal-means plus quadratic time trend to the beer
#     sales time series. Interpret the regression output. Save the standardized 
#     residuals from the fit for further analysis.
# (f) Construct and interpret the time series plot of the standardized residuals from
#     part (e). Again use proper plotting symbols to check for any remaining 
#     seasonality in the residuals.
# -----------------------------------------------------------------------------------

data(beersales)


# ----------------------------------------------------------------------------------
# (a) Display and interpret the plot the time series plot for these data.
# ----------------------------------------------------------------------------------

plot(beersales,
     ylab = "Beer Sales (in millions)",
     main = "Beer Sales over Time")

# Beer sales is not independent over time. Sales appear to be low in the winter season
# and highest in the summer season.


# ----------------------------------------------------------------------------------
# (b) Now construct a time series plot that uses separate plotting symbols for the
#     various months. Does your interpretation change from that in part (a)?
# ----------------------------------------------------------------------------------

plot(beersales,
     ylab = "Beer Sales (in millions)",
     main = "Beer Sales over Time")
points(x = as.vector(time(beersales)),
       y = beersales,
       pch = as.vector(season(beersales)),
       col = c("blue", "blue", "blue", 
               "green", "green", "green",
               "red", "red", "red",
               "orange", "orange", "orange"))

# As mentioned in Part (a), we can see that the sales depends on the season.
#   We see the winter/fall months are similar in sales, whereas the spring/summer
#   months are similar in sales.


# ----------------------------------------------------------------------------------
# (c) Use least squares to fit a seasonal-means trend to this time series. 
#     Interpret the regression output. Save the standardized residuals from the 
#     fit for further analysis.
# ----------------------------------------------------------------------------------

month. = season(beersales)

beer_model1 = lm(beersales ~ 0 + month.)
summary(beer_model1)

# Average beer sales is highest in the summer/spring, lowest in the fall/winter

beer_model1_stresiduals = rstudent(beer_model1)


# ----------------------------------------------------------------------------------
# (d) Construct and interpret the time series plot of the standardized residuals from
#     part (c). Be sure to use proper plotting symbols to check on seasonality in the
#     standardized residuals.
# ----------------------------------------------------------------------------------

plot(x = as.vector(time(beersales)),
     y = beer_model1_stresiduals,
     xlab = "Year",
     ylab = "Studentized Residuals",
     main = "Studentized Residuals",
     type = "l")
abline(h = 0)
points(x = as.vector(time(beersales)),
       y = beer_model1_stresiduals,
       pch = as.vector(season(beersales)),
       col = c("blue", "blue", "blue", 
               "green", "green", "green",
               "red", "red", "red",
               "orange", "orange", "orange"))

# The residuals appear to be correlated as concurrent beer sales are near each other.
# Also appears to be an increasing trend over time.
# The residuals do not behave like a white noise process.


# ----------------------------------------------------------------------------------
# (e) Use least squares to fit a seasonal-means plus quadratic time trend to the beer
#     sales time series. Interpret the regression output. Save the standardized 
#     residuals from the fit for further analysis.
# ----------------------------------------------------------------------------------

time1 = time(beersales)
time2 = time(beersales)^2

beer_model2 = lm(beersales ~ 0 + month. + time1 + time2)
summary(beer_model2)

# Linear and quadratic terms are (highly) significant

plot(beersales,
     ylab = "Beer Sales (in millions)",
     main = "Beer Sales over Time")
points(time(beersales),
       beer_model2$fitted.values,
       type = "l",
       col = "red")

beer_model2_stresiduals = rstudent(beer_model2)

# ----------------------------------------------------------------------------------
# (f) Construct and interpret the time series plot of the standardized residuals from
#     part (e). Again use proper plotting symbols to check for any remaining 
#     seasonality in the residuals.
# ----------------------------------------------------------------------------------


plot(x = as.vector(time(beersales)),
     y = beer_model2_stresiduals,
     xlab = "Year",
     ylab = "Studentized Residuals",
     main = "Studentized Residuals",
     type = "l")
abline(h = 0)
points(x = as.vector(time(beersales)),
       y = beer_model2_stresiduals,
       pch = as.vector(season(beersales)),
       col = c("blue", "blue", "blue", 
               "green", "green", "green",
               "red", "red", "red",
               "orange", "orange", "orange"))

# The seasonality pattern still exists within the data, but this is a 
# better fit than the previous model, as the time trend is no longer present.


# ------------------------------------------------------------------------------------
# Exercise 3.12:
#
# beersales: Monthly beer sales in millions of barrels, 01/1975 - 12/1990.
#
# (a) Obtain the residuals from the least squares fit of the seasonal-means plus 
#     quadratic time trend model.
# (b) Perform a runs test on the standardized residuals and interpret the results.
# (c) Calculate and interpret the sample autocorrelations for the standardized 
#     residuals.
# (d) Investigate the normality of the standardized residuals (error terms). 
#     Consider histograms and normal probability plots. Interpret the plots.
# -----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# (a) Obtain the residuals from the least squares fit of the seasonal-means plus 
#     quadratic time trend model.
# ----------------------------------------------------------------------------------

# Already done


# ----------------------------------------------------------------------------------
# (b) Perform a runs test on the standardized residuals and interpret the results.
# ----------------------------------------------------------------------------------

runs(beer_model2_stresiduals)

# Therefore, we reject the null hypothesis that the residuals are independent

# ----------------------------------------------------------------------------------
# (c) Calculate and interpret the sample autocorrelations for the standardized 
#     residuals.
# ----------------------------------------------------------------------------------

acf(beer_model2_stresiduals,
    main = "ACF of the Studentized Residuals")

# The residuals do not behave as white noise, as the correlation between observation
#   one month apart is statistically significant.

# ----------------------------------------------------------------------------------
# (d) Investigate the normality of the standardized residuals (error terms). 
#     Consider histograms and normal probability plots. Interpret the plots.
# ----------------------------------------------------------------------------------


hist(beer_model2_stresiduals, 
     xlab="Studentized Residuals",
     main="Histogram of Studentized Residuals") # Distribution appears symmetric...

qqnorm(beer_model2_stresiduals)
qqline(beer_model2_stresiduals) # Distribution appears to be normal...

shapiro.test(beer_model2_stresiduals)

# p-value > 0.05, fail to reject that the distribution of the residuals is Normal