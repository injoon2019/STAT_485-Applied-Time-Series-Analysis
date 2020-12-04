rm(list=ls())
library(TSA)


# -----------------------------------------------------------------------
#  1.   "hare" dataset
#
# hare: These are yearly hare abundances for the main drainage of 
#       the Hudson Bay, based on trapper questionnaires.
#
# Goal: Specify an ARIMA(p,d,q) model and estimate the model parameters:
#         (a) Method of Moments 
#         (b) Least Squares
#         (c) Maximum Likelihood
# -----------------------------------------------------------------------

data(hare)
hare

plot(hare,
     xlab='Year',
     ylab='Abundance',
     type='o') # Looks at least approximately stationary

# Check to see if the data is normally distributed
shapiro.test(hare)
qqnorm(hare)
qqline(hare)

# Apply a power-transformation
BoxCox.ar(hare) # Suggests a square-root transformation

sqrt.hare = sqrt(hare)


plot(sqrt.hare,
     xlab='Year',
     ylab='Square-root Abundance',
     type='o') # Looks at least approximately stationary


# Check to see if the data is normally distributed
shapiro.test(sqrt.hare)
qqnorm(sqrt.hare)
qqline(sqrt.hare)

# ------ Let's look at the acf, pacf

ACF.sqrt.hare = acf(sqrt.hare,
                    ci.type = "ma") # Cuts off after the 1st lag
pacf(sqrt.hare) # Cuts off after the 3rd lag (although the 3rd lag is marginally significant)

eacf(sqrt.hare,
     ar.max = 5,
     ma.max = 5)

# Possible models are
#   ARMA(1,1), or AR(2).

# ------ Proceed with an AR(2) model; see Ch 7.1 page 153


        # ------------------------
        # Method of Moment
        # ------------------------

# use the "ar" function to fit an AR(2) model to the data of the form
#  W_t - mu = phi_1*(W_{t-1} - mu) + phi_2*(W_{t-2} - mu) + e_t, 
#       where E(W_t) = mu.
AR2.MoM.model = ar(sqrt.hare,
                   order.max=2,
                   AIC=F,
                   method='yw')
AR2.MoM.model

# Estimate of mu
AR2.MoM.model$x.mean ; mean(sqrt.hare)

# Estimate sigma_e^2
sigma.e.2 = var(sqrt.hare) * (1 - ACF.sqrt.hare$acf[1:2] %*% AR2.MoM.model$ar)
sigma.e.2


      # ------------------------
      # Least Squares
      # ------------------------

# --- Method 1
# use the "ar" function to fit an AR(2) model to the data of the form
#  W_t - mu = phi_1*(W_{t-1} - mu) + phi_2*(W_{t-2} - mu) + e_t, 
#       where E(W_t) = mu.
AR2.LS.model1 = ar(sqrt.hare,
                   order.max=2,
                   AIC=F,
                   method="ols")
AR2.LS.model1 
# Note: "Intercept" is the intercept term in the model
#       "sigma^2" is the estimate for sigma_e^2


# Estimate of mu
AR2.LS.model1$x.mean

# Estimate of sigma_e^2
mean(AR2.LS.model1$resid^2, na.rm = T)

# --- Method 2
# use the "arima" function to fit an AR(2) model to the data of the form
#  W_t - mu = phi_1*(W_{t-1} - mu) + phi_2*(W_{t-2} - mu) + e_t, 
#       where E(W_t) = mu.
AR2.LS.model2 = arima(sqrt.hare,
                      order=c(2,0,0),
                      method = "CSS")

AR2.LS.model2

# Estimate of mu
AR2.LS.model2$coef[which( names(AR2.LS.model2$coef) %in% "intercept" )]

# Estimate of sigma_e^2
AR2.LS.model2$sigma2

      # ------------------------
      # Maximum Likelihood
      # ------------------------

# --- Method 1
# use the "ar" function to fit an AR(2) model to the data of the form
#  W_t - mu = phi_1*(W_{t-1} - mu) + phi_2*(W_{t-2} - mu) + e_t, 
#       where E(W_t) = mu.

AR2.MLE.model1 = ar(sqrt.hare,
                   order.max=2,
                   AIC=F,
                   method="mle")
AR2.MLE.model1
# Note: "sigma^2" is the estimate for sigma_e^2

# Estimate of mu
AR2.MLE.model1$x.mean

# Estimate of sigma_e^2
mean(AR2.MLE.model1$resid^2, na.rm = T)



# --- Method 2
# use the "arima" function to fit an AR(2) model to the data of the form
#  W_t - mu = phi_1*(W_{t-1} - mu) + phi_2*(W_{t-2} - mu) + e_t, 
#       where E(W_t) = mu.
AR2.MLE.model2 = arima(sqrt.hare,
                      order=c(2,0,0),
                      method = "ML")

AR2.MLE.model2

# Estimate of mu
AR2.MLE.model2$coef[which( names(AR2.LS.model2$coef) %in% "intercept" )]

# Estimate of sigma_e^2
AR2.MLE.model2$sigma2




# -----------------------------------------------------------------------
#  2.   "oil.price" dataset
#
# hare: Monthly spot price for crude oil, Cushing, OK 
#       (in U.S. dollars per barrel), January, 1986 - January, 2006.
#
# Goal: Specify an ARIMA(p,d,q) model and estimate the model parameters
# -----------------------------------------------------------------------

data(oil.price)

plot(oil.price, 
     ylab='Price per Barrel',
     type='l',
     main = "Monthly Price of Oil \n January 1986 - January 2006")

acf(as.vector(oil.price),
    xaxp=c(0,24,12),
    main = "Sample ACF for the Oil Price Time Series")

BoxCox.ar(as.vector(oil.price))

log.oil.price = log(oil.price)

plot(log.oil.price, 
     ylab='Log Price per Barrel',
     type='l',
     main = "Monthly Price of Oil \n January 1986 - January 2006") # Linear trend


plot(diff(log.oil.price), 
     ylab='Log-Difference Price per Barrel',
     type='l',
     main = "Monthly Difference Price of Oil \n January 1986 - January 2006") # Appears stationary

# Check to see if the data is normally distributed
shapiro.test( diff(as.vector(log.oil.price)) )
qqnorm(diff(as.vector(log.oil.price)))
qqline(diff(as.vector(log.oil.price)))
# Although it fails the shapiro-wilk test, the data (visually) appears to 
#   follow a normal distribution!


ACF.MA1 = acf(diff(as.vector(log.oil.price)),
              xaxp=c(0,24,12),
              main = "Sample ACF for the Logarithm-Difference \n of Oil Price Time Series")

# Cuts off after the 1st lag

pacf(diff(as.vector(log.oil.price))) # Cuts off after the 2nd lag (although the 2nd lag is marginally significant)

eacf(diff(as.vector(log.oil.price)))

# ------ Proceed with an MA(1) model; see Ch 7.1 page 153


      # ------------------------
      # Method of Moment
      # ------------------------

# Write a function to estimate theta via method of moments.
#   See "Chapter 7 R Commands" in the textbook, p. 443

estimate.ma1.mom = function(x){
  
  r = acf(x,plot=F)$acf[1]
  
  if (abs(r) <= 0.5){
    num = -1 + sqrt(1-4*r^2)
    den = 2*r
    val = num / den
  }
  
  if (abs(r) > 0.5){
    val = NA
  }
  
  return(val)
}


theta1.hat = estimate.ma1.mom(diff(as.vector(log.oil.price)))
theta1.hat

# Estimate sigma_e^2
sigma.e.2 = var(diff(as.vector(log.oil.price)))/ (1 + theta1.hat^2)
sigma.e.2



      # ------------------------
      # Least Squares
      # ------------------------


MA1.LS.model = arima(log.oil.price,
                      order=c(0,1,1),
                      method = "CSS")

# Note: The definition of the ARMA model used in "arima" is 
#       X[t] = a[1]X[t-1] + … + a[p]X[t-p] + e[t] + b[1]e[t-1] + … + b[q]e[t-q]
# Ie, theta = -b

MA1.LS.model

# Estimate of sigma_e^2
MA1.LS.model$sigma2


      # ------------------------
      # Maximum Likelihood
      # ------------------------

MA1.MLE.model = arima(log.oil.price,
                     order=c(0,1,1),
                     method = "ML")


MA1.MLE.model

# Estimate of sigma_e^2
MA1.MLE.model$sigma2

