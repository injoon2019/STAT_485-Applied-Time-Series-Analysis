rm(list=ls())
library(TSA)


# -----------------------------------------------------------------------
#  1.   "hare" dataset
#
# hare: These are yearly hare abundances for the main drainage of 
#       the Hudson Bay, based on trapper questionnaires.
#
# Goal: Specify an ARIMA(p,d,q) model and estimate the model parameters
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


# use the "ar" function to fit an AR(2) model to the data of the form
#  W_t - mu = phi_1*(W_{t-1} - mu) + phi_2*(W_{t-2} - mu) + e_t, 
#       where E(W_t) = mu.
AR2.model =ar(sqrt.hare,
              order.max=2,
              AIC=F,
              method='yw')
AR2.model

# Estimate sigma_e^2
sigma.e.2 = var(sqrt.hare) * (1 - ACF.sqrt.hare$acf[1:2] %*% AR2.model$ar)
sigma.e.2




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
     main = "Monthly Price of Oil \n January 1986 - January 2006") # Linear trend


ACF.MA1 = acf(diff(as.vector(log.oil.price)),
    xaxp=c(0,24,12),
    main = "Sample ACF for the Logarithm-Difference \n of Oil Price Time Series")

# Cuts off after the 1st lag

pacf(diff(as.vector(log.oil.price))) # Cuts off after the 2nd lag (although the 2nd lag is marginally significant)

eacf(diff(as.vector(log.oil.price)))

# ------ Proceed with an MA(1) model; see Ch 7.1 page 153


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


