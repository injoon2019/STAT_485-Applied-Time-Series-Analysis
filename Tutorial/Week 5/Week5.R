rm(list=ls()) # Clear the Environment / History

set.seed(135343466) # <- fix the seed so the results are reproducible!
library(TSA)


# ---------------------------------------------------------------------
# Simulate a random walk process
#   {Y_t : t in I}, where I = {1,...,n}
#
#  Y_t = Y_{t-1} + e_t = sum_{u=1}^t e_u
#
# We will fit a linear regression line to assess  
#  if the R output is valid.
#
# This program follows the following steps:
#
# (A) Specify "n" and "sigma.e"
# (B) Simulate normal white noise {e_t : t in I}
# (C) Generate the random walk {Y_t : t in I}
# (D) Plot the time series
# (E) Fit a linear regression line: Y_t = beta0 + beta1 * t + e_t
# (F) Specify the number of simulations to conduct and 
#       initialize vectors / matrices for simulation study
# (G) Fit a linear regression line: Y_t = beta0 + beta1 * t + e_t,
#       for each simulation and store the estimates
# (H) Assess the estimates of beta0
# (I) Assess the estimates of beta1
# (J) Do "extra" stuff with E(Y_t) and Cov(Y_t, Y_{t-k})
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# - (A) Specify n and sigma.e
n = 1000
# n = 2000
sigma.e = 1
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# - (B) Simulate normal white noise {e_t : t in I}
e.t = rnorm(n, 
            mean = 0,
            sd = sigma.e)
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------  
# - (C) Generate the random walk {Y_t : t in I}
Y.t = sapply(1:n, function(t){
  sum(e.t[1:t])
})

Y.t = ts(Y.t)
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------  
# - (D) Plot the time series
plot(Y.t,
     type = "l",
     xlab = "Time",
     ylab = expression("Y"["t"]))
# --------------------------------------------------------------------- 

# ---------------------------------------------------------------------  
# - (E) Fit a linear regression line: Y_t = beta0 + beta1 * t + e_t
model.linear = lm(Y.t ~ time(Y.t))
summary(model.linear)

# - Add the regression line to the plot
abline(model.linear, col="red")

# ---------------------------------------------------------------------  
# - (F) Specify the number of simulations to conduct and 
#         initialize vectors / matrices for simulation study

m = 2500
Y.t_all = matrix(NA, 
                 nrow = m,
                 ncol = n) 

beta0.est    = rep(NA, m)
beta1.est    = rep(NA, m)
beta0.se     = rep(NA, m)
beta1.se     = rep(NA, m)
# --------------------------------------------------------------------- 

# ---------------------------------------------------------------------  
# - (G) Fit a linear regression line: Y_t = beta0 + beta1 * t + e_t,
#         for each simulation and store the estimates
for (j in 1:m){
  
  if (j %% 100 == 1) print(paste("Finished", j-1, "out of", m) )
  
  # - Simulating normal white noise
  e.t = rnorm(n, 
              mean = 0,
              sd = sigma.e)
  
  # - Generating the random walk
  Y.t = sapply(1:n, function(t){
    sum(e.t[1:t])
  })
  
  Y.t_all[j,] = Y.t
  
  Y.t = ts(Y.t)
  
  
  # - Fit a linear regression line
  model.linear = lm(Y.t ~ time(Y.t))
  
  beta0.est[j] = coef(summary(model.linear))[1,1]
  beta1.est[j] = coef(summary(model.linear))[2,1]
  
  beta0.se[j]  = coef(summary(model.linear))[1,2]
  beta1.se[j]  = coef(summary(model.linear))[2,2]
  
  if (j == m) print("Done!")
}
# ---------------------------------------------------------------------  

# ---------------------------------------------------------------------  
# - (H) Assess the estimates of beta0
hist(beta0.est,
     main = expression(paste("Histogram of ", hat(beta)[0], " Estimates")),
     xlab = expression(paste(hat(beta)[0])),
     breaks = "scott")

abline(v = 0,
       lty = 1,
       col = "red")
abline(v = mean(beta0.est),
       lty = 1,
       col = "blue")
legend("topleft",
       legend=c(paste("True mean = 0"), 
                paste("Average value", round(mean(beta0.est),3))),
       box.lty=1,
       cex = 0.75,
       lty = c(1,1),
       col = c("red", "blue"))

# Standard error of estimate
sd(beta0.est)

# Averaged standard error from regression output
mean(beta0.se)

# The discrepancy is due to the linear regression treating the
#   observations as "independent" and hence underestimating the 
#   standard error!
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------  
# - (I) Assess the estimates of beta1

# -----------------------------------------------
#                 beta1
# -----------------------------------------------

hist(beta1.est,
     main = expression(paste("Histogram of ", hat(beta)[1], " Estimates")),
     xlab = expression(paste(hat(beta)[1])),
     breaks = "scott")

abline(v = 0,
       lty = 1,
       col = "red")
abline(v = mean(beta1.est),
       lty = 1,
       col = "blue")
legend("topleft",
       legend=c(paste("True mean = 0"), 
                paste("Average value", round(mean(beta1.est),4))),
       box.lty=1,
       cex = 0.75,
       lty = c(1,1),
       col = c("red", "blue") )

# Standard error of estimate
sd(beta1.est)

# Averaged standard error from regression output
mean(beta1.se)

# The discrepancy is due to the linear regression treating the
#   observations as "independent" and hence underestimating the 
#   standard error!
# ---------------------------------------------------------------------  

# ---------------------------------------------------------------------  
# - Do "extra" stuff with E(Y_t) and Cov(Y_t, Y_{t-k})

# -----------------------------------------------
#                 Y.t
# -----------------------------------------------


# Plot the "k"th realization of {Y_t : t in I}
k = 686
plot(x = 1:n,
     y = Y.t_all[k,],
     type = "l",
     xlab = "Time",
     ylab = expression("Y"["t"]))


# ------------------------
#        E(Y.t)
# ------------------------


# - Plot the average (over each simulation) of Y.t 
#     (for t=1,...,n)
Y.t_mean = apply(Y.t_all, 2, mean)

plot(1:n,
     Y.t_mean,
     type = "l",
     xlab = "Time",
     ylab = expression(paste(bar(Y)["t"]))) 
# Since E(Y.t) = 0 - this result is to be expected

# ------------------------
#        Var(Y.t)
# ------------------------

# Take the variance of the "t"th value (t = 1,...,n) across the simulations
# var(Y.t_all[,1]) 
# var(Y.t_all[,5])
# var(Y.t_all[,100])

# Recall that var(Y.t) = t * sigma.e^2. What do you notice from above?

Y.t_var = apply(Y.t_all, 2, var)
plot(x = 1:n, 
     y = Y.t_var, 
     type = "l",
     xlab = "Time",
     ylab = expression(paste("Var(Y"["t"],")")),
     main = expression(paste("Simulated Var(Y"["t"],")")))

points(x = 1:n,
       y = 1:n*sigma.e^2, # t * sigma.e^2 is the true variance
       type = "l",
       col = "red") 

legend("topleft",
       legend=c(expression(paste("True variance = t", sigma["e"]^2)), 
                paste("Simulated variance")),
       box.lty=1,
       lty = c(1,1),
       col = c("red","black"),
       cex = 0.75)

# ------------------------
#     Cov(Y.t, Y.t_k)
# ------------------------

Covariance = cov(Y.t_all)

# What does "Covariance" actually compute?
#   let's look at the "t.val"th and "t_k.val"th values for each of the simulation,
#   and compute the covariance between them!

t.val = 5 
t_k.val = 1 

a = Y.t_all[,t_k.val]
b = Y.t_all[,t.val]

cov(a,b) # compute the covariance between "a" and "b"
Covariance[t.val, t_k.val]

# Recall that the cov(Y_t, Y_{t-k}) = (t-k) sigma.e^2, for k >= 0

# Check a few other values
Covariance[1,500] ; min(1,500) * sigma.e^2
Covariance[600,233] ; min(233,600) * sigma.e^2
Covariance[333,456] ; min(333,456) * sigma.e^2


# ------------------------
#     Corr(Y.t, Y.t_k)
# ------------------------

Correlation = cor(Y.t_all)


# What does "Correlation" actually compute?
#   let's look at the "t.val"th and "t_k.val"th values for each of the simulation,
#   and compute the correlation between them!

t.val = 89
t_k.val = 5

a = Y.t_all[,t_k.val]
b = Y.t_all[,t.val]

cor(a,b) # compute the covariance between "a" and "b"
Correlation[t.val, t_k.val]
ifelse( t_k.val <= t.val, sqrt(t_k.val / t.val), sqrt(t.val / t_k.val))

# Recall that the Corr(Y_t, Y_{t-l}) = sqrt( (t-k)/t ), for k >= 0

# Check a few other values
Correlation[164,345] ; sqrt(165/345)
Correlation[600,233] ; sqrt(233/600)
Correlation[333,456] ; sqrt(333/456)
# ---------------------------------------------------------------------  


