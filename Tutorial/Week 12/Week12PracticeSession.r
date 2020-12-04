library(TSA)

# Suppose we have the following small dataset of size n=5:
Y_vec <- c(5, 4, 3, 7, 1)

##############################################################

# Fit an AR(1) model to this dataset, "by hand", i.e. you can use R for the
# calculations but we won't use the ar() function directly.
# Note: Allow for the possibility of a non-zero constant mean.

# Estimate phi1:
rks <- acf(Y_vec)$acf
rks <- as.numeric(rks)
phi_hat <- rks[1] # r_1
phi_hat # -0.6

# Estimate mu:
mu_hat <- mean(Y_vec) # y_bar
mu_hat # 4

# Estimate gamma_0:
s2 <- var(Y_vec)

# Estimate sigma2_e:
sigma2_e_hat <- (1 - phi_hat*phi_hat) * s2 # (1−phi1*r1)*s2
sigma2_e_hat # 3.2

# NOTE: Alternatively, if wanted to use R to fit the model we could type:
myfit <- ar(Y_vec, method='yw', order.max=1, aic=FALSE)
phi_hat <- myfit$ar

##############################################################

# Write out what this estimate model looks like:
# (Y_t - 4) = -0.6 * (Y_{t-1} - 4) + e_t,
# where e_t is iid with mean 0 and variance 3.2

# Generate a realization from this estimated model, to see what it looks like.
set.seed(111111)
generate_detrended_model <- arima.sim(n=100, model=list(ar=c(phi_hat)), sd=sqrt(sigma2_e_hat))
generate_our_model <- generate_detrended_model + mu_hat
plot(generate_our_model, ylab='Estimated Process')

# Add our process onto this plot, to see if they match well.
# Note: It will be very hard to tell if they "match", because our sample size
#       is so small.
lines(Y_vec, col='red')