rm(list=ls()) # Clear the Environment / History

set.seed(135343466) # <- fix the seed so the results are reproducible!
library(TSA)

# 1. (2 marks) The color dataset in the TSA package gives the values of a colour property
# from 35 consecutive batches in an industrial process. Suppose we decide to fit an AR(1)
# model to this dataset.
data(color)
# (a) Fit the AR(1) model to this dataset, using the Maximum Likelihood Estimation
# approach within the ar() function. Give the estimates of PI and MU.
# (Hint: We will need the argument method= "mle" in the ar( ) function.)
AR1.model.MLE = ar(color, order.max=1, AIC=F, method='mle')
AR1.model.MLE
#PI = 0.57

# Estimate of mu
AR1.model.MLE$x.mean
#mu  = 74.34

# (b) Write out the full equation(s) you could use to estimate 2
# e , using the estimates
# of PI and MU. Make sure to plug the estimates of PI and MU into the equation. You do
# not have to actually evaluate this estimate, since the dataset is somewhat large.

#  Y_t - 74.34 = 0.57 *(Y_{t-1} - 74.34) + e_t, 
#       where E(Y_t) = mu.

# 2. (3 marks)
# (a) Fit the AR(1) model to the color dataset, using the Method of Moments approach
# within the ar() function. Give the estimates of PI and MU.
# (Hint: We will need the argument method= "yw" in the ar( ) function. This
#   stands for \Yule-Walker", because the Yule-Walker equations need to be solved to
# get the MOM parameter estimates.)
AR1.model.YW = ar(color, order.max=1, AIC=F, method='yw')
AR1.model.YW
#PI = 0.53

# Estimate of mu
AR1.model.YW$x.mean
# mu = 74.89

# (b) Using equation(s) we have learned about in Video 29, obtain an estimate of the
# process variance r_0.
# (Hint: You may have to explore a bit to find a function in R that can give you the
#   sample variance of a dataset.)
gamma_0 <- var(color)
gamma_0 #37.1042

# (c) Using the above results, and equation(s) we have learned about in Video 29,
# obtain an estimate of the white noise variance 2
# e .
# (Hint: If you need some sample correlations from the dataset: For any dataset
#   mydata , the vector of rk-values is given by acf( mydata) $acf .)
mean(AR1.model.YW$resid^2, na.rm = T) # 24.41
