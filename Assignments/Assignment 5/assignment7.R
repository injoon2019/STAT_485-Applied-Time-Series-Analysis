rm(list=ls()) # Clear the Environment / History

set.seed(135343466) # <- fix the seed so the results are reproducible!
library(TSA)
# (a) Read in this dataset using the function data() in the TSA package. Create a plot
# of the dataset.
data(winnebago)
plot(winnebago, type='l', ylab="sales-monthly")

# (b) Suppose we wish to fit a power transformation to this dataset. Find a value of
# the parameter lambda that approximatley maximizes the log-likelihood (i.e., the value
# of lambda that best matches" the data).
# (Note: You may obtain an approximate value just by looking at the Box-Cox plot
# (it doesn't have to be exactly optimal), or if you wish you can extract the exact
# optimal value using the code in Video 21.)
lambda.estimation = BoxCox.ar(winnebago)
lambda.estimation$lambda[which.max(lambda.estimation$loglike)]
# lambda: -0.1

#(c) Write out the equation for the power transformation, for this value of lambda
# ( (Y_t)^(-0.1) - 1) / (-0.1)

# (d) Transform the data using the power transformation you wrote in part (c). Plot the
# resulting transformed dataset. Make sure you label your axes in an informative
# way.
transformed_data = (winnebago**(-0.1) - 1 ) / (-0.1)
plot(log(transformed), type='l', ylab="sales-monthly")
