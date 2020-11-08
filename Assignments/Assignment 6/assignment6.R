rm(list=ls()) # Clear the Environment / History

set.seed(135343466) # <- fix the seed so the results are reproducible!
library(TSA)

# The dataset robot gives the final position (in the x-direction) of an industrial
# robot put through a series of planned exercises many times. Read in this dataset, and
# use it to answer the following questions.

data(robot)
plot(robot, type='l', ylab="final position")

# (a) Create a sample ACF plot for this dataset. Explain what you see, and any
# conclusions you might be able to make from this plot.
acf(robot, ylab=expression(r[k]))
# It looks like there is no clear cut-off and no exponential decaying trend.
# So I don't think it's an MA or AR model

# (b) Create a sample PACF plot for this dataset. Explain what you see, and any
# conclusions you might be able to make from this plot.
pacf(robot, ylab=expression(r[k]))
#There is an cut-off after q=12, but it's a too big number and no exponential decaying
#However, it looks like there is a sinusoidal trend. 
# So similar to (a), I don't think this is an MA or AR model. 

# (c) Create a sample EACF table for this dataset. Explain what you see, and any
# conclusions you might be able to make from this table.
eacf(robot)
#It looks like there is an upper left 'O' where p=1 and q=1.
#So I guess this can be an ARMA(1, 1) model. 

# (d) Based on the above results, make a conclusion about a model that may be appro-
#   priate for this dataset. Explain your reasoning.

#Because through (a) and (b), it doesn't look like MA model or AR model.
#However, (c) shows that it can be a ARMA(1,1) model. So I think this is an ARMA(1,1) model. 