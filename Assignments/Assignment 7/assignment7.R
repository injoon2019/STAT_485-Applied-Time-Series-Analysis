rm(list=ls()) # Clear the Environment / History

set.seed(135343466) # <- fix the seed so the results are reproducible!
library(TSA)

# (a) Load in and plot the time series dataset. Does this data appear to come from a
# stationary process? Why or why not?
data(airmiles)
plot(airmiles, type='l', ylab="air miles")

# (b) Plot the first difference of the time series. What improvements do you see here?
#   Is there still something in this data that needs to be accounted for?
plot(diff(airmiles))


# (c) Create the sample ACF plot, sample PACF plot and sample EACF table for the
# first difference of the time series. Explain any conclusions you can make from
# each of these visualizations. If you find that it is difficult to make a single overall
# conclusion about the underlying model, explain why it is difficult.
acf(diff(airmiles), ylab=expression(r[k]))

pacf(diff(airmiles), ylab=expression(r[k]))

eacf(diff(airmiles))
 
# (d) Sometimes this type of data is seasonal. So, perhaps the difficulty in choosing an
# appropriate ARMA model is due to the fact that this is a subset-ARMA model,
# i.e. some of the coefficients are zero. Create the \best subset-ARMA selection plot.
# (Hint: An example of how to create this plot is shown in Video 28. You can ignore
# the Warning message that R gives you.)

#Select and evaluate the best subset ARMA models:
armasubsets.select = armasubsets(y=diff(airmiles), nar = 14, nma = 14, y.name = 'y')

#Plot the subset ARMA selection
plot(armasubsets.select)
