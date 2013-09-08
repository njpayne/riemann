# Gaussian White Noise
# September 8, 2013

# In Gaussian white noise, the noise is composed of independent and identically
# distributed random variables with mean 0 and variance sigma squared. Note
# That all possible isolations are present with equal strength

# Note that the function filter() is used below
# Filter() applies linear filtering to a univariate 
# time series or to each series separately of a multivariate time series. 
# For further information, please see the following:
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/filter.html

# Generate a collection of 500 random normal variables with mean 0 and variance 1
w = rnorm(500,0,1) # 500 N(0,1) random variables
v = filter(w, sides=2, rep(1/3,3)) # moving average
png('c:/Users/Nate/Git/riemann/gaussian_white_noise.png')
par(mfrow=c(2,1))
plot.ts(w, main="white noise") # This generates a plot of gaussian white noise
plot.ts(v, main="moving average") # This generates a three period moving average
dev.off()

# ---------------------------------------------------------------------------
# Let us instead consider the white noise as an input and calculate the output
# using a second order equation. For this model, we could use the following code
w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
png('c:/Users/Nate/Git/riemann/autoregression_white_noise.png')
par(mfrow=c(1,1))
plot.ts(x, main="autoregression")
# This will generate an autoregressive series generated from the gaussian model above
dev.off()

# Note that arima.sim could also be used ....