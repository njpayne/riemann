# R code created to analyze 101 simulated (X,Y) data points using spline methods
# Compiled September 17, 2013
simul101.data <- read.table(file = "c:/Users/Nate/Git/riemann/101datapairs.txt", 
                            header=FALSE, col.names = c('x', 'y'))
simul101.data <- data.frame(simul101.data)
attach(simul101.data) # Attach the simulated data frame

plot(x, y,
     xlab = "x Values",
     ylab = "y values") # First look at a scatter plot of Y against X
title("Plot For 101 Simulated Data Points")
# As can be shown on the graph, there appears to be a relationship between Y  
# Unfortunately, there does not appear to be a conventional functional relationship

# ----------------------------------------------------------------------------------
# (Cubic) Regression Splines - 4 Equally Spaced Knots

library(splines) # Load the "splines" package

# The bs() function in R produces B-splines, a computationally efficient 
# way to compute cubic regression splines

number.knots <- 4 # Specifying 4 equally spaced knots
spacings <- seq(from=min(x),to=max(x),length=number.knots+2)[2:(number.knots+1)]
regr.spline <- lm(y ~ bs(x, df = NULL, knots=spacings, degree = 3, intercept=T))

# Plot the data with the regression spline overlaid
png("c:/Users/Nate/Git/riemann/101datapairs_4_knots.png")
x.values <- seq(from=min(x), to=max(x), length=200)
plot(x, y,
     xlab = "x Values",
     ylab = "y values") # First look at a scatter plot of Y against X
title("Plot For 101 Simulated Data Points: 4 Knots Regression Spline")
lines(x.values, predict(regr.spline, data.frame(x=x.values)))
dev.off()

# ----------------------------------------------------------------------------------
# (Cubic) Regression Splines - 10 Equally Spaced Knots

number.knots <- 10 # 10 equally spaced knots
spacings <- seq(from=min(x),to=max(x),length=number.knots+2)[2:(number.knots+1)]
regr.spline <- lm(y ~ bs(x, df = NULL, knots=spacings, degree = 3, intercept=T))

# Plot the data with the regression spline overlaid
x.values <- seq(from=min(x), to=max(x), length=200)
plot(x, y); lines(x.values, predict(regr.spline, data.frame(x=x.values)))

# Note that We can specify unequally spaced knots.  For example, we could put more 
# knots in the "wiggly" region of the data (from x=0.5 to x=1.0) and 
# fewer knots in the "flat" region of the data:

regr.spline <- lm(y ~ bs(x, df = NULL, knots=c(0.25, 0.5, 0.6, 0.7, 0.8, 0.9), degree = 3, intercept=T))
x.values <- seq(from=min(x), to=max(x), length=200)
# Plot the data with the regression spline overlaid
png("c:/Users/Nate/Git/riemann/101datapairs_10_knots.png")
x.values <- seq(from=min(x), to=max(x), length=200)
plot(x, y,
     xlab = "x Values",
     ylab = "y values") # First look at a scatter plot of Y against X
title("Plot For 101 Simulated Data Points: 10 Knots Regression Spline")
lines(x.values, predict(regr.spline, data.frame(x=x.values)))
dev.off()

# The 10 knot option seems to work well for this function!!

# ----------------------------------------------------------------------------------
# The smooth.spline() function in R computes (cubic) smoothing splines
smoothspline.reg <- smooth.spline(x, y)

# Plot the data with the smoothing spline overlaid
plot(x, y); lines(smoothspline.reg)

# By default, the value of the smoothing parameter is 
# determined by (generalized) cross-validation.
# We can see the default choice of smoothing parameter using the following
smoothspline.reg$spar # The default choice is 0.5296

# We can also specify a larger smoothing parameter value
png("c:/Users/Nate/Git/riemann/101datapairs_smoo_spline_large.png")
smoothspline.reg.large <- smooth.spline(x, y, spar = 0.9)
plot(x, y,
     xlab = "x Values",
     ylab = "y values") # First look at a scatter plot of Y against X
title("Plot For 101 Sim. Data Points: Large Smoothing Parameter")
lines(smoothspline.reg.large)
dev.off()

# We can also specify a smaller smoothing parameter value
png("c:/Users/Nate/Git/riemann/101datapairs_smoo_spline_small.png")
smoothspline.reg.small <- smooth.spline(x, y, spar = 0.3)
plot(x, y,
     xlab = "x Values",
     ylab = "y values") # First look at a scatter plot of Y against X
title("Plot For 101 Sim. Data Points: Small Smoothing Parameter")
lines(smoothspline.reg.small)
dev.off()
