# Spline-based Regression with the Old Faithful Data Set
# Compiled September 17, 2013

oldfaithful.data <- read.table(file = "c:/Users/Nate/Git/riemann/oldfaithfuldata.txt", 
                               header=FALSE, col.names = c('obsno', 'eruption.length', 'waiting.time'))

# The X variable here is the length of time (in minutes) it takes for the geyser to erupt.
# The Y variable is the waiting time until the next eruption.
attach(oldfaithful.data) # attaching the data frame

# ----------------------------------------------------------------------------------
# Create a regression spline with more knots placed closer to denser data
regr.spline.OF <- lm(waiting.time ~ bs(eruption.length, df = NULL, 
                                       knots=c(1.9, 2.2, 2.5, 3.0, 3.6, 4.1, 4.6), 
                                       degree = 3, intercept=T))
x.values <- seq(from=min(eruption.length), 
                to=max(eruption.length), 
                length=200)
plot(eruption.length, 
     waiting.time, 
     main = "Old Faithful Eruptions Fit By Regression Spline")
lines(x.values, predict(regr.spline.OF, data.frame(eruption.length=x.values)))
# The proposed fit seems somewhat problematic here ...

# Let me finally try the smoothing-spline method
smoothspline.reg.OF <- smooth.spline(eruption.length, waiting.time)
plot(eruption.length, 
     waiting.time,
     main = "Old Faithful Eruptions Fit By Smoothing-Spline")
lines(smoothspline.reg.OF)
# The fit on both models appears to be non-optimal
# Potential approach may be the expectation maximum method (EM Algorithm)

