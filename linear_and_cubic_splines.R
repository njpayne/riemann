# Linear and Cubic splines
# Code compiled September 17, 2013

# Regression splines provide one approach toallowing a regression model to 
# capture functions/trends in a predictor, X, while allowing *flexible* relationships
# between X and the average response as a function of the variable X, or E(Y|X).

# This code will show how simple linear splines can be coded in R, and then how 
# models can be fit and displayed in R.  The code will then show what
# cubic spline models look like, and also show how to fit such a model and view 
# the fitted curve.  Finally, the code will show what a "natural spline" is in 
# relation to a cubic splines

# Let us Suppose we have a predictor that takes the values 1:24
x <- c(1:24)

# Let ut generate a response variable that is predicted by
# the variable X, but in some non-linear fashion:
mu <- 10 + 5 * sin( x * pi / 24 )  - 2 * cos( (x-6)*4/24 )
set.seed(201131709) # Set the initial seed
eee <- rnorm(length(mu)) # Generate some random errors for the model
y <- mu + eee # Create the model

# Plot the resulting code
png("c:/Users/Nate/Git/riemann/linear_cubic_spline_base.png")
plot(x,y,
     ylab="Y Values",
     xlab="Z Values")
lines(x, mu, col="red")
title("Plot of Raw Data & Mean Curve For Spline Work")
dev.off()

# As noted, it's pretty clear that a line won't describe mu well for these data.
# An alternative is to assume a PIECEWISE LINEAR function.  
# To do this, I will first decide on some (small number) of knot locations.  These
# are places where you will allow the line to change direction and
# track a new line.  How to pick these is worth discussion, but for
# this example I will use the equally spaced values (6, 12, 18).
#
# The linear form of this model will be
# mean = b0 + b1 * X + b2 * [(X-6)^  +] + b3 * [(X-12)^ +] + b4 * [(X-18)^ +]
#
# Here we define (X-k)^+ = max( 0, X-k ) -- this simply measures the
# the distance from the point X=k "on the right side of k" and is 
# set to 0 for any value of X below the knot location, k.  

# This model allows the mean curve to be linear at any position of X, 
# while the slope is actually changed as the curve moves past
# a knot.  For example, this model implies:
# Slope of curve at X=3 equals:   b1
# Slope of curve at X=9 equals:   b1 + b2
# Slope of curve at X=15 equals:  b1 + b2 + b3
# Slope of curve at X=21 equals:  b1 + b2 + b3 + b4

# fit the model and look at this result:
# First, define some new predictors:

x6 <- ( x - 6 ) # New predictor
x6[ x6<0 ] <- 0

x12 <- ( x - 12 ) # New predictor
x12[ x12<0 ] <- 0

x18 <- ( x - 18 ) # New predictor
x18[ x18<0 ] <- 0

print(cbind(x, x6, x12, x18)) # Look at these new predictors
fit_ls <- lm(y ~ x + x6 + x12 + x18) # Fit the linear spline
summary(fit_ls) # Generate a summary of the result
fitted_mean_ls <- predict(fit_ls) # Look at the fitted model

png("c:/Users/Nate/Git/riemann/linear_spline_example.png") # Plot the data
plot(x, y, xlab = "X Values", ylab="Y values")
lines(x, mu, col="red")
lines(x, fitted_mean_ls, col="blue", lwd=2)
title("Data, True Mean Curve, and Fitted (Blue) Using Linear Spline")
dev.off()

# AS is shown on the graph, the linear spline allows a change at the position
# For these data, there is almost no change that is used at x=6
# On the other hand, there are obvious changes that are observed at x=12 and x = 18

# ----------------------------------------------------------------------------------
# Review Cubic Splines
# One alternative method to allow a flexible fit is to use the polynomial model
# A cubic model could useful in this case. Let us augment it using a spline
x.squared <- x^2 # Create the squared variable
x.cubed <- x^3 # Create the cubic variable
fit_cub <- lm( y ~ x + x.squared + x.cubed ) # Fit the model using the lm() approach
summary(fit_cub) # Print a summary of the fitted model
fitted_mean_cubic_mod <- predict(fit_cub) # Note that this is a "vector" of predictions

png("c:/Users/Nate/Git/riemann/cubic_model_example.png") # Plot the data
plot(x, y, xlab="X Values", ylab="Y Values")
lines(x, mu, col="red" )
lines(x, fitted_mean_cubic_mod, col="green", lwd=2 )
lines(x, fitted_mean_ls, col="blue", lwd=2)
title("Data, True Mean Curve, and Fitted (Blue) Using Cubic Spline")
dev.off()

# This fit seems "close" to the data, and is "smooth" meaning
# the curve is continuous, AND the derivative of the curve is
# also continuous -- so no sharp changes of direction like the
# linear spline.

# The form of the cubic model that was used is:
# mean = b0 + b1 * X + b2 * X^2 + b3 * X^3 + b4 *[ (X-6)^+ ]^3 + b5 * [ (X-12)^+ ]^3 +  
#                  b6 * [ (X-18)^+ ]^3

# This form is quite similar in spirit to the linear spline, but now we add
# additional cubic terms rather than additional linear terms.
# The resulting model permits more flexibility than the cubic
# polynomial model, and we'll see that clearly once we fit this model
x6.cubed <- x6^3
x12.cubed <- x12^3
x18.cubed <- x18^3

# Fit the model
fit_cub_spline <- lm( y ~ x + x.squared + x.cubed + x6.cubed + x12.cubed + x18.cubed )
summary(fit_cub_spline)
fitted_mean_cubic_spline <- predict(fit_cub_spline)

# Plot the new cubic regression spline
png("c:/Users/Nate/Git/riemann/cubic_spline_example.png") # Plot the data
plot(x, y, xlab="X Values", ylab="Y Values")
lines(x, mu, col="red" )
lines(x, fitted_mean_cubic_spline, col="purple", lwd=2 )
lines(x, fitted_mean_cubic_mod, col="green", lwd=2 )
lines(x, fitted_mean_ls, col="blue", lwd=2)
title("Data, True Mean Curve, And Fitted (Purple) Using Cubic Spline")
dev.off() # Turn the device off

# This fit also seems "close" to the data, and is smooth.
# However, unlike a cubic polynomial it allows a more flexible
# fit to the data.  It's not clear whether this additional 
# flexibility is really desirable since the fitted curve is
# clearly more variable than the underlying mean curve...

#----------------------------------------------------------------------
# Natural Splines
# Natural splines are another type of flexible 
# polynomial-based function that starts with a cubic
# spline, and then imposes the constraint that the function
# for the mean is to be linear (rather than cubic) beyond
# some boundary points -- usual the min and max of X.
# Writing the form of the predictors used for this is 
# not simple. I relied on a package = "splines"

library( splines )
fit_nat_spline <- lm( y ~ ns(x, knots=c(6,12,18)))
summary(fit_nat_spline)
fitted_mean_natural_spline <- predict(fit_nat_spline)

# Generata a plot of the values
png("c:/Users/Nate/Git/riemann/various_spline_example.png") # Plot the data
plot(x, y, xlab="X Values", ylab="Y Values")
lines(x, mu, col="red" )
lines(x, fitted_mean_natural_spline, col="orange", lwd=2 )
lines(x, fitted_mean_cubic_spline, col="purple", lwd=2 )
#lines(x, fitted_mean_cubic_mod, col="green", lwd=2 ) # Cubic Model
lines(x, fitted_mean_ls, col="blue", lwd=2)
title("True Mean (Red), Natural (Orange), Cubic (Purple), Linear (Blue)")
dev.off()