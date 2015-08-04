# Investigating Bootstrap
# September 21, 20113

# Looking at the geyser data from the MASS package
# This classic dataset contains the time between 299 eruptions of the Old Faithful 
# geyser in Yellowstone, and the length of the subsequent eruptions; 
# these variables are called waiting and duration. We'll look at the linear 
# regression of waiting on duration. We'll re-sample (duration, waiting) pairs, and 
# would like confidence intervals for the regression coefficients

# ---------------------------------------------------------------------
# Parametric Boostrap
# We attempt a linear regression of waiting against duration
# We would like to get confidence intervals for the regression coefficients

library(MASS) # Use the MASS library
data(geyser) # Attached the geyser data
geyser.lm <- lm(waiting ~ duration, data = geyser) # Create a simple linear model
summary(geyser.lm) # Generate a summary of the data

# Define the resample function
resample <- function(x) {
  sample(x,size=length(x),replace=TRUE)
}

# The first step in the parametric bootstrap is to build the simulator
# This just means sampling rows from the data frame
resample.geyser <- function() { # Create the resample simulator function
  sample.rows <- resample(1:nrow(geyser))
  return(geyser[sample.rows,])
}

# Next we define the estimator
est.waiting.on.duration <- function(data) {
  fit <- lm(waiting ~ duration, data=data)
  return(coefficients(fit))
}

# Put these together to get a bootstrapped confidence interval for the results
geyser.lm.cis <- function(B,alpha) {
  tboot <- replicate(B,est.waiting.on.duration(resample.geyser()))
  low.quantiles <- apply(tboot,1,quantile,probs=alpha/2)
  high.quantiles <- apply(tboot,1,quantile,probs=1-alpha/2)
  low.cis <- 2*coefficients(geyser.lm) - high.quantiles
  high.cis <- 2*coefficients(geyser.lm) - low.quantiles
  cis <- rbind(low.cis,high.cis)
  return(cis)
}

# Look at the results, using 1000 boostrap samples
signif(geyser.lm.cis(B=1000,alpha=0.05),3)

# ---------------------------------------------------------------------
# Let us look at a non-parametric example where we resample residuals
library(MASS) # Use the MASS library
data(oecdpanel) # Attached the OECD data
oecd.lm <- lm(growth ~ initgdp + popgro + inv, data=oecdpanel)

# Resample residuals from the linear model for the OECD data
resample.residuals.oecd <- function() {
  # Resampling residuals leaves the independent variables alone, so copy them
  new.frame <- oecdpanel
  # Take the old fitted values, and perturb them by resampling the residuals
  new.growths <- fitted(oecd.lm) + resample(residuals(oecd.lm))
  # Make these the new values of the response
  new.frame$growth <- new.growths
  # We're done
  return(new.frame)
}

# Wrapper for estimating the OECD linear model on a data frame
# Returns: Vector of linear regression coefficients
oecd.estimator <- function(data) {
  fit <- lm(growth~initgdp + popgro + inv, data=data)
  return(coefficients(fit))
}

# Confidence intervals by resampling residuals
# Output: array of upper and lower confidence limits for each coefficient
oecd.lm.cis <- function(B,alpha) {
  tboot <- replicate(B,oecd.estimator(resample.residuals.oecd()))
  low.quantiles <- apply(tboot,1,quantile,probs=alpha/2)
  high.quantiles <- apply(tboot,1,quantile,probs=1-alpha/2)
  low.cis <- 2*coefficients(oecd.lm) - high.quantiles
  high.cis <- 2*coefficients(oecd.lm) - low.quantiles
  cis <- rbind(low.cis,high.cis)
  return(cis)
}

# Deliver the confidence intervals
signif(oecd.lm.cis(1e4,0.05),3)

# ---------------------------------------------------------------------
# Understanding where the bootstrap fails
# Nonparametric bootstrapping does badly on things where changing a single
# data point can drastically change the result, like extremes of distributions
# In this example below, it can be shown that resampling to get confidence 
# intervals for the maximum of a uniform distribution fails

# Calculate actual coverage probability of what looks like a 95% CI
# Presume we know X~Unif(0,theta), and are trying to estimate theta
# In reality, theta is fixed at 1
# The MLE is max(x)
# Draw 1000 bootstrap replicates by resampling x, and take the max on each
# Find the quantiles of these re-estimates and correspond 95% CI
# Check if the CI covers 1 (the true value of theta)

# Inputs: None
# Calls: resample
# Outputs: TRUE if the CI covers 1, FALSE otherwise
is.covered <- function() {
  x <- runif(100)
  max.boot <- replicate(1e3,max(resample(x)))
  # all() takes a vector of Boolean quantities and returns TRUE if all are TRUE
  # The any() function similarly returns TRUE if any of its arguments are TRUE
  all(1 >= 2*max(x) - quantile(max.boot,0.975), 
      1 <= 2*max(x) - quantile(max.boot,0.025))
}

sum(replicate(1000,is.covered())) # Sum up the total number positives
# On my first run I achieved 906, and on the second run I achieved 880
# This is less than the true coverage probability which should be around 950