# R Code used to analyze & understanding estimated sample errrors & Ridge Regression
# For more details see http://www.stanford.edu/~hastie/Papers/LARS/
# September 29, 2013

# Regularization is a general term that describes methods that impose a penalty on 
# estimates that try to go to far afield. In ridge regression, the penalty is the 
# squared norm of the ?? vector. The ridge estimate of ?? is like the least squares 
# estimate, but seeks to minimize the objective function.
# The ?? is a nonnegative tuning parameter that must be chosen in some manner.
# If ?? = 0, the objective function is the same as that for least squares.

# A medium ?? yields an estimate somewhere "between" zero and the least squares
# estimate. Thus a ridge estimate is a type of shrinkage estimate

# Why ridge regression?
# If there is a great deal of multicolinearity in the explanatory variables, then
# X'X will be almost non-invertible, which means that ( X ' X)^-1 )will be very large
# I note that finding the ridge estimate is no more difficult than finding the
# least squares estimate

# Now, there are 2 questions that need to be answered & which are explored in the
# code below: 1) How good is the ridge predictor? 2) What should ?? be?
# Note that henceforth, I refer to ERR as the in sample error
# Remember that Error = Bias^2 + Inherent variance + Estimation variance

diab <- read.table(file = "c:/Users/Nate/Git/riemann/diabetes.data",header=T)

# In general, I note that The larger the ??, the smaller the H_??, which means as
# ?? increases, the bias increases but the estimation variance decreases
# In order to decide which ?? is best, we have to estimate the ERR for all (or many)
# ??'s

# We first normalize the variables, calling the results x and y:
p <- 10 # Number of predictors in the model
N <- 442 # Number of patients in the study
sigma2 <- sum(lm(Y ~.,data=diab)$resid^2)/(N-p-1)

# Sigma2 is the residual variance from the full model
y <- diab[,11]
y <- y-mean(y)
x <- diab[,1:10]
x <- sweep(x,2,apply(x,2,mean),"-")
x <- sweep(x,2,sqrt(apply(x^2,2,sum)),"/")

# One approach to determining how good a ridge predictor is, is to perform the matrix 
# manipulations directly. For this, you first have to turn x into a matrix. 
# Right now it is a data frame.Thus:

x <- as.matrix(x) # This is the normalized data

# -------------------------------------------------------------------------------
# Note that the following section assumes you have created the lambda function (see end) 

# For a given lambda, ridge regression proceeds as follows:
beta.lambda <- solve(t(x)%*%x+lambda*diag(p),t(x)%*%y)
# diag(p) is the pxp identity
h.lambda <- x%*%solve(t(x)%*%x+lambda*diag(p),t(x))
y.lambda <- x%*%beta.lambda
rss.lambda <- sum((y-y.lambda)^2) # Residual sum of squares
err.lambda <- rss.lambda/N # Error
edf.lambda <- sum(diag(h.lambda))+1 # Effective degress of freedom = trace hat matrix
pen.lambda <- 2*sigma2*(edf.lambda)/N # penalty
errhat.lambda <- err.lambda + pen.lambda # In sample error = error + penalty
# These calculations work, but are not terrifically efficient.

# Another approach that works with the linear model fitter is to
# make use of the augmented data, where here we are leaving out the 1N vector:
xx <- rbind(x,sqrt(lambda)*diag(p))
yy <- c(y,rep(0,10))
lm.lambda <- lm(yy ~ xx-1)
# The lm.lambda will have the correct estimates of the coefficients, but the other 
# output is not correct, since it is using the augmented data as well. The first
# N of the residuals are the correct residuals, hence 
(rss.lambda <- sum(lm.lambda$resid[1:N]^2)) # Note that () around the call prints
# The output of the call

# -------------------------------------------------------------------------------
# A more efficient procedure if one is going to calculate the regression for many
# ??'s is to use the singular value decomposition. Why? Because, if we follow
# the steps below, the single value decomposition needs to be calculated just once
# as do the RSS and the sigma^2 error.
N <- 442 # Number of patients
p <- 10 # Number of predictors
s <- svd(x)
w <- t(s$u)%*%y
d2 <- s$d^2
rss <- sum(y^2)-sum(w^2)
s2 <- rss/(N-p-1)

# Then to find the ERR's  for a given set of ??'s, and plot the results, we follow:
lambdas <- (0:100)/100 # Move up in units of 0.01
errhat <- NULL # Initially start this matrix empty
for(lambda in lambdas) { # Create the for loop to loop through the results
  rssl <- sum((w*lambda/(d2+lambda))^2)+rss
  edfl <- sum(d2/(d2+lambda))+1
  errhat <- c(errhat,(rssl + 2*s2*edfl)/N)
}

# Generate a plot of the resulting lambdas from the for loop above (0.01 increments)
png("c:/Users/Nate/Git/riemann/ridge_regression_lambda_0_1.png")
plot(errhat ~ lambdas,
     type= 'l',
     col = 2,
     xlab= 'Lambda',
     ylab= 'Estimated Errors',
     main = 'Plot Showing  ESS At Various ?? Values (0 to 1; 0.01 Increments)')
dev.off()

#You might want to repeat, focussing on smaller ??, e.g.,
lambdas <- (0:100)/500

# Generate a plot of the resulting lambdas from the for loop above (0.002 increments)
png("c:/Users/Nate/Git/riemann/ridge_regression_lambda_0_1_500.png")
plot(errhat ~ lambdas,
     type= 'l',
     col = 2,
     xlab= 'Lambda',
     ylab= 'Estimated Errors',
     main = 'Plot Showing  ESS At Various ?? Values (0 to 1; 0.002 Increments)')
dev.off()

# To find the best, it is easy enough to try a finer grid of values. Or you can use 
# the optimize function in R. You need to define the function that, given
# ??, yields ERR:
f <- function(lambda) {
    rssl <- sum((w*lambda/(d2+lambda))^2)+rss
    edf <- sum(d2/(d2+lambda))+1
    (rssl + 2*s2*edf)/N
  }

optimize(f,c(0,.02))
# The output gives the best ?? (at least the best it found) and the corresponding error
# Note that the $minimum = [1] 0.007378992, and $objective = [1] 3002.279

# To remind, in order to find the best lambda, we made a few modifications in order 
# to preserve some "equivariance." That is, we do not want the predictions to be 
# effected by the units of the variables. That is, it should not matter whether 
# height is in inches or centimeters, or whether temperature is in degrees Fahrenheit 
# or centigrade or Kelvin. To that end, we make the following modifications:
# 1. Normalize y so that it has zero mean (that is, subtract y from y);
# 2. Normalize the explanatory variables so that they have zero means and squared 
# norms of 1.

# For any particular lambda, the calculations of err ?? and edf(??) are easy enough 
# using a computer. To find the best ??, one can choose a range of ??'s, then 
# calculate the ERR for each one over a grid in that range. With the normalizations 
# we made above, the best ?? is most likely reasonably small, so starting with a 
# range of [0,1] is usually fine.



