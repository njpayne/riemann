# The model below "pretends to or "fakes" a linear regression and bootstraps the confidence interval on the slope.

set.seed(15723) # Set the seeed

# Generate the required data
x <- 0:100 # Generate a series from 0 to 100
y <- 2*x + rnorm(101, 0, 10) # Generate the corresponding y values

plot(x, y, main=expression("y ~ 2*x + rnorm(101, 0, 10)"),
     xlab="Artificial Explanatory Variables",
     ylab="Artificial Response Variables") # Plot the resulting data

mod1 <- lm(y ~ x) # Fit the model
summary(mod1) # Review the model
par(mfrow=c(2,2)) # Put the following plots into a 2 x 2 layout
plot(mod1) # Review the plots of the data
ryHat <- fitted(mod1) # Generate a vector of fitted values
# This vector is 101 rows x 1 column

errors <- resid(mod1) # Grab the residuals from the model

# Make the bootstrapping function
boot <- function(n = 10000){
  b1 <- numeric(n)
  b1[1] <- coef(mod1)[2]
  
  for(i in 2:n){
    residBoot <- sample(errors, replace=F)
    yBoot <- yHat + residBoot
    modBoot <- lm(yBoot ~ x)
    b1[i] <- coef(modBoot)[2]
  }
  
  return(b1)
}

# Run the bootstrapping function
system.time( bootB1 <- boot() )
mean(bootB1)