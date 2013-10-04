# Random Walk With Drift ... Time Series Regression
# Generate a random walk with drift of length n=100
# October 1, 2013

png("c:/Users/Nate/Git/riemann/random_walk_regression.png")
par(mfcol=c(3,2))
for(i in 1:6){ # Start the for loop
  x = ts(cumsum(rnorm(100,0.01,1))) # Data
  reg = lm(x~0+time(x), na.action=NULL) # Regression
  plot(x) # plot the data
  lines(0.01*time(x), col="red", lty="dashed") # plot the mean
  abline(reg, col="blue") # plot the regression line
}
dev.off()