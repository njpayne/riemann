# September 19, 2013
# Created by Nathaniel Payne

# Simulate the same processes from randomwalk_whitenoise_autoregress.R using 
# the random number generator, for 300 time points.  Draw plots of the realized 
# time series (using R). Repeat for the moving average and autoregressive processes, 
# changing the parameters in some way of your choosing.  You may either simply 
# change the value of a coeffient or change the number of terms in the model.  
# Write one sentenc on what difference you see in the resulting time series.

# Previous process had data  w = c(1, -1, 3, -1, -1, 1, -1, 0, -1, -1, -2, 1, 0, 0, 4, 5, -1, 2)
# The formula for rbinom(n, size, prob) is equal to
sim = rbinom(300, 10, 0.5) #300 time points, 10 trials per time point, 50% probability

# My previous moving average model was represented as the following
# v = filter(w, sides = 1, rep(1/2,2)) # Create a moving average
# Create a 1/3(w_t-1 + w_t + w_t+1) moving average model
v = filter(sim, sides = 2, rep(1/3,3)) # Create a moving average
# In this example, w_t is replaced by an average
# of its current value and its immediate neighbors in the past and future

# Previous process has the autoregressive model
# a = filter(w, filter=c(-.9), method="recursive") # Create an autoregressive model
# Create a new x_t = x_t-1 - 0.9x_t-2 + w_t model
a = filter(sim, filter = c(1, -.9), method="recursive")
# In this model, I have added a time point to the model
# This model represents a regression or prediction of the current value xt of a 
# time series as a function of the past two values of the series

# Plot the time series
png("c:/Users/Nate/Git/riemann/binomial_simulated_timeseries.png")
plot.ts(sim,
        y=NULL, 
        main="Time Series Plot of Simulated Binomial Processes (n = 300)", 
        axes = TRUE,
        ylab = "White Noise Processes",
        xlab = "Time",
        ylim = c(-5, 20))
lines(v, lty = "dashed", col = c("red")) # Plot the moving average process
lines(a, lty = "dashed", col = c("blue")) # Plot the autoregressive model
legend("topleft", # Place the legendin the top left
       c("Simulated Data", "Moving Average", "Autoregressive"), 
       lwd=c(2.5, 2.5, 2.5),
       lty=c(1, 1, 1),
       col=c("black", "red", "blue"),
       cex=0.75) # This is the standard size for the legend
dev.off()