# yi = 6, 4, 8, 4, 4, 6, 4, 5, 4, 4, 3, 6, 5, 5, 9, 10, 4, 7
# wi = 1, -1, 3, -1, -1, 1, -1, 0, -1, -1, -2, 1, 0, 0, 4, 5, -1, 2

# Calculate the complete sequence of values for each of these following processes:
# moving average model v_t = 0.5 w_{t-1} + 05 w_t
# random walk r_t = r_{t-1} + w_t
# autoregressive time series X_t = 0.9 X_{t-1} + w_t where we assumed x_0 = 0

# Generate a white noise process using binomial coin flips with 10 trials
w = c(1, -1, 3, -1, -1, 1, -1, 0, -1, -1, -2, 1, 0, 0, 4, 5, -1, 2)
y = seq(1:18) # If necessary, generate a sequence of corresponding y values

v = filter(w, sides = 1, rep(1/2,2)) # Create a moving average
a = filter(w, filter=c(-.9), method="recursive") # Create an autoregressive model
r = cumsum(w) # Create a random walk model with no drift

# Plot the time series
png("c:/Users/Nate/Git/riemann/randomwalk_whitenoise_autoregress.png")
plot.ts(w,
        y=NULL, 
        main="Time Series Plot of Random Binomial Processes (Question 1)", 
        axes = TRUE,
        ylab = "White Noise Processes",
        xlab = "Time",
        ylim = c(-5, 10))
lines(v, lty = "dashed", col = c("red")) # Plot the moving average process
lines(a, lty = "dashed", col = c("blue")) # Plot the autoregressive model
lines(r, lty = "dashed", col = c("purple")) # Plot the random walk model
legend("topleft", # Place the legendin the top left
       c("Original Data", "Moving Average", "Autoregressive", "Random Walk"), 
       lwd=c(2.5, 2.5, 2.5, 2.5),
       lty=c(1, 1, 1, 1),
       col=c("black", "red", "blue", "purple"),
       cex=0.75) # This is the standard size for the legend
dev.off()