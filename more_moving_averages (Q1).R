# More work with moving averages
# October 1, 2013

# Consider the MA(2) model xt = 10 + wt + .5wt-1 + .3wt-2, where wt ~ iid N(0,1).  
# The coefficients are ??1= 0.5 and ??2= 0.3.  
# Because this is an MA(2), the theoretical ACF will have nonzero values only at 
# lags 1 and 2.

# Create a plot of the theoretical ACF
par(mfcol=c(1,1)) # Ensure that we only have 1 graph per page
acfma2=ARMAacf(ma=c(0.5,0.3), lag.max=10)
acfma2
lags=0:10
plot(lags, # Create a plot of the theoretical ACF
     acfma2,
     xlim=c(1,10), 
     ylab="ACF",
     type="h", 
     main = "Theoretical ACF for MA(2) with theta1 = 0.5, theta2 = 0.3")
abline (h=0)

# As nearly always is the case, sample data won't behave quite so perfectly as theory.  
# We thus simulate n = 150 sample values for the model
png("c:/Users/Nate/Git/riemann/more_moving_averages_Ma(2).png")
xc=arima.sim(n=150, list(ma=c(0.5, 0.3)))
x=xc+10
plot (x, type="b", main = "Simulated MA(2) Series")
acf(x, xlim=c(1,10), main="ACF for Simulated MA(2) Data")
dev.off()
# Note that the pattern on the graph is typical for situations where an MA(2) model 
# may be useful.  There are two statistically significant "spikes" at lags 1 and 2 
# followed by non-significant values for other lags.  Note that due to sampling error, 
# the sample ACF did not match the theoretical pattern exactly.

# ------------------------------------------------------------------------------------
# Suppose that an MA(1) model is xt = 10 + wt + .7wt-1, where wt???iidN(0,1).  
# Thus the coefficient ??1= 0.7.

# Determines the ACF and store it in an object named acfma1.
acfma1=ARMAacf(ma=c(0.7), lag.max=10) # 10 lags of ACF for MA(1) with theta1 = 0.7
# Plot the lags versus the ACF values for lags 1 to 10.  
# The ylab parameter labels the y-axis and the "main" parameter puts a title on the plot.
lags=0:10 #creates a variable named lags that ranges from 0 to 10.
plot(lags,acfma1,xlim=c(1,10), ylab="r",type="h", main = "ACF for MA(1) with theta1 = 0.7")
abline (h=0) #Adds a horizontal axis to the plot

# Simulate 150 data points from the model
png("c:/Users/Nate/Git/riemann/more_moving_averages_Ma(1).png")
xc=arima.sim(n=150, list(ma=c(0.7))) #Simulates n = 150 values from MA(1)
x=xc+10 # adds 10 to make mean = 10. Simulation defaults to mean = 0.
plot(x,type="b", main="Simulated MA(1) Data")
acf(x, xlim=c(1,10), main="ACF for Simulated MA(1) Data")
dev.off()

# ------------------------------------------------------------------------------------
# Suppose that an MA(1) model is xt = 0.5wt + 0.5wt-1, where wt???iidN(0,1).  
# Thus the coefficient ??0= 0.5 and the coefficient ??1 = 0.5.

# Here is the orginal data
# yi = 6, 4, 8, 4, 4, 6, 4, 5, 4, 4, 3, 6, 5, 5, 9, 10, 4, 7
# wi = 1, -1, 3, -1, -1, 1, -1, 0, -1, -1, -2, 1, 0, 0, 4, 5, -1, 2

# Generate a white noise process using binomial coin flips with 10 trials
w = c(1, -1, 3, -1, -1, 1, -1, 0, -1, -1, -2, 1, 0, 0, 4, 5, -1, 2)
y = seq(1:18) # If necessary, generate a sequence of corresponding y values

# The process that we are using is a moving average model v_t = 0.5 w_{t-1} + 0.5 w_t

# Note that the moving average process is simply 0.5*t-1 + 0.5*t.
# Note that I have set the first value, ma[0] = 0, since ma[-1] does not exist
# By hand, I can easily calculate the moving average process here ...
ma = c(0, 0, 1, 1, -1, 0, 0, 0.5, -0.5, -1, -1.5, -0.5, 0.5, 0, 2, 4.5, 2, 0.5)

# Generate a plot with the original data w and the moving average data ma
png("c:/Users/Nate/Git/riemann/more_moving_averages_Ma_Binomial.png")
plot.ts(y, w, # Plot the original data
        xy.label=FALSE, 
        xy.lines=TRUE,
        xlab="Time",
        ylab="White Noise Process",
        main="Example Illustrating MA Plot For Binomial Process") 
lines(ma, col=2) # Plot the new moving average lines on the data
legend("topleft", # Place the legendin the top left
       c("Original Data", "Moving Average"), 
       lwd=c(2.5, 2.5),
       lty=c(1, 1),
       col=c("black", "red"),
       cex=0.75) # This is the standard size for the legend
dev.off()

# Generate a plot of the MA process using the ACF function
png("c:/Users/Nate/Git/riemann/more_moving_averages_Ma_ACF.png")
acf(ma, main="Plot demonstrating ACF For the MA Process")
dev.off()
