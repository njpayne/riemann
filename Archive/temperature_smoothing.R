# Time Series Analysis of Global Temperature Data
# October 30, 2013

# Note that the R package used for the text is astsa
# install.packages("astsa") # Install the R package
library("astsa") # Load the package from the library
ls() # List all the objects that are in the working environment

require(astsa) # Ensure that you have the package ready to be used
# You could also create a function .First <- function(){require(astsa)}
# which runs every time R opens

data(gtemp) # Load the temperature data

# Plotting global warming data
png('c:/Users/Nate/Git/riemann/temperature_smoothing_raw.png')
plot(gtemp, type="o", ylab="Global Temperature Deviations",
     main="Global Temperature Deviations: 1880 - 2009")
dev.off()

# Let's attempt multiple smoothing methods for the global temperature data

# ------------------------------------------------------------------------------------
# Method 1 - Moving average smoother
# I will try a decade long moving average smoother
ma_10 = filter(gtemp, sides=2, rep(1,10)/10) # This is a  10 period moving average

data(gtemp) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/temp_deviations_10_MA_Smoother.png')
plot(gtemp, type="o", ylab="Global Temperature Deviations",
     main="Global Temperature Deviations: 1880 - 2009")
lines(ma_10, col = "red") # Add the moving average line to the graph ...
legend("topleft", c("10-Year MA","Actual"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c("red", "black"),
       cex=0.75)
dev.off()
# We observe that there is a positive trend in the temperature data .. 

# ------------------------------------------------------------------------------------
# Method 2 - Kernel Smoothing
# "Kernel smoothing is a moving average smoother that uses a weight function,
# or kernel, to average the observations"

# "Note that the to implement this in R, use the
# ksmooth function. The wider the bandwidth, b, the smoother the result."

plot(gtemp, type="o", ylab="Global Temperature Deviations",
     main="Global Temperature Deviations: 1880 - 2009")
lines(ksmooth(time(gtemp), gtemp, "normal", bandwidth = 10), col = "green")

# ------------------------------------------------------------------------------------
# Method 3 - Smoothing Splines
# The code for the general smoothing spline estimation is provided on page 76 of the text ...

# Smoothing Splines
plot(gtemp, type="o", ylab="Global Temperature Deviations",
     main="Global Temperature Deviations: 1880 - 2009")
lines(smooth.spline(time(gtemp), gtemp, spar=1), col = "purple") # This is use to estimate the trend

# ------------------------------------------------------------------------------------
# Combining all methods ... in one graph

png('c:/Users/Nate/Git/riemann/temperature_smoothing_combined.png')
plot(gtemp, type="o", ylab="Global Temperature Deviations",
     main="Global Temperature Deviations: 1880 - 2009")
lines(ma_10, col = "red") # Add the moving average line to the graph ...
lines(ksmooth(time(gtemp), gtemp, "normal", bandwidth = 10), col = "green") # Kernel smoothing
lines(smooth.spline(time(gtemp), gtemp, spar=1), col = "blue") # Smoothing splines
legend("topleft", c("10-Year MA","Kernel Smoothing", "Smoothing Splines", "Actual"), 
       lty=c(1,1,1,1),
       lwd=c(2.5,2.5, 2.5,2.5),
       col=c("red", "green", "blue", "black"),
       cex=0.75)
dev.off()
