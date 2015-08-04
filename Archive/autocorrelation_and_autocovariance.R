# Plots demonstrating the autocorrelation and autocovariance function
# for problem 1.7 from Shumway and Stoffer (2011), Time Series Analysis & Its
# Applications, with R examples

# The provided moving average is x_{t}=w_{t-1}+2_{w_{t}}+w_{t-1} 
# The task is to determine the autocovariance and autocorrelation function
# for these moving average models

autocov = c(6, 4, 1, 0, 0, 0) # Results from manual calculations of autocovariance
autocorr = c(1, 1/6, 1/3, 0, 0, 0) # Results from manual calculations of autocorrelation

# Plot the time series
png("c:/Users/Nate/Git/riemann/autocorrelation_and_autocovariance.png")
plot.ts(autocov,
        y=NULL, 
        main="Plots Comparing Autocovariance and Autocorrelation", 
        axes = TRUE,
        ylab = "Moving Average Process",
        xlab = "Time",
        ylim = c(-1, 7))
lines(autocorr, lty = "dashed", col = c("red")) # Plot the moving average process
legend("topright", # Place the legendin the top left
       c("Autocovariance", "Autocorrelation"), 
       lwd=c(2.5, 2.5),
       lty=c(1, 1),
       col=c("red", "blue"),
       cex=0.75) # This is the standard size for the legend
dev.off()