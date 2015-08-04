# Doing more work with moving averages
# October 1, 2013

# Note that I am generating observations from the autoregression
# x_t = -0.9x_t-2 + w_t 

# Generate 150 random normal observations with mean = 0, variance = 1
w = rnorm(150,0,1)
x = filter(w, filter = c(0, -0.9), method="recursive")[-(1:50)] #AR
x2 = 2*cos(2*pi*(1:100)/4) # Sinusoid function
x3 = x2 + rnorm(100, 0, 1) # Sinusoid + Noise function
v = filter(x, rep(1,4)/4) # moving average for the autoregressive function
v2 = filter(x2, rep(1,4)/4) # moving average for the sinusoid function
v3 = filter(x3, rep(1,4)/4) #moving average for the sinusoid & noise function

# Generate the plot showing all of the various graphs for comparison purpose
png("c:/Users/Nate/Git/riemann/evaluating_moving_averages_sin.png")
par(mfrow=c(3,1))
plot.ts(x, main="Autoregression")
lines(v, lty="dashed")
plot.ts(x2, main="Sinusoid Autoregressive Moving Average")
lines(v2, lty="dashed")
plot.ts(x3, main="Sinusoid + Noise Autoregressive Moving Average")
lines(v3, lty="dashed")
dev.off()