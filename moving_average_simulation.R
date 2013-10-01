# Moving Average Simulation
# October 1, 2013

# Simulate a series of n=500 moving average observations 
# and compute the sample ACF to lag 20. 
# Compare the sample ACF you obtain to the actual ACF

# Note that we need to generate 2 extra observations due to the loss
# of the end points in making the MA simulation

png("c:/Users/Nate/Git/riemann/moving_average_simulation_502.png")
wa = rnorm(502, 0,1) # 502 Moving average observations
va = filter(wa, sides=2, rep(1,3)/3)
acf(va,20,  # Plot and print the results for part a
    main = "Series of n=500 Moving Average Observations",
    na.action=na.pass)
dev.off()

png("c:/Users/Nate/Git/riemann/moving_average_simulation_52.png")
wb = rnorm(52, 0,1) # 52 Moving average observations
vb = filter(wb, sides=2, rep(1,3)/3)
acf(wb,20,  # Plot and print the results for part a
    main = "Series of n=50 Moving Average Observations",
    na.action=na.pass)
dev.off()

# ----------------------------------------------------------------------------
# Now, plot both of the acf curves on the same output (for comparison)
png("c:/Users/Nate/Git/riemann/moving_average_simulation_500_50.png")
par(mfrow=c(2,1)) # Plot the graphs in 2 rows, 1 column
wa = rnorm(502, 0,1) # 502 Moving average observations
va = filter(wa, sides=2, rep(1,3)/3)
acf(va,20,  # Plot and print the results for part a
    main = "Series of n=500 Moving Average Observations",
    na.action=na.pass)
wb = rnorm(52, 0,1) # 52 Moving average observations
vb = filter(wb, sides=2, rep(1,3)/3)
acf(wb,20,  # Plot and print the results for part a
    main = "Series of n=50 Moving Average Observations",
    na.action=na.pass)
dev.off()
