# Gaussian White Noise Simulation
# October 1, 2013

# Simulate a series of n=500 Gaussian white noise observations 
# and compute the sample ACF to lag 20. 
# Compare the sample ACF you obtain to the actual ACF

png("c:/Users/Nate/Git/riemann/gaussian_white_noise_simulation_500.png")
wa = rnorm(500, 0,1) # 500 white noise observations
acf(wa,20,  # Plot and print the results for part a
    main = "Series of n=500 Gaussian White Noise Observations")
dev.off()

png("c:/Users/Nate/Git/riemann/gaussian_white_noise_simulation_50.png")
wb = rnorm(50, 0,1) # 500 white noise observations
acf(wb,20,  # Plot and print the results for part a
    main = "Series of n=50 Gaussian White Noise Observations")
dev.off()

# ----------------------------------------------------------------------------
# Now, plot both of the acf curves on the same output (for comparison)
png("c:/Users/Nate/Git/riemann/gaussian_white_noise_simulation_500_50.png")
par(mfrow=c(2,1)) # Plot the graphs in 2 rows, 1 column
wa = rnorm(500, 0,1) # 500 white noise observations
acf(wa,20,  # Plot and print the results for part a
    main = "Series of n=500 Gaussian White Noise Observations")
wb = rnorm(50, 0,1) # 500 white noise observations
acf(wb,20,  # Plot and print the results for part a
    main = "Series of n=50 Gaussian White Noise Observations")
dev.off()
