# Looking at heteroscedastcitity
# October 1, 2013

# Melting glaciers deposit yearly layers of sand and silt during the spring
# melting seasons, which can be reconstructed yearly over a period ranging
# from the time deglaciation began in New England (about 12,600 years ago)
# to the time it ended (about 6,000 years ago). Such sedimentary deposits,
# called varves, can be used as proxies for paleoclimatic parameters, such as
# temperature, because, in a warm year, more sand and silt are deposited
# from the receding glacier. The data "varve" shows the thicknesses of the yearly
# varves collected from one location in Massachusetts for 634 years, beginning
# 11,834 years ago. For further information, see Shumway and Verosub (1992).
# Because the variation in thicknesses increases in proportion to the amount
# deposited, a logarithmic transformation could remove the nonstationarity
# observable in the variance as a function of time.

library("astsa") # Load the package from the library
ls() # List all the objects that are in the working environment

require(astsa) # Ensure that you have the package ready to be used
# You could also create a function .First <- function(){require(astsa)}
# which runs every time R opens

data(varve) # Load the glacial data
varve # Inspect the data element

varv1 = varve[1:317] #Split the data into 2 parts (subset1)
varv2 = varve[318:634] #Split the data into 2 parts (subset1)

var(varv1) # Calculate the variance of subset 1
var(varv1) # Calculate the variance of subset 2
var(log(varv1)) # Calculate the variance of the log for subset 1
var(log(varv2)) # Calculate the variance of the log for subset 2

png('c:/Users/Nate/Git/riemann/variance_heteroscedastic_full_data.png')
par(mfrow = c(1,2)) # Plot the data ... 1 row, 2 columns
hist(varve,
     main = "Histogram of Varve") # Generate a histogram of the overall data
hist(log(varve),
     main = "Histogram of log(Varve)") # Generate a histogram of the log of the overall data
dev.off()

png('c:/Users/Nate/Git/riemann/variance_heteroscedastic_full_data_100.png')
par(mfrow = c(1,1)) # Plot the data ... 1 row, 2 columns
plot(log(varve),
     main = "Plot of the Varve values over time") # Check for time intervals of the order of 100
dev.off()

png('c:/Users/Nate/Git/riemann/variance_heteroscedastic_full_data_acf.png')
par(mfrow = c(1,1)) # Plot the data ... 1 row, 2 columns
acf(log(varve),
    main = "Plot showing the ACF Function For The Varve Data") # Examine the ACF of the glacier data
dev.off()

png('c:/Users/Nate/Git/riemann/variance_heteroscedastic_full_data_comparison.png')
par(mfrow = c(2,1))
plot(diff(log(varve)),
     main = "Time Plot Of The Differenced Series") # Difference the data and plot the data over time
acf(diff(log(varve)),
    main = "ACF Function of the Differenced Series") # Plot the acf function
dev.off()