# Statistical Analysis Of Time Series Data
# Reference to book material found at http://www.stat.pitt.edu/stoffer/tsa3/
# September 8, 2013

# Note that the R package used for the text is astsa
install.packages("astsa") # Install the R package
library("astsa") # Load the package from the library
ls() # List all the objects that are in the working environment

require(astsa) # Ensure that you have the package ready to be used
# You could also create a function .First <- function(){require(astsa)}
# which runs every time R opens

# ---------------------------------------------------------------------------
# Plotting Earnings Data for Johnsons & Johnsons
data(jj) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/quarterly_earnings.png')
plot(jj, type="o", ylab="Quarterly Earnings per Share",
     main="Plot of J & J Quarterly Earnings: 1960 - 1980")
dev.off()

# ---------------------------------------------------------------------------
# Plotting global warming data
data(gtemp) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/temp_deviations.png')
plot(gtemp, type="o", ylab="Global Temperature Deviations",
     main="Global Temperature Deviations: 1880 - 2009")
dev.off()

# ---------------------------------------------------------------------------
# Plotting of speech data
data(speech) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/speech.png')
plot(speech, main="Plotting Speech Sound Frequency Data")
dev.off()

# ---------------------------------------------------------------------------
# Plotting of NYSE financial returns
# Data covers the period from February 2, 1984 to December 31, 1991
data(nyse) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/stock_returns.png')
plot(nyse, ylab="NYSE Returns", 
     main="Daily Returns of the New York Stock Exchange")
dev.off()

# ---------------------------------------------------------------------------
# Comparing El Nino with fishing populations over time
# Example illustrating the plotting of multiple types of data
data(soi) # Command to use the data set from the library
data(rec) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/El_Nino_vs_Fishing_Populations.png')
par(mfrow = c(2,1)) # Create a 2 row by 1 column frame
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")
dev.off()

# ---------------------------------------------------------------------------
# Plotting various brain patterns over time between cortex and thalamus & cerebellum
data(fmri1) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/brain_patters.png')
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
ts.plot(fmri1[,2:5], lty=c(1,2,4,5), ylab="BOLD", xlab="", main="Cortex")
ts.plot(fmri1[,6:9], lty=c(1,2,4,5), ylab="BOLD", xlab="", main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=2)
dev.off()

# ---------------------------------------------------------------------------
# Comparing the vibrations of an earthquake shock & an explosion
data(EQ5) # Command to use the data set from the library
data(EXP6) # Command to use the data set from the library
png('c:/Users/Nate/Git/riemann/earthquake_vs_explosion.png')
par(mfrow=c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")
dev.off()