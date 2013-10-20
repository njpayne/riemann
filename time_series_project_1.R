# October 16, 2013
# Time series analysis - Project 1
# Nathaniel Payne

# Let's look at an initial data set
# This data set contains information on the data on the age of death 
# of successive kings of England, starting with William the Conqueror 
# (original source: Hipel and Mcleod, 1994).
kings <- scan("C:/Users/Nate/Git/riemann/kings.dat",skip=3) # Read in the data
kings_ts <- ts(kings) # Use the time series function to put the data in time series form

# Plot the graph showing the age of Kings in England
png("C:/Users/Nate/Git/riemann/kings_ts_graph.png")
plot.ts(kings_ts) # Plot the time series
title(main = "Age of Death of Successive English Kings")
dev.off()

# Plot the graph showing the number of births per month in New York city, 
# from January 1946 to December 1959
births <- scan("C:/Users/Nate/Git/riemann/nybirths.dat") # Read in the data
births_ts <- ts(births) # Use the time series function to put the data in time series form

# Plot the graph showing the number of births per month in New York city
png("C:/Users/Nate/Git/riemann/briths_ts_New_York.png")
plot.ts(births_ts) # Plot the time series
title(main = "Number of Births per Month in New York City: 1946-59")
dev.off()

# install.packages("fpp") # This is the package from the forecasting library
library(fpp) # Open the library
# Below are some examples showing various types of time series data
png("C:/Users/Nate/Git/riemann/compare_time_series.png")
par(mfrow=c(2,2))
plot(hsales,xlab="Year",ylab="Monthly housing sales (millions)")
plot(ustreas,xlab="Day",ylab="US treasury bill contracts")
plot(elec,xlab="Year",ylab="Australian monthly electricity production")
plot(diff(dj),xlab="Day",ylab="Daily change in Dow Jones index")
title(main="Graphs Comparing Different Time Series Patterns", line=-2, outer=TRUE)
dev.off()

# Let us try now decomposing a time series data set
# The data shows the number of new orders for electrical equipment (computer, electronic and 
# optical products) in the Euro area (16 countries). The data have been adjusted by working 
# days and normalized so a value of 100 corresponds to 2005.
png("C:/Users/Nate/Git/riemann/electrical_manufacturing.png")
par(mfrow=c(1,1))
fit <- stl(elecequip, s.window=5)
plot(elecequip,
     main="Electrical Equipment Manufacturing - Euro Area - 2005",
     ylab="New orders index", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")
dev.off()

png("C:/Users/Nate/Git/riemann/electrical_manufacturing_decomp.png")
plot(fit,
     main = "Decomposition of the Electrical Manufacturing Data Set") # Decompose the time series data using the forecasing data set
dev.off()

# Let us decompose the time series components
births_ts_decompose <- decompose(births_ts)

# Create a basic 5 period moving average
elecequip_ma <- ma(elecequip, order = 5)

# Plot the moving average against the electricity sales
png("C:/Users/Nate/Git/riemann/electrical_manufacturing_12_Month_MA.png")
plot(elecequip, main="Electrical Equipment Manufacturing - 12 Month MA",
     ylab="GWh", xlab="Year")
lines(ma(elecequip_ma,12),col="red") # This is the 12 period moving average
dev.off()

# Plotting various moving average models
png("C:/Users/Nate/Git/riemann/electrical_manufacturing_Various_Month_MA.png")
plot(elecequip, main="Electrical Equipment Manufacturing - Various MA Models",
     ylab="GWh", xlab="Year")
lines(ma(elecequip_ma,6),col=2) # This is the 6 period moving average
lines(ma(elecequip_ma,12),col=3) # This is the 12 period moving average
lines(ma(elecequip_ma,24),col=4) # This is the 24 period moving average
legend("topleft", c("6-Month","12-Month", "24-Month"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c(2,3,4),
       cex=0.75)
dev.off()

# Decompose the time series using an STL decomposition
png("C:/Users/Nate/Git/riemann/electrical_manufacturing_stl_decomposition.png")
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit, main = "STL Decomposition Of Electrical Manufacturing Data")
dev.off()

# Create some Naive forecasts of the seasonally adjusted data using STL decomposition
png("C:/Users/Nate/Git/riemann/electrical_manufacturing_stl_naive_forecast.png")
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",
     main="Naive Forecasts of Seasonally Adjusted Manufacturing Data")
dev.off()

# Create Forecasts using STL & Random Walk
png("C:/Users/Nate/Git/riemann/electrical_manufacturing_stl_naive_rw.png")
fcast <- forecast(fit, method="naive")
plot(fcast, ylab="New orders index")
dev.off()

# ------------------------------------------------------------------------------------------
# Exponential Smoothing
# Dataset contains total annual rainfall in inches for London, from 1813-1912 
# (original data from Hipel and McLeod, 1994).

rain <- scan("C:/Users/Nate/Git/riemann/precip.dat",skip=1)
rainseries <- ts(rain,start=c(1813))

# Plot the data set
png("C:/Users/Nate/Git/riemann/precip_plot.png")
plot.ts(rainseries, main = "Total Annual Rainfall in Inches for London: 1813-1912")
dev.off()

# Plot the exponential smoothing line
png("C:/Users/Nate/Git/riemann/precip_plot_expo_smoo.png")
precip_expo <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
plot(precip_expo, main = "Exponential Smoothing - Inches Of Rainfall In London: 1813-1912")
legend("topleft", c("Actual","Exponential Smoothing"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c(1,2),
       cex=0.75)
dev.off()

# Generate forecasts for the next 8 periods using exponential smoothing
precip_expo_forecast <- forecast.HoltWinters(precip_expo, h=8)

# Plot forecasts for the next 8 periods on the original data
png("C:/Users/Nate/Git/riemann/precip_plot_expo_forecast_8.png")
plot.forecast(precip_expo_forecast,
              xlab = "Year",
              ylab = "Rainfall In Inches",
              main = "Forecasts For The Exponential Smoothing Model")
dev.off()

# Let us review the acf function for the Exponential Smoothing Method
png("C:/Users/Nate/Git/riemann/precip_plot_expo_acf.png")
acf(precip_expo_forecast$residuals, 
    lag.max=20,
    main = "ACF Function For The Exponential Smoothing Model")
dev.off()

# Carry out the Ljung-Box Test
Box.test(precip_expo_forecast$residuals, lag=20, type="Ljung-Box")

# Check that the residuals are normally distributed
png("C:/Users/Nate/Git/riemann/precip_plot_expo_residuals.png")
plot.ts(precip_expo_forecast$residuals,
        main = "Plot Of Residuals - Exponential Smoothing Model")
dev.off()

# Check whether the forecast errors are normally distributed with mean zero
plotForecastErrors <- function(forecasterrors) # Create a function to generate the hist
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins, main = "Histogram Of Forecast Errors")
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# Pass the array of residuals into the function
png("C:/Users/Nate/Git/riemann/precip_plot_expo_residuals_error.png")
plotForecastErrors(precip_expo_forecast$residuals)
dev.off()

# -------------------------------------------------------------------------------------
# Holt's Exponential smoothing
# Example data set: The annual diameter of women's skirts at the hem, from 1866 to 1911.
skirts <- scan("C:/Users/Nate/Git/riemann/skirts.dat",skip=5) # Read in the data

# Generate the time series model
skirtsseries <- ts(skirts,start=c(1866))

# Plot the raw data
png("C:/Users/Nate/Git/riemann/skirts_raw.png")
plot.ts(skirtsseries,
        ylab="Skirt Height (Inches)",
        main = "Annual Diameter of Women's Skirts At Hem: 1866 to 1911") # Plot the time series model
dev.off()

# Let's look at Holt's predictions
skirt_holts <- HoltWinters(skirtsseries, gamma=FALSE)

# Plot the resulting output
png("C:/Users/Nate/Git/riemann/skirts_holts.png")
plot(skirt_holts, main = "Holt's Model - Diameter of Women's Skirts At Hem: 1866 to 1911")
legend("topright", c("Actual","Holt's"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c(1,2),
       cex=0.75)
dev.off()

# Calculate projections for the next 20 time periods
skirt_holts_forecast <- forecast.HoltWinters(skirt_holts, h=20)

# Plot the projections
png("C:/Users/Nate/Git/riemann/skirts_holts_forecast.png")
plot.forecast(skirt_holts_forecast,
              main = "20 Period Forecast Generated Using Holt's Model",
              ylab = "Skirt Height (In Inches)",
              xlab = "Year")
legend("topleft", c("Actual","Forecast"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c(1,"blue"),
       cex=0.75)
dev.off()

# Check whether the forecast can be improved upon
png("C:/Users/Nate/Git/riemann/skirts_holts_forecast_acf.png")
acf(skirt_holts_forecast$residuals, lag.max=20,
    main = "ACF Function Generated From Holt's Forecast")
dev.off()

# Let us look at the forecast errors
png("C:/Users/Nate/Git/riemann/skirts_holts_forecast_errors.png")
plotForecastErrors(skirt_holts_forecast$residuals)
dev.off()

# ----------------------------------------------------------------------------------------
# Holt-Winters

# The data set souvenir provides monthly sales for a souvenir shop at a beach resort town 
# in Queensland, Australia, for January 1987-December 1993 (original data from Wheelwright 
# and Hyndman, 1998).

souvenir <- scan("C:/Users/Nate/Git/riemann/souvenir.dat")
souvenir_ts <- ts(souvenir, frequency=12, start=c(1987,1))

# Plot the data set
png("C:/Users/Nate/Git/riemann/souvenir_plot.png")
plot.ts(souvenir_ts,
        ylab = "Dollars Spent (Revenue)",
        main = "Monthly Sales for A Growing Souvenir Shop At A Beach Resort")
dev.off()

# The data set is not additive and needs to be transformed ... Thus, let's transform it
logsouvenir <- log(souvenir_ts)

# Let us plot the original data here
png("C:/Users/Nate/Git/riemann/logsouvenir_plot.png")
plot.ts(logsouvenir,
        ylab = "Dollars Spent (Revenue)",
        main = "Log(Monthly Sales) for A Souvenir Shop At A Beach Resort")
dev.off()

# Let us use Holt-Winters model here ...
souvenir_hw <- HoltWinters(logsouvenir)

# Let us plot Holt-Winters overtop of the data set
png("C:/Users/Nate/Git/riemann/souvenir_hw_plot.png")
plot(souvenir_hw,
     main = "Holt-Winters Applied To Log(Sales) Of A Souvenir Beach Shop")
legend("topleft", c("Actual","Holt-Winters"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c(1,"red"),
       cex=0.75)
dev.off()

# Generate forecasts of the next 48 months
souvenir_hw_forecast <- forecast.HoltWinters(souvenir_hw, h=48)

# Plot the forecasts
png("C:/Users/Nate/Git/riemann/souvenir_hw_forecast.png")
plot.forecast(souvenir_hw_forecast, ylab = "Log(Dollars)", xlab = "Time (Years)",
              main = "Holt-Winters Forecast For Log(Sales) Of A Souvenir Beach Shop")
legend("topleft", c("Actual","Forecast (Holt-Winters)"), 
       lty=c(1,1),
       lwd=c(2.5,2.5),
       col=c(1,"blue"),
       cex=0.75)
dev.off()

# Plot the ACF
png("C:/Users/Nate/Git/riemann/souvenir_hw_forecast_acf.png")
acf(souvenir_hw_forecast$residuals, 
    lag.max=20, 
    main = "ACF From Holt-Winters Model: Log(Sales)")
dev.off()

# Conduct the Ljung-Box test
Box.test(souvenir_hw_forecast$residuals, lag=20, type="Ljung-Box")

# Let us plot the forecast errors in a Histogram
png("C:/Users/Nate/Git/riemann/souvenir_hw_forecast_errors.png")
plotForecastErrors(souvenir_hw_forecast$residuals)
dev.off()

# ----------------------------------------------------------------------------------------
# ARIMA Models & Differencing A Series

# Let us different the ARIMA model for Series A
skirts_diff <- diff(skirtsseries, differences=1)

png("C:/Users/Nate/Git/riemann/skirts_diff.png")
par(mfrow=c(2,1))
plot.ts(skirtsseries, main = "Undifferenced Time Series - Women's Skirt Heights")
plot.ts(skirts_diff, main = "Differenced Time Series - Women's Skirt Heights")
dev.off()

skirts_diff_twice <- diff(skirtsseries, differences=2) # Difference the time series twice

png("C:/Users/Nate/Git/riemann/skirts_diff_twice.png")
par(mfrow=c(3,1))
plot.ts(skirtsseries, main = "Undifferenced Time Series - Women's Skirt Heights")
plot.ts(skirts_diff, main = "Single Differenced Time Series - Women's Skirt Heights")
plot.ts(skirts_diff_twice, main = "Twice Differenced Time Series - Women's Skirt Heights")
dev.off()

# Let us revise the kings data set
kings_diff <- diff(kings_ts, differences=1)

png("C:/Users/Nate/Git/riemann/kings_acf.png")
par(mfrow=c(2,1))
acf(kings_diff, lag.max=20, main = "ACF Function for the Once Differenced Kings Data")
pacf(kings_diff, lag.max=20, main = "PACF Function for the Once Differenced Kings Data")
dev.off()

# Let us plot the Kings time series
kings_ts_arima <- arima(kings_ts, order=c(0,1,1))

# library("forecast") # load the "forecast" R library if necessary
kings_ts_arima_forecast <- forecast.Arima(kings_ts_arima, h=5)

par(mfrow=c(1,1)) # Reset the plot area

# Plot the forecasts
png("C:/Users/Nate/Git/riemann/kings_ts_arima_forecast.png")
plot.forecast(kings_ts_arima_forecast, 
              main = "ARIMA (0,1,1) Forecast For The Death Age Of Kings",
              xlab = "Number of periods being considered",
              ylab = "Age of Death")
dev.off()

# Check the errors of the model & plot the acf
png("C:/Users/Nate/Git/riemann/kings_ts_arima_forecast_acf.png")
acf(kings_ts_arima_forecast$residuals,
    main = 'ACF Function For The ARIMA (0,1,1) Death Data')
dev.off()

# Plot the forecast errors to check for normality
png("C:/Users/Nate/Git/riemann/kings_ts_arima_forecast_errors.png")
plot.ts(kings_ts_arima_forecast$residuals,
        ylab = "Residuals",
        main = "Plot Showing Forecast Errors For The Death Data")
dev.off()

# Plot the errors to check for normality (histogram)
png("C:/Users/Nate/Git/riemann/kings_ts_arima_forecast_normal.png")
plotForecastErrors(kings_ts_arima_forecast$residuals)
dev.off()

# Final Combined plot the errors to check for normality
png("C:/Users/Nate/Git/riemann/kings_ts_arima_forecast_normal_error.png")
par(mfrow = c(2,1))
plot.ts(kings_ts_arima_forecast$residuals,
        ylab = "Residuals",
        main = "Plot Showing Forecast Errors For The Death Data")
plotForecastErrors(kings_ts_arima_forecast$residuals)
dev.off()
