# ARIMA Tutorial -------------------------------------------
'Source:  https://online.stat.psu.edu/stat510/lesson/3/3.1

  ARIMA (0,0,0) (p,d,q)
  p:    AR order  
  d:    Number of differences to make stationary
  q:    MA order

'

# Clear Namespace
rm(list=ls())

# Load Libraries
library(astsa)

# How to Determine Appropriate ARIMA Model
' Three items should be considered. 
  1.) Time Series Plot of the Data
  2.) ACF
  3.) PACF

  Plot: Give us indications of what types of transformations we may need. 
  AR1:  ACF will tail off, PACF will cut off at 1.   
  MA1:  ACF will but off at 1, PACF will show a slow decay. 
  ARMA(1,1): 
  **If the ACF and PACF do not tail off, but instead have values that stay close
    to 1 over many lags, then the series is non-stationary and differencing will be
    needed. 
'
plot(AirPassengers)
acf2(AirPassengers)
ap.diff1 <- diff(AirPassengers)
plot(ap.diff1)
acf2(ap.diff1)

ap.diff.log <- diff(log(AirPassengers))
acf2(ap.diff.log)


# SARIMA Model 
'Source:  https://towardsdatascience.com/time-series-forecasting-with-a-sarima-model-db051b7ae459 

  Electricity Usage:
  - Shows Periodicity. 
  - If you are dealing with a SARIMA model, youll need to apply a single as well as seasonal
    difference to your data. 
'

# Take Seasonal difference
ap.diff.s <- diff(log(AirPassengers), lag=12)
plot(ap.diff.s)

# Take non-seasonal difference thereafter
ap.diff.s.d <- diff(ap.diff.s)

# Plot ACF
acf2(ap.diff.s.d)
  
















