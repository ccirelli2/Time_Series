# Clear Namespace --------------------------------------------------------------
rm(list=ls())

# Get AirPassenger Dataset -----------------------------------------------------
'AirPassengers = shows trend and possible seasonality or ciclicality'
data(AirPassengers)

# Slide 26 - White Noise -------------------------------------------------------
'iid = independent, identically distributed
 ex  = mu = 0, var = sigma^2'

# Generate 500 iid points w/ mean=0, sd=1
w = rnorm(500, mean=0, sd=1)
plot(as.ts(w, type='l', main='White Noise'))
w_var = var(w)
w_mu  = mean(w)

# Calculated Lagged Difference -------------------------------------------------
'x   = series of integers, 
 lag = an integer indicating which lag to use, 
 diff= an integer indicating the order of the diff
 '
# Test Function
t      = c(0,1,2,3,4)
t_diff = diff(x=t, lag=1, differences=1)    # returns (1,1,1,1), w/ len of 4

# Try on AirPassenger Dataset
ap_diff = diff(x=AirPassengers, lag=1, differences=1)

# Plot Original & Transformed AirPassenger Dataset
' You can see that the upward trend has been eliminated
  Why does this work?  Note than in the test function example, when we calculate
  the difference we are left with only the difference, not the progressively growing
  series.  Instead of going from 0-4 we are left with a constant, which is the growth. 
  Question:  Why doesnt this only leave us with the value of the trend as opposed to
             the data less the trend?  
'
par(mfrow=c(2,1))
plot(as.ts(AirPassengers, main='Original AirPassenger Dataset'))
plot(as.ts(ap_diff, main='Lagged Difference AirPassenger Dataset'))

# Calculate & Plot Auto Cross-Covariance & Correlation -----------------------------------
' acf:  stands for auto correlation function.  It is auto correlation because it is 
        the correlation between different time periods of the same series. 
        it shows the auto correlation between time periods p(0), p(1), etc. '
rm(list=ls())
acf(AirPassengers)
acf(ap_diff)

# Decomposition ---------------------------------------------------------------------------
'ref:  https://anomaly.io/seasonal-trend-decomposition-in-r/index.html
 def:  mathmatical operation that splits a time series into its component parts, eg
       trend, seaonality and noise. 
 additive decomposition = Seaonal + trend + noise; use when seaonal variation is constant
 multiplicative decomp  = Seaonal * trend * noise; use when seasonal variation is going up or down over time

 4-Step Process

 1.) Detect Trend
 2.) Detrend Time Series (using either the additive or multiplicative approach)
 3.) Get Average Seaonality
 4.) Get Remaining Noise
'
rm(list=ls())
library(forecast)

# Step1 - Detect Trend Using Moving-Average
'Note that this is monthly data, so it appears that the most appropriate cycle would be 
 every year or 12 months, e.g. order = 12'

# Load Data
data(AirPassengers)

# Calculate Moving Average
trend_passenger_count = ma(AirPassengers, order=12)
plot(as.ts(AirPassengers), main='Original Time Series')
lines(trend_passenger_count)
plot(as.ts(trend_passenger_count), main='Air Passenger Trend')

# Step2 - Detrend by Subtracting out the trend
'Note: Note the difference.  This is clearly a case when we should use the multiplicative
       detrend.  Otherwise, the magnitude of the trend / seaonality still grows with time'
additive_detrend = AirPassengers - trend_passenger_count
multiplicative_detrend = AirPassengers / trend_passenger_count
par(mfrow=c(2,1))
plot(as.ts(additive_detrend), main='Air Passenger - Additive Detrended')
plot(as.ts(multiplicative_detrend), main='Air Passenger - Multiplicative Detrend')

# Step3 - Get Average Seasonality
' Add the Seasonality together and divide by the seasonality period
  This requires creating a matrix w/ each column containing elements
  of the same period (ex: elements only from january, etc)
  Finally, we compute the mean of each column
  note:  t(matrix) means matrix transpose
'

# Average Seasonality Using Additive Decomposition
matrix_airpassengers = t(matrix(data=additive_detrend, nro=12))
seasonal_passengers  = colMeans(matrix_airpassengers, na.rm=T)
plot(as.ts(rep(seasonal_passengers, 12)))

# Average Seasonality Using Multiplicative Decomposition
matrix_airpassengers = t(matrix(data=multiplicative_detrend, nro=12))
seasonal_passengers  = colMeans(matrix_airpassengers, na.rm=T)
plot(as.ts(rep(seasonal_passengers, 12)))

# Step4 - Analyze Random Noise
' Random = Time Series - Seasonal  - Trend
  Random = Time Series / (Trend * Seasonal)'

# Additive Formula
''
random_airpassengers = AirPassengers - trend_passenger_count - seasonal_passengers
plot(as.ts(random_airpassengers), main='White Noise')

# Multiplicative
random_airpassengers = AirPassengers / (trend_passenger_count * seasonal_passengers)
plot(as.ts(random_airpassengers), main='White Noise')

# Decomposition Function
ts_passengers = ts(AirPassengers, frequency=12)
decompose_passengers = decompose(ts_passengers, 'additive')
plot(decompose_passengers)




# Smoothing ----------------------------------------------------------------------
'https://johannesmehlem.com/blog/exponential-smoothing-time-series-forecasting-r/#chapter2'




