### Description
'Notes & R Exercises from chapter 3

Good Definition ARIMA Models (https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/)
  ARIMA is the abbreviation for AutoRegressive Integrated Moving Average. 
  Auto Regressive (AR) terms refer to the lags of the differenced series, 
  Moving Average (MA) terms refer to the lags of errors and I is the number 
  of difference used to make the time series stationary.

Another resource for understanding the output of ARIMA models and how to choose the best
  https://rpubs.com/riazakhan94/arima_with_example

'

# Clear Namespace & Plots
rm(list=ls())

### Example 3.5 MA(1) Process
' R ARIMA model explanation https://people.duke.edu/~rnau/411arim.htm
  Explanation ARIMA(0,0,1) model https://www.quora.com/What-is-the-interpretation-of-an-auto-arima-0-0-1-result-in-r
  ARIMA(p,d,q)
  p = Means that the PACF value = 0, meaning that the lags in the PACF plot are not significant. 
  d = Means that the TS provides is stationary and differences is not necessary. 
  q = In this case equals 1, which means that after one step the gamma(h+1) drops to zero. 
'
# Set up Pallet for Plot
par(mfrow=c(2,1))   # Combines two plots on one pallet. 

# Generate ARIMA Model
model1 = arima.sim(list(order=c(0,0,1), ma=.9), n=100) 
model2 = arima.sim(list(order=c(0,0,1), ma=-0.9), n=100)
# Generate ACF Plot
acf(model1)
'Note that this is a stable series'

# Plot Model
plot(model1, ylab='x', main=(expression(MA(1)~~~theta==+.5)))
'Observations - Looks stable to me.'
plot(model2, ylab='x', main=expression(MA(1)~~~theta==-.5))


### Test if Theta Controls the Variance (observation made on page 84)
model3 = arima.sim(list(order=c(0,0,1), ma=0.1), n=100)
plot(model3, main= 'Theta => 0.1')

test_var_arima_model <- function(theta){
  # Generate Model
  m <- arima.sim(list(order=c(0,0,1), ma=theta), n=100)
  # Return variance
  m_var <- var(m)
  return(m_var)
}

# Iterate For Loop and Calculate Variance for Theta = i/10 (0.1 - 1)
for (i in 1:10){
  theta = i/10
  m_var = test_var_arima_model(theta)
  print(paste('Theta =>', theta, 'Variance', m_var))
}
' Observations:   Clearly the value of Theta controls the variance of the model
                  Variane is positively correlated with theta. 
'

# Generate ACF Plot
for (i in 1:2){
  theta = i/10
  m_var = test_var_arima_model(theta)
  par(mfrow=c(2,1))   # Combines two plots on one pallet.
  acf(m_var)
}



# Page 86 - Fit ARIMA model
' Documentation Notation:  https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average
  on-seasonal ARIMA models are generally denoted ARIMA(p,d,q) where parameters p, d, and q are non-negative 
  integers, p is the order (number of time lags) of the autoregressive model, d is the degree of differencing 
  (the number of times the data have had past values subtracted), and q is the order of the moving-average model. 
  Seasonal ARIMA models are usually denoted ARIMA(p,d,q)(P,D,Q)m, where m refers to the number of periods in 
  each season, and the uppercase P,D,Q refer to the autoregressive, differencing, and moving average terms for
  the seasonal part of the ARIMA model.[2][3]

  When two out of the three terms are zeros, the model may be referred to based on the non-zero parameter, 
  dropping "AR", "I" or "MA" from the acronym describing the model. For example, ARIMA (1,0,0) is AR(1), 
  ARIMA(0,1,0) is I(1), and ARIMA(0,0,1) is MA(1).
'
rm(list=ls())

set.seed(7)
x = rnorm(150, mean=5)

# AR(1) & MA(1) Model
arima(x, order=c(1,0,1)) # AR and MA Order of 1

# AR(1) Model
arima(x, order=c(1,0,0))

# MA(1) Model
arima(x, order=c(0,0,1))


















  


