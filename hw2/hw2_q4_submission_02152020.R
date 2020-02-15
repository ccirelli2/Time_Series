### Question 4 ---------------------------------------------------------------------
' Use sales.xls and perform time series analysis on MONTHLY sales. 
  Make sure your analysis cover the following steps
  1.) Data exploration to check whether the data is stationary and perform any transformational needs
  2.) Model fitting
  3.) Model diagnosis
  4.) Prediction'


# Clear Namespace
rm(list=ls())
dev.off(dev.list())

# Load Libraries
library(readxl)
library(plyr)
library(ggplot2)
library(astsa)
library(tseries)

## DATA PREPARATION --------------------------------------------------------

# Load Data & Get Monthly Sales
sales <- read_excel("Desktop/repositories/Time_Series/hw2/sales.xls")
sales$yr <- format(sales$`Order Date`, format='%Y')
sales$yr_month <- format(sales$`Order Date`, format='%Y-%m')
sales[c('Sales', 'yr_month')]
df.sum <- ddply(sales, c('yr_month'), summarize, Sales = sum(Sales))
df.sum

cycle(df.sum)
  
# Plot Monthly Sales
' Mean and variance do not appear to be constant'
ggplot(df.sum, aes(x=df.sum$yr_month, y=df.sum$Sales, group=1)) + ggtitle('Monthly Sales - Origianal Data') + 
  xlab('Yr + Month') + ylab('Sales $') + geom_line()

# Dickey Fuller Test
adf.test(df.sum$Sales, k=0) # pvalue < 0.05

# Check If Mean Constant
' Mean is not constant and therefore, the timeseries is not stationary'
monthplot(df.sum$Sales, main='Monthly Mean')

# ACF & PACF - Original Data
' ACF Looks like a slow decaying function up to lag=4 & the PACF drops from 0.36 to 0.03 or close to zero. 
  This looks like an AR(1) process'
acf2(df.sum$Sales)

# Take diff and log of data
' Seems to e centered about the mean now.  Some change in variance'
diff.sales <- diff(log(df.sum$Sales))
plot(diff.sales, type='l', main='Monthly Sales - Log of Diff')

# Take ACF & PACF - Differenced Data
' Interpretation:  This looks like an AR model based on the ACF and order of 2 based on the PACF'
acf2(diff.sales, max.lag=12)

## FIT AR MODELS ------------------------------------------------------

# YW Fit
' Note: |phi| < 1'
sales.yw <- ar.yw(diff.sales, order=2)
sales.yw$x.mean
sales.yw.mse <- sum(sales.yw$resid[3: length(sales.yw$resid)]^2) / length(sales.yw$resid[3 : length(sales.yw$resid)])
plot(sales.yw$resid, type='l')
sales.yw$ar # coefficients
sales.yw$asy.var.coef # covariance matrix

# MLE Fit
sales.mle <- ar.mle(diff.sales, order=2)
sales.mle$x.mean
sales.mle.mse <- sum((sales.mle$resid[3: length(sales.mle$resid)])^2) / length(sales.mle$resid)

# YW Prediction
sales.pr <- predict(sales.yw, n.ahead=10)
comb <- c(diff.sales, sales.pr$pred)
ts.plot(comb, type='l', main = 'Plot YW Prediction')  # Plot w/ prediction
lines(sales.pr$pred + sales.pr$se, lty=2) # conf intv
lines(sales.pr$pred - sales.pr$se, lty=2) # conf intv

# MLE Prediction
sales.pr <- predict(sales.mle, n.ahead=10)
comb <- c(diff.sales, sales.pr$pred)
ts.plot(comb, type='l', main='Plot MLE Prediction')  # Plot w/ prediction
lines(sales.pr$pred + sales.pr$se, lty=2) # conf intv
lines(sales.pr$pred - sales.pr$se, lty=2) # conf intv

# Compare MSE
' MLE has the lower MSE'
paste('MLE MSE => ', sales.mle.mse)
paste('YW MSE  => ', sales.yw.mse)


## FIT ARIMA MODELS -----------------------------------------------------------

# Arima Model
sales.mle.ar1 <- arima(diff.sales, c(1,0,0))
sales.mle.ar2 <- arima(diff.sales, c(2,0,0))


# MA Model
sales.mle.ma1 <- arima(diff.sales, c(0,0,1))
sales.mle.ma2 <- arima(diff.sales, c(0,0,2))

# AR(1) AR(2) Predictions
ar1.pr <- predict(sales.mle.ar1, n.ahead=10)
ar1.mse <- sum(ar1.pr$se^2) / length(ar1.pr$se)
ar2.pr <- predict(sales.mle.ar2, n.ahead=10)
ar2.mse <- sum(ar2.pr$se^2) / length(ar2.pr$se)

# MA(1) MA(2) Predictions
ma1.pr <- predict(sales.mle.ma1, n.ahead=10)
ma1.mse <- sum(ma1.pr$se^2) / length(ma1.pr$se)
ma2.pr <- predict(sales.mle.ma2, n.ahead=10)
ma2.mse <- sum(ma2.pr$se^2) / length(ma2.pr$se)

# Compare Results
mse.results <- c(ar1.mse, ar2.mse, ma1.mse, ma2.mse)
mse.labels <- c('AR1', 'AR2', 'MA1', 'MA2')
barplot(mse.results, names.arg = mse.labels, main= 'MSE RESULTS')















