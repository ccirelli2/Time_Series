# MINI PROJECT - SEA SURFACE TEMPERATURES --------------
'author:  Chris Cirelli'


# Clear Namespace & Charts-------------------------------
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# Import Libraries --------------------------------------
library(ggplot2)
library(readr)
library(bsts)
library(oce)
library(ggplot2)
library(astsa)
library(TSA)
library(pracma)
library(dplyr)
library(lubridate)
library(reshape)
library(tseries)
library(forecast)
library(dplyr)

# Import Data -------------------------------------------
setwd('/home/ccirelli2/Desktop/repositories/Time_Series/mini_project/data')
data <- read_csv('data.csv')

# Data Exploration --------------------------------------

# Get Summary Statistics
summary(data)
data$startDate 
' Note:  2018 only has 4 observations. 
         Also, while the days of the month match up between different years, 
         the day of the month is different from month to month.  This may or may
         not affect our analysis if their is a trend within months  '

# Modify Columns
data$SST <- data$`0`
data$timeIdx <- NULL
data$`0` <- NULL

# Create Time Series
'  Obj: Create a time series of one of the lvls using different cycle values
        The choice of cycles was taken from the exploration of the data'
ts.data.10.freq1  <- ts(data$`10`, frequency=1)
ts.data.10.freq6 <- ts(data$`10`, frequency=6)
ts.data.10.freq11 <- ts(data$`10`, frequency=11)
ts.data.sst.freq1  <- ts(data$`SST`, frequency=1)
ts.data.sst.freq6 <- ts(data$`SST`, frequency=6)
ts.data.sst.freq11 <- ts(data$`SST`, frequency=11)


# Plot 1 - All Levels 
data.range <- data[4:24,]
data.range <- data
ggplot(data.range, aes(data.range, x= data.range$startDate)) + 
  geom_line(aes(y=data.range$`10`), colour='red') +
  geom_line(aes(y=data.range$`20`), colour='green') + 
  geom_line(aes(y=data.range$`30`), colour='blue') + 
  geom_line(aes(y=data.range$`40`), colour='black') + 
  geom_line(aes(y=data.range$`50`), colour='purple') + 
  geom_line(aes(y=data.range$`60`), colour='yellow') + 
  geom_line(aes(y=data.range$`70`), colour='black') + 
  geom_line(aes(y=data.range$`80`), colour='grey') + 
  geom_line(aes(y=data.range$`90`), colour='brown')  + 
  ylab('Temperature') + xlab('Dates') + ggtitle('TS - Gibralter Sea Surface Temperatures - By lvl') 
' Observations:  There is a clear seasonal trend/cycle for this time-series'

# Plot 2 - Averages By Month
df2 <- data 
df2$month <- month(as.POSIXlt(data$startDate, format="%m-%d-%y")) 
df2$startDate <- NULL
df2$SST <- NULL
df2.groupby.month <- group_by(df2, month)
df2.avg.by.month <- aggregate.data.frame(df2.groupby.month, by=list(df2.groupby.month$month), mean)
plot(df2.avg.by.month$`10`, type='l', main='TS - Avg Monthly Temp')

ggplot(df2.avg.by.month, aes(df2.avg.by.month$month)) + 
  geom_line(aes(y=df2.avg.by.month$`10`), colour='red') +
  geom_line(aes(y=df2.avg.by.month$`20`), colour='green') + 
  geom_line(aes(y=df2.avg.by.month$`30`), colour='blue') + 
  geom_line(aes(y=df2.avg.by.month$`40`), colour='black') + 
  geom_line(aes(y=df2.avg.by.month$`50`), colour='purple') + 
  geom_line(aes(y=df2.avg.by.month$`60`), colour='yellow') + 
  geom_line(aes(y=df2.avg.by.month$`70`), colour='black') + 
  geom_line(aes(y=df2.avg.by.month$`80`), colour='grey') + 
  geom_line(aes(y=df2.avg.by.month$`90`), colour='brown') +   
  ylab('Avg Sea Lvl Temp') + xlab('Months') + ggtitle('TS - Sea Surface Temps Grouped By Month') 
' Oberservations:  All lvls appear to exhibit the same seasonal trend, although some (70-90)
  are less pronounced.  These may require a different arima process'

# Seasonal Plot (Repeating Cycle)
' At Month 8: The Cicle is highest
  At Month 2: Temperature is at its lowest. 
  Cycle: Appears to be every 6 months'
data$month <- month(as.POSIXlt(data$startDate, format="%m-%d-%y")) 
test <- data.frame(data$month, data$`SST`)
plot(x=test$data.month, y=test$data.SST, main='Cycles By Month')


# Check for Stationarity & Determine Seasonality-----------------------------------

# Plot Data
plot(ts.data.sst.freq1, main='TS Plot - Lvl 10 - Freq 1')

# Plot ACF & PACF
acf2.no.trans <- acf2(ts.data.sst.freq11)
' Observations:  Non stationary.  ACF does not tail off to zero'

# Plot Decomposition
plot(decompose(ts.data.sst.freq11))
plot(decompose(ts.data.sst.freq6))
' Observations:  
    - As far as I can tell, the frequency of the cycle occurs at 11 as opposed to 6, which would
      explain why the trend'

# Check Stationarity - Constant Mean
monthplot(data$SST, main='TS - "SST" - Mean by Month')
monthplot(data$`20`, main='TS - "20" - Mean by Month')
' Observation: Not stationary.  Probably requires one 
  non-seasonal difference.'

# Determine if Trend Exist (Moving Average)
ts.mvavg.2 <- movavg(ts.data.10.freq11, n=2, type='s')
ts.mvavg.11 <- movavg(ts.data.10.freq11, n=11, type='s')
plot(ts.mvavg.11, type='l', main='Moving Average - N=11')
' There seems to be a slight upward trend in the data, so 
  a single non-seasonal differencing may be in order. '

# Is Varaince Constant?
ts.10.mean <- mean(ts.data.10.freq11)
df.var <- data.frame(rep(ts.10.mean, length(ts.data.10.freq11)))
names(df.var)[names(df.var) == 'rep.ts.10.mean..length.ts.data.10.freq11..'] <- 'TS.Mean'
df.var$Lvl10 <- ts.data.10.freq11
df.var$month <- month(as.POSIXlt(data$startDate, format="%m-%d-%y")) 
df.var$var <- (ts.data.10.freq11 - ts.10.mean)^2
df.var.by.month <- group_by(df.var, df.var$month)
df.groupby.month <- aggregate(df.var, list(df.var$month), FUN=mean)
plot(df.groupby.month$var, type='l', main="TS Variance By Month")

# Dickey Fuller Test 
adf.test(data$`10`, k=11)
      ' p-value= 0.027
    reject null hypothesis that data is stationary'


# Apply Transformations ---------------------------------------------
' D = 6, d = 1, log=True'
ts.D6.d1.log <- diff(log(ts.data.10.freq6), lag=6)
plot(ts.D6.d1.log, main='TS - Transformed - D=6, d=1, log=True')

' D = 11, d = 1, log=True'
ts.D11.d1.log <- diff(log(ts.data.10.freq6), lag=11)
plot(ts.D11.d1.log, main='TS - Transformed - D=11, d=1, log=True')

# Plot ACF & PACF to Determine SARIMA Order -------------------------------
acf2(ts.D6.d1.log)
acf2(ts.D11.d1.log)
  ' ACF:  The ACF appears to indicate a seasonal AR process.
        The seasonal component of the PACF indicates a seasonal AR process of 2 or 3. 
        The non-seasonal aspect of the ACF appears also to be an AR process of order 1. 
        Differencing should probably be 1
        Season should be 11.' 
         
 
# Identifying Optimal SARIMA Parameters - Auto.Arima -----------------------
' https://stackoverflow.com/questions/56192723/how-to-automate-sarima-model-for-time-series-forecasting
  Auto.Arima = https://towardsdatascience.com/time-series-analysis-with-auto-arima-in-r-2b220b20e8ab
  - Webpage says that the auto.arima helps to implement Regression model with ARIMA errors. 
  - https://pkg.robjhyndman.com/forecast/reference/auto.arima.html
  - Note that auto-arima captures the seasonal components as well. 
'

# Step1: See which seasonal frequency has the lowest AIC
aa.freq1 <- auto.arima(ts.data.10.freq1)
aa.freq6 <- auto.arima(ts.data.10.freq6)
aa.freq11<- auto.arima(ts.data.10.freq11)

# Step2: Fit Model
aa.aic.results <- c(aa.freq1$aic, aa.freq6$aic, aa.freq11$aic)
barplot(aa.aic.results, names.arg = c('freq1', 'freq6', 'freq11'), main='AIC Results For Diff Lags')
' Observations:  Freq of 11 has th elowest AIC'

# Step3:  Train / Test Split
' Note:  We are using the actual data here without transformations'
num.train <- floor(length(ts.data.10.freq11) * 0.8)
ts.train <- ts.data.10.freq11[1:num.train]
ts.test <- ts.data.10.freq11[ (num.train+1) : length(ts.data.10.freq11)]



# Train Sarima Model - Manually Select Orders -----------------------

# Fit Model To Training Data
s.arima.1 <- arima(ts.train, order=c(1,1,0), seasonal = list(order=c(2,1,0), period=11), )
s.arima.2 <- arima(ts.train, order=c(1,1,0), seasonal = list(order=c(2,1,0), period=11))
s.arima.3 <- arima(ts.train, order=c(1,1,0), seasonal = list(order=c(2,1,0), period=11))

# Generate Prediction For Test Data
s.arima.pred.32 <- predict(s.arima.1, length(ts.test)) 
df.pred.32 <- data.frame(s.arima.pred.32$pred, ts.test)

ggplot(df.pred.32, aes(s.arima.pred.32, x=index(df.pred.32))) + geom_line(aes(y=df.pred.32$s.arima.pred.32.pred, colour='red')) + 
  geom_line(aes(y=df.pred.32$ts.test, colour='green')) + ggtitle('ARIMA(1,1,0)x(2,1,0) - Prediction vs Test')

# Calculate MSE 
s.arima.pred.32.mse <- sum(ts.test - df.pred.32$s.arima.pred.32.pred)^2
s.arima.pred.32.mse


' Ljung-Box test: tries to reject the independence of some values. 
  If p-value < 0.05, you can reject the null hypothesis, i.e. there is dependence. 
  If p-value > 0.05, you fail to reject the null-hyp and assume independnece
  source:  https://online.stat.psu.edu/stat510/lesson/3/3.2.
  
  Observations:  The model suggested by auto.arima does not perform well using the
  sarima function.  Based on our investigation of the data, our intuition tells us 
  that there is a seasonal component to our data.  Indeed, the following order for the
  sarima model performed best in terms of AIC, Ljung-Box statistic and QQ plot measures
  (2,0,1,1,1,1,11), which is a non-seasonal AR(2) model w/ zero differencing, non-seasonal
  MA(1) model, seasonal AR(1) model, seasonal MA(1) model, 1 seasonal difference and 
  a cycle of 11. '




# Fit Auto.Arima model & Make Predictions ------------------------------

# Fit Model - Use Entire Dataset
aa.fit.1 <- auto.arima(ts.data.10.freq11, trace=TRUE, ic='bic')
summary(aa.fit.1)
aa.pred.1 <- forecast(aa.fit.1, length(ts.test))
print(paste('Model ->', aa.pred.1$model))

# Fit Model Arima Using Auto.Arima Model Output
s.arima.4 <- arima(ts.train, order=c(1,0,0), seasonal = list(order=c(2,1,0), period=11), )
sarima(ts.train, 1,0,0,2,1,0,11)
s.arima.4.pred.32 <- predict(s.arima.4, length(ts.test)) 
df.pred.4.32 <- data.frame(s.arima.4.pred.32$pred, ts.test)
s.arima.pred.4.32.mse <- sum(ts.test - df.pred.4.32$s.arima.4.pred.32.pred)^2
s.arima.pred.4.32.mse
summary(s.arima.4)
qqnorm(s.arima.4$residuals)


# Fit Model 
aa.fit.1 <- auto.arima(ts.train, trace=TRUE, ic='bic')
summary(aa.fit.1)
aa.pred.1 <- forecast(aa.fit.1, length(ts.test))
print(paste('Model ->', aa.pred.1$model))

# QQ Plot - Check form Normalicy
plot(aa.pred.1$residuals, main='Prediction Residuals')
qqnorm(aa.pred.1$residuals)
' Note:  Best fit model = (ar1, ar2, ma1) or (p,d,q) = (2,1,1)
         Not clear why it does not provide the seasona components 
         of the process.  If we use the entire ts data set as opposed
         to just the test data, the model suggest an additional sar(1)
         process.
         Residuals look normal, which gives evidence that our model 
         is preddy good.'

# Step5:  Plot Predicted vs Actual 
df.pred <- data.frame(seq(1,48,1))
df.pred$actual <- ts.test
df.pred$prediction <- aa.pred.1$mean
ggplot(df.pred, aes(df.pred, x=seq.1..48..1.)) + geom_line(aes(y=actual, colour='actual')) +
  geom_line(aes(y=prediction, colour='prediction')) + ylab('Temperature') + 
  xlab('Last 48 Measurements') + ggtitle('SARIMA Model - Sea Surface Temps - 
                                        Actual vs Prediction - Last 48 periods') 

# Step6:  Assess Model - Calculate MSE
pred1.mse <- round(sum(ts.test - aa.pred.1$mean)^2,2)
pred1.rmse <- round(sqrt(pred1.mse),2)
print(paste('Auto arima prediction MSE & RMSE -> ', pred1.mse, '/', pred1.rmse))

# Plot Forecast - N Periods After TS Ends
aa.fit.2  <- auto.arima(data$`10`)
aa.pred.24 <- forecast(aa.fit.2, h=24) 
aa.pred.64 <- forecast(aa.fit.2, h=64) 
aa.pred.64 <- forecast(aa.fit.2, h=128) 
par(mfrow=c(3,1))
plot(aa.pred.24, main='Prediction - 24 Additional Values')
plot(aa.pred.48, main='Prediction - 64 Additional Values')
plot(aa.pred.64, main='Prediction - 128 Additional Values')
' Observations:  Model does a pretty good job of capturing the seasonal trends of the original time-series
                 As expected, when the periods get larger, the forecast regresses to a constant mean'
# Compare MSE of Maunual vs Auto.Arima Model -----------------------------
  


# Multivariant Time Series Prediction Model -----------------------------------

# Prepare Columns
data$startDate <- NULL

# Create Dataset
ts.mtv <- ts(data[1:10])

# Train / Test Split
train.length <- floor(length(ts.mtv[,1]) * 0.8)
test.length  <- length(ts.mtv[,1]) - train.length
ts.mtv.train <- ts.mtv[1: train.length * 0.7, 1:10]   
ts.mtv.test  <- ts.mtv[127: length(ts.mtv[,1]), 1:10]

head(ts.mtv.train)
length(ts.mtv.test)
 
# Compute Cross-Covariance of Two Univariate Time Series
' References: 
  https://en.wikipedia.org/wiki/Cross-correlation
'
# Iterate Columns - Calculate Correlation Between Each Column & Target
plot(cor(ts.mtv.train[,1:9],ts.mtv.train[,10]))


# Generate Scatter Plot
scatter.smooth(x=ts.mtv.train[,1:9], y=ts.mtv.train[,10])
pairs(ts.mtv, main='Scatter Plot of SST vs Sea Level Temps')
?arima
# Fit Multivariate ARIMA Model
sarima(ts.mtv[,10], 1,0,0,1,0,0,12, xreg=ts.mtv[,1:9])
ts.mtv.arima.fit <- Arima(ts.mtv.train[,10], order=c(1,0,0), seasonal=list(order=c(1,0,0), period=12), 
                      xreg=ts.mtv.train[,1:9])

ts.mtv.pred <- predict(ts.mtv.arima.fit, xreg=ts.mtv.test, h=14)
