rm(list=ls())
require(astsa)


##################################
##
## air passengers example
##
##################################
x = AirPassengers
plot(x)
plot(log(x))
plot(diff(log(x)))

acf2(diff(log(x)),100)

## several different ways to remove the seasonality


## option 1: remove monthly average
monthplot(diff(log(x)))
xtrans = diff(log(x))
AirPassengers

## get monthly average
cycle(xtrans)
monthly = tapply(xtrans, cycle(xtrans), mean)
monthly 
xmean = c(monthly[2:12],rep(monthly,11))


xx = xtrans - xmean

acf2(xx)
sarima(xx,1,0,1)

## option 2: model the seasonal part as seasonal persistency
ddlx = diff(diff(log(x)),12)
monthplot(ddlx)

acf2(ddlx)

a = sarima(log(x),0,1,1,0,1,0,12)
acf2(resid(a$fit))

a = sarima(log(x),1,1,1,0,1,0,12)
acf2(resid(a$fit))

sarima(log(x),1,1,1,0,1,1,12)

sarima(log(x),0,1,1,0,1,1,12)

sarima(log(x),1,1,0,0,1,1,12)

sarima.for(log(x),12,0,1,1,0,1,1,12)

## option 3: model the seasonal part as a another time series
dlx = diff(log(x))

acf2(dlx)

sarima(log(x),1,1,1,1,0,0,12)

a = sarima(log(x),1,1,1,1,0,0,12)
acf2(resid(a$fit))


a = sarima(log(x),1,1,1,1,0,0,12)
acf2(resid(a$fit))


sarima(log(x),2,1,1,1,0,0,12)

sarima(log(x),2,1,1,2,0,0,12)

sarima.for(log(x),12,0,1,1,1,0,0,12)

## a nice summary of the whole work flow for time series data analysis
## https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/