require(astsa)

#################################
##
##  Rec vs SOI series
##
#################################

# Data Inspection ----------------------------------------------

# REC - Index of number of new fish over 453 months
rec
summary(rec)

# SOI - Southern Oscillation Index - The SOI measures the difference in surface air pressure between Tahiti and Darwin
summary(soi)

# Plot both on same slide
par(mfrow=c(2,1))
plot(rec)
plot(soi)

# Calculate Cross Covariance (produces ACF Plot) 
ccf(soi,rec)

# Cross Correlation
cor(rec[-(1:6)],soi[-(448:453)])
cor(soi[-(1:6)],rec[-(448:453)])
'Why is it different?'

plot(lag(soi,-6),rec)

dummy = ifelse(soi<0,0,1)


fish = ts.intersect(rec,soiL6 = lag(soi,-6),dL6 = lag(dummy,-6),dframe = TRUE)
a <- summary(fit <- lm(rec ~soiL6*dL6, data = fish))

soiL6 = lag(soi,-6)
dL6 = lag(dummy,-6)
points(soiL6[-(448:453)],fitted(fit),pch="+",col=2)


acf2(resid(fit))


sarima(rec,2,0,0,xreg=cbind(soiL6,dL6,soiL6*dL6))

out = arima(rec,c(2,0,0),xreg=cbind(soiL6,dL6,soiL6*dL6))
acf2(out$residuals)

out = arima(rec,c(2,0,0),seasonal=list(order=c(1,0,1),period=12),xreg=cbind(soiL6,dL6,soiL6*dL6))
acf2(out$residuals)

sarima(rec,2,0,0,1,0,1,S=12,xreg=cbind(soiL6,dL6,soiL6*dL6))

library(forecast)
train.id = 1:350
test.id = 351:453
?Arima
fit <- Arima(rec[train.id],c(2,0,0),xreg=cbind(soiL6,dL6,soiL6*dL6)[train.id,])
fit2 <- Arima(rec[test.id],c(2,0,0),xreg=cbind(soiL6,dL6,soiL6*dL6)[test.id,],model=fit)
onestep <- fitted(fit2)
plot(rec)
lines(time(rec)[test.id],as.vector(onestep),col="red")
mean((rec[test.id]-as.vector(onestep))^2)

## Arima function is different from arima
fit <- Arima(rec[train.id],c(2,0,0),seasonal=list(order=c(1,0,1),period=12),xreg=cbind(soiL6,dL6,soiL6*dL6)[train.id,])
fit2 <- Arima(rec[test.id],c(2,0,0),seasonal=list(order=c(1,0,1),period=12),xreg=cbind(soiL6,dL6,soiL6*dL6)[test.id,],model=fit)
onestep <- fitted(fit2)
plot(rec)
lines(time(rec)[test.id],as.vector(onestep),col="red")
mean((rec[test.id]-as.vector(onestep))^2)


## a nice summary of the whole work flow for time series data analysis

## https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/