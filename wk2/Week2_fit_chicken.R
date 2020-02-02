install.packages("astsa")
library(astsa)

par(mfrow=c(1,1),mar = c(4,4,2,2))
summary(fit <- lm(chicken~time(chicken),na.action=NULL))

##################################
##
##  Step 1: Detrend and remove seasonality
##
##################################


## plot the original series.
plot(chicken,ylab = "cents per pound",col="blue",lwd=2)
abline(fit)

## plot the detrended sereis.
par(mfrow=c(2,1))
plot(resid(fit),type="l",main="detrended")
plot(diff(chicken),type="l",main="first difference")

## plot the acf and pacf
ggseasonplot(diff(chicken), year.labels=TRUE, year.labels.left=TRUE) 
acf(diff(chicken),48,main="")

par(mfrow=c(3,1),mar=c(2,4,2,2))
acf(chicken,48,main="");title("chicken", line = 0.5)

acf(resid(fit),48,main="");title("detrended", line = 0.5)
pacf(resid(fit),48,main="");title("detrended", line = 0.5)

acf(diff(chicken),48,main="");title("first difference", line = 0.5)
pacf(diff(chicken),48,main="");title("first difference", line = 0.5)

#season <- as.factor(round(time(diff(chicken))-floor(time(diff(chicken))),2))
#fit1 = lm(diff(chicken)~season)
#season_trend <- fit1$fitted.values

a <- decompose(diff(chicken))
season_trend <- a$seasonal
plot.ts(season_trend)

price <- diff(chicken)-season_trend

par(mfrow=c(3,1))
plot.ts(diff(chicken),ylab="",main="detrended chicken price")
plot.ts(season_trend,ylab="",main="seasonal component")
plot(price,ylab="",main="remainder")

par(mfrow=c(2,1))
acf(price,48,main="chicken price with seaonality and trend removed")
pacf(price,48,main="chicken price with seaonality and trend removed")

# which one is better?
tseries::adf.test(chicken)
tseries::adf.test(resid(fit))
tseries::adf.test(diff(chicken))

#################################
##
## part 2: estimation Fit an AR(2) model
##
####################################

## Yule-Walker
chick.ar <- ar(price, order.max = 2)

plot(chick.ar$resid)

par(mfrow=c(2,1))
acf(chick.ar$resid[-(1:2)],48,main=expression(paste("residual: ",w_t)))
pacf(chick.ar$resid[-(1:2)],48,main=expression(paste("residual: ",w_t)))



## mle
ar(price, order.max = 2, method="mle")

# linear regression?

n=length(price)
lm(price[-(1:2)]~price[1:(n-2)]+price[2:(n-1)]-1)

ar(price, order.max = 2, method="ols")

