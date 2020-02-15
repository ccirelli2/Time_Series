require("astsa")

###########################################################
##                                                       ##
## analysis of the glacial varve series                  ##
##                                                       ##
###########################################################
par(mfrow=c(2,1))
plot(varve)

plot(log(varve))

plot(diff(log(varve)))

acf2(diff(log(varve)))

sarima(diff(log(varve)),0,0,1)

## what does the Ljung-Box plot say?

out = arima(diff(log(varve)),c(0,0,1))
acf2(out$residuals)

sarima(diff(log(varve)),0,0,1,no.constant = TRUE)
sarima(diff(log(varve)),1,0,1,no.constant = TRUE)


