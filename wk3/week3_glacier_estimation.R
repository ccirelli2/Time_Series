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

acf2(diff((varve)))

arima(diff(log(varve)),c(0,0,1))

arima(log(varve),c(0,1,1))

###########################################################
##                                                       ##
## analysis of the GNP data                              ##
##                                                       ##
###########################################################


plot(gnp)
acf(gnp)
plot(diff(gnp))

plot(diff(log(gnp))) ## growth rate
acf2(diff(log(gnp)))

arima(log(gnp),c(1,1,0)) ## AR(1)

arima(log(gnp),c(0,1,2)) ## MA(2)

## which model to choose?