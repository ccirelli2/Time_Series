require("astsa")

###########################################################
##                                                       ##
## analysis of the GNP data with model diagnostics       ##
##                                                       ##
###########################################################


plot(gnp)
acf(gnp)

plot(diff(log(gnp))) ## growth rate
acf2(diff(log(gnp)))

out = ar(diff(log(gnp)),1) ## AR(1)
acf2(out$resid)

arima(log(gnp),c(1,1,0)) ## AR(1)

arima(log(gnp),c(0,1,2)) ## MA(2)


## diagnostic

sarima(log(gnp),1,1,0) ## AR(1)

sarima(log(gnp),0,1,2) ## MA(2)


## which model to choose?

ARMAtoMA(ar=.35,ma=0,10)


