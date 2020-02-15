## analysis of rec series using Yule-Walker esitmation and MLE.
rm(list=ls())
require("astsa")
?rec
acf(rec,lag=48)
?acf2
acf2(rec, 48) # will produce values and a graphic


####### Yule-walker estimation
?ar
rec.yw = ar.yw(rec,order=2)
rec.yw$x.mean
rec.yw$ar # coefficients
rec.yw$asy.var.coef ## covariance matrix

## prediction
rec.pr = predict(rec.yw,n.ahead = 24)
ts.plot(rec,rec.pr$pred,col=1:2)
lines(rec.pr$pred+rec.pr$se,col=4,lty=2)
lines(rec.pr$pred-rec.pr$se,col=4,lty=2)

### MLE
rec.mle = ar.mle(rec,order=2)
rec.mle$x.mean
rec.mle$ar
rec.mle$asy.var.coef



## compare that to the theoretical ACF and PACF of an AR(2) process
ACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24)[-1]
PACF = ARMAacf(ar=c(1.5,-.75), ma=0, 24, pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0)
plot(PACF, type="h", xlab="lag", ylim=c(-.8,1)); abline(h=0)


