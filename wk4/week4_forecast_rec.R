require("astsa")

###########################################################
##                                                       ##
## estimation and prediction for the recruitment series  ##
##                                                       ##
###########################################################
plot(rec)
acf2(rec)
?acf2
####### Yule-walker estimation
rec.yw = ar.yw(rec,order=2)
rec.yw$x.mean
rec.yw$ar # coefficients
rec.yw$asy.var.coef ## covariance matrix

### MLE
rec.mle = ar.mle(rec,order=2)
rec.mle$x.mean
rec.mle$ar
rec.mle$asy.var.coef

fore.mle = predict(rec.mle, n.ahead=24)
ts.plot(rec,fore.mle$pred,col=1:2,xlim=c(1980,1990),ylab="recruitment")

U = fore.mle$pred+fore.mle$se 
L = fore.mle$pred-fore.mle$se
xx = c(time(U),rev(time(U)))
yy = c(L,rev(U))
polygon(xx,yy,border=8,col=gray(.6,alpha=.2))
lines(fore.mle$pred,type="p",col=2)

abline(h = 62.26,col=gray(.6))
fore.mle$se
c(fore.mle$se)

## ols
regr = ar.ols(rec,order=2,demean=FALSE, intercept=TRUE)

fore = predict(regr, n.ahead=24)
ts.plot(rec,fore$pred,col=1:2,xlim=c(1980,1990),ylab="recruitment")

U = fore$pred+fore$se; L = fore$pred-fore$se
xx = c(time(U),rev(time(U)))
yy = c(L,rev(U))
polygon(xx,yy,border=8,col=gray(.6,alpha=.2))
lines(fore$pred,type="p",col=2)

abline(h = 62.26,col=gray(.6))
fore$se
c(fore$se)



