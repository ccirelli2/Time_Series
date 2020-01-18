install.packages("astsa")
library(astsa)

## plot the original series
par(mfrow=c(2,1),mar=c(2,2,0,2))
plot(soi,ylab="",xlab="",main="Southern Oscillation Index")
plot(rec,ylab="",xlab="",main="Recruitment")

## plot the acf
(r = round(acf(soi,6,plot=FALSE)$acf[-1],3))

par(mfrow=c(3,1))
acf(soi, 48, main="Southern Oscillation Index")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, 48, main="SOI vs Recruitment", ylab="CCF")

n = length(soi)
cor(rec[7:n],soi[1:(n-6)])

## MA smoother
par(mfrow=c(1,1),mar=c(4,4,2,2))
wgts = c(.5,rep(1,11),.5)/12
soif = filter(soi,sides=2,filter=wgts)
plot(soi)
lines(soif,lwd=2,col=4)
par(fig = c(.65,1,.65,1),new=TRUE)
nwgts = c(rep(0,20),wgts,rep(0,20))
plot(nwgts,type="l",ylim=c(-0.02,.1),xaxt="n",yaxt="n",ann=FALSE)


## kernel smoother
par(mfrow=c(1,1),mar=c(4,4,2,2))
plot(soi)
lines(ksmooth(time(soi),soi,"normal",bandwidth=1),lwd=2,col=4)
par(fig = c(.65,1,.65,1),new=TRUE)
gauss = function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}
x = seq(from = -3, to = 3, by = 0.001)
plot(x,gauss(x),type="l",ylim=c(-0.02,.45),xaxt="n",yaxt="n",ann=FALSE)

## compare to the output from decompose
soicomponents <- decompose(soi)
plot(soicomponents)
