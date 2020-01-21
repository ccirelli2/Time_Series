# Clearn Namespace
rm(list=ls())

# Load Libraries
'Note:  This package is associated with our book Time Series Analysis and Its Applications'
library(astsa)


## plot the original series
' Soi:  Southern Oscillation Index for a period of 453 months 1950-1987
  rec:  recruitment number of new fish for periodss 435 months 1950-1987
'

par(mfrow=c(2,1),mar=c(2,2,0,2))
plot(soi,ylab="",xlab="",main="Southern Oscillation Index")
plot(rec,ylab="",xlab="",main="Recruitment")

## ACF
(r = round(acf(soi,6,plot=FALSE)$acf[-1],3))

# PLOT ACF
par(mfrow=c(3,1))
acf(soi, 48, main="Southern Oscillation Index")
acf(rec, 48, main="Recruitment")
ccf(soi, rec, 48, main="SOI vs Recruitment", ylab="CCF")

n = length(soi)
cor(rec[7:n],soi[1:(n-6)])

## MA smoother
'mar:   c(bottom, left, top, right) which specify the margins on each side of the plot
 mfrow: number of rows
 reP:   replicate eliements of a vector
 Filter:  reference = https://online.stat.psu.edu/stat510/lesson/5/5.2
          The term filter is sometimes used to describe a smoothing procedure. For instance, 
          if the smoothed value for a particular time is calculated as a linear combination of observations 
          for surrounding times, it might be said that we’ve applied a linear filter to the data 
          (not the same as saying the result is a straight line, by the way).
          filter(series, sides= either one side or two, filter = wieghts to use)
          so I guess we can make an arbitrary filter that is applied to one or both sides of the target value in the time series. 
          see documentation at online.stat
'

par(mfrow=c(1,1),mar=c(4,4,2,2))
wgts = c(.5,rep(1,11),.5)/12  # filter of length 13 applied to both sides of series
soif = filter(soi,sides=2,filter=wgts)

plot(soi)
lines(soif,lwd=2,col=4)
par(fig = c(.65,1,.65,1),new=TRUE)
nwgts = c(rep(0,20),wgts,rep(0,20))  # creates 0 padding on each side of wgts
plot(nwgts,type="l",ylim=c(-0.02,.1),xaxt="n",yaxt="n",ann=FALSE)   # ylim - yaxis limit


## kernel smoother
' Kernel Regression Smoother - ksmooth
  Note:  This looks like the smooth technique that is used with fitting a regression line
  time:  creates the vector of times at which a time series was sampled
'
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






