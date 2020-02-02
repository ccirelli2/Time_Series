require("astsa")

###########################################################
##                                                       ##
## simulate data from an AR(2) model                     ##
##                                                       ##
###########################################################


set.seed(8675309)
ar2 = arima.sim(list(order=c(2,0,0),ar=c(1.5,-.75)),n=144)
plot(ar2)

ar2 = arima.sim(list(order=c(2,0,0),ar=c(1,-.25)),n=144)
plot(ar2)

acf2(ar2)

## to calculate the theoretical ACF and PACF for this model
ACF = ARMAacf(ar=c(1.5,-.75),ma=0,50)
plot(ACF,type="h",xlab="lag")
abline(h=0)

PACF = ARMAacf(ar=c(1.5,-.75),ma=0,50,pacf=TRUE)
plot(PACF,type="h",xlab="lag")
abline(h=0)

###########################################################
##                                                       ##
## check causality                                       ##
##                                                       ##
###########################################################

z = c(1,-1.5,.75)
(a = polyroot(z))

z = c(1,-1.37,.58)
(a = polyroot(z))

###########################################################
##                                                       ##
## Estimation and CI                                     ##
##                                                       ##
###########################################################

gamma0_hat = sum((ar2-mean(ar2))^2)/144 
gamma1_hat = sum((ar2[-1]-mean(ar2))*(ar2[-144]-mean(ar2)))/144 
gamma2_hat = sum((ar2[-(1:2)]-mean(ar2))*(ar2[-(143:144)]-mean(ar2)))/144 
rho1_hat = gamma1_hat/gamma0_hat
rho2_hat = gamma2_hat/gamma0_hat

R = matrix(c(1,rho1_hat,rho1_hat,1),2)
b = c(rho1_hat,rho2_hat)
phi_hat = solve(R)%*%b
phi_hat

sigma2_w = as.numeric(gamma0_hat*(1-t(b)%*%phi_hat))
sigma2_w

Gamma = R*gamma0_hat
sigma2_w/144*solve(Gamma)
