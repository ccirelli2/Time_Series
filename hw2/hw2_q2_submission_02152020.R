' Question #2:
  Let xt represent the cardiovascular mortality series (cmort in astsa package)
'

# Clear Namespace
rm(list=ls())
dev.off(dev.list())

# Import Libraries
library(astsa)

# Show All Datasets
data(package = .packages(all.available = TRUE))

# Load Cardiovascular Dataset
cmort

# Question 2(b)
'Plot the data and ACF, PACF.  Comment on the stationarity and discuss which model you would use'
plot(cmort, main='Mortality Rate')

' Stationarity:  The data does not appear to be stationry.  The mean appears to decline over time "trend" and 
                 there appears to be yearly seaonslity
'
cmort1  = cmort[0: 100]
cmort2  = cmort[408 : 508]
cmort1_mu = mean(cmort1)
cmort2_mu = mean(cmort2)
mu1_2 = c(cmort1_mu, cmort2_mu)
barplot(mu1_2, main='Barplot Mu1 and Mu2')    # Proof mu is not constant. 
monthplot(cmort, main='Mortality Data - Mean')

'Plot ACF & PACF'
par(mfrow=c(2,1))
acf(cmort)
pacf(cmort)
acf2(cmort)


'Observations:  The ACF and PACF plots indicate that this is an AR model.  In the case of the ACF, 
                the phi value progressively decreases from 1 as the lag increases.  If this were an MA 
                model we woudl see a single bar and then nothing. 
                In the case of the PACF plot, we see two bars and then minimal ones thereafter.  If this 
                were an MA model we would see progressively declining bars as is the case with the ACF plot.'



# b.) Fit an AR(2) model to xt using the method of moments Yule Walker equation 
cmort.diff <- diff(log(cmort))
ar.yw <- ar.yw(cmort.diff, order=2)
out = acf(cmort.diff)
rho = out$acf[1:5]
G2 = matrix(c(rho[1],rho[2],rho[2],rho[1]),2)
g2 = c(rho[2],rho[3])
solve(G2)%*%g2


