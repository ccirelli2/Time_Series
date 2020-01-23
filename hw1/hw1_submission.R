# HOMEWORK 1 __________________________________________________________

' Question #1
  
  Consider a signal-plus-noise model of the general form 
  xt = st + wt, 
  where wt is Gaussian white noise with sigma square = 1. 
  Simulate and plot n= 200 observations from each of the following
  two models:


  Model 1:  st = 0 when t = 1..100
            st = 10*exp(-(1:100/20)*cos(2*pi*1:100/4))

  Model 2:  st = 0 when t = 1...100
            st = 10*exp(-(1:100)/200 * cos(2*pi*101:200/4))

  Compare the general appearance of the series from Model 1 & 2
  with the eath-quake series and the explosion series show in figure
  1.7 (from book)
'
# Clear Namespace
rm(list=ls())


# QUESTION #1 ------------------------------------------------------

## Model 1
n  <- 200
wt <- rnorm(200, 0,1)
st <- c(rep(0,100), 10*exp(-(1:100)/20) * cos(2*pi*1:100/4))
xt <- ts(st + wt)
plot(xt, main='Model 1')

' Model 1 Observations
  plot(10*exp(-(1:100)/20)):          Represents an exponential decay function. 
  plot(type="l", cos(2*pi*1:100/4)):  Represents repeating cos function between 0 and 100 intervals.
  Product:                            The product of the two would result in a decaying influence of the 
                                      cosine function from intervals 100-200, such that at one point (around 150), 
                                      it looks like the function returns to white noise remains. 
                                      I image this is analagous to what an earthquake looks like on a richter scale'

## Model 2
n  <- 200
wt <- rnorm(200, 0,1)
st <- (10 * exp(-(1:100 / 200))) * cos((2*pi*101:200) / 200)

# Plot st
plot(type= "l", ((10 * exp(-(1:100 / 200))) * cos((2*pi*101:200) / 200)))

# Plot xt
xt <- wt + st
plot(xt, type='l', main = 'Xt Plot')


' Model 2 Observations
  plot(type= "l", 10 * exp(-(1:100 / 200))):    Decaying exponential function, but less pronounced.  So the decay is more progressive. 
  plot(type= "l", cos((2*pi*101:200) / 200)):   This function looks like a sigmoid function that ranges from -1 to 1 over the ts 101:200
  plot(type= "l", ((10 * exp(-(1:100 / 200))) * cos((2*pi*101:200) / 200))):   
                                                Together we get a similar sigmoid function, but leff pronounced transition in the center. 
  
  Product:  Results in two progressive piques, one that grows from 0 to 100, then drops immediately and then grows again until 200. 
                                                              
'

' Comparison of Model 1 w/ Earthquake
  - They are very similar in the sense that both start out with 
    what looks like white noise, and then progress to something that
    looks cyclical in nature.  

  Comparison of Model 2 w/ Explosion
  - They are very similar in that both have two piques. That said, 
    the piques in Model two are much closer in time and I dont see the
    same type of cyclicality in model 2.  
'



# QUESTION #2 ---------------------------------------------------------------

' Consider a process consisting of a linear trend with an additive noise term
  consisting of independent random variable wt with zero mean and var sigma2

  xt = B0 + B1t + wt
  
  where B0 and B1 are fixed constants. 

'
# Clear Namespace
rm(list=ls())

# Linear function w/ white noise
n  <- 100
x  <- c(1: n)
B0 <- 0
B1 <- x^2 
wt <- rnorm(n, 0,1) * 200
y  <- ts(B0 + B1 + wt)
plot(x,y, type='l', main='Linear Function + White Noise')


# a.) Show that xt is nonstationary
' Tests
  1.) Is the mean constant over time?
  2.) Is the covariance constant over time?
  Note: we will use a moving average to see if the mu changes
        over time. 
'
library(pracma) #movavg()

# Calculate Autocovariance
acf(y, lag=10)
acf(y, lag=20)
'Observation:   It appears that the covariance is dependent on '

# Calculate the Moving Average
' mu test:      mu does not depend on t'
mv_avg <- movavg(y, n=10, type='s')
plot(mv_avg, type='l', main='Moving Average, Window=10')

# Split Data & Calculate Mean & Var
y0_10 <- y[0:10]
y11_20 <- y[90:100]

# Mean & Variance First Set of 10
mu_y0_10 <-  round(mean(y0_10),2)
var_y0_10 <- round(var(y0_10), 2)

# Mean & Variance of Second Set of 10
mu_y11_20 <- round(mean(y11_20), 2)
var_y11_20 <- round(var(y11_20), 2)

# Results
print(paste('Set1 mean & variance', mu_y0_10, var_y0_10))
print(paste('Set2 mean & variance', mu_y11_20, var_y11_20))
r <- c(mu_y0_10, mu_y11_20, var_y0_10, var_y11_20)
barplot(r, type='bar', names.arg= c('mu_set1', 'mu_set2', 'var_set1', 'var_set2'))

'Observations:  There is a clear different in the mu and var over time
                The auto covariance exhibits the trends associated with a nonstationary 
                time series
'

# Check Distribution
hist(y, type='h')  # definitly not gaussian


# Try taking a random sample of 





