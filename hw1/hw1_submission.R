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
# Clear Namespace & Plots
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# Linear function w/ white noise
' Explanation:  I create an exponential trend over 100 observations 
                with white noise = normal distribution * a magnitude of 200. 
'
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
'Observation:   
  - Irrespective of the lag time of 10 or 20 there is a high autocorrelation between 
    our x values. '

# Calculate the Moving Average
' Observation:
  - The mu is changing over time, i.e. it is not constant. '
mv_avg <- movavg(y, n=10, type='s')
plot(mv_avg, type='l', main='Moving Average, Window=10')

## Manually Split Data & Calculate Mean & Variance @ Two Distinct Time Periods

# Split Data Into Two Time Intervals
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

# Check Distribution of Y Values
hist(y, type='h')  # definitly not gaussian

'Observations:  There is a clear difference in the mu and variance of this time series 
                between these two time intervals. 

Conclusion:     Based on the covariance & mean tests this time series is not stationary. 
'


## QUESTION #3 -------------------------------------------------------------
' Consider the two weekly time series oil and gas (astsa package).
  The oil series is in dollars per barrel, while the gas series is in cents
  per gallon. 
  Use R codes ?oil and ?gas to get the help file on the two datasets. 

  a.) Plot the data on the same graph. Do you believe the data is stationary?
  b.) In economics, we often use the percentage of change in priar (termed 
      growth rate or return), rather than absolute price change. 
      Use a transformation of the form yt = Delta Log(xt) = log(xt) - log(xt-1)
      to the data, where xt is the oil or gas price series. 
      Plot the data on the same graph, look at the sample ACFs of the transformed
      data and comment. 
'
# Clear Namespace & Plots
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# Load Libraries
library(astsa)

# Load Data
data(oil)
data(gas)

# Plot Data w/ Two Diff Scales
plot(oil, main = 'Oil & Gas')
lines(gas)

# Standardize Data
oil_mu  = mean(oil)
oil_sd  = sd(oil)
gas_mu  = mean(gas)
gas_sd  = sd(gas)

oil_st  = (oil - oil_mu) / oil_sd
gas_st  = (gas - gas_mu) / gas_sd

# Plot Standardized Data On Same Plot
plot(oil_st, main = 'Oil & Gas Standardized')
lines(gas_st)     

' Observations:  The are almost identical
'
# Get Summary Statistics
summary(oil_st)
summary(gas_st)
' Observations:  Summary statistics are almost identical as well'

# Do You Believe the Data is Stationary?
oil_acf = acf(oil_st)
gas_acf = acf(gas_st)
' Answer:  The acf plots indicate that both oil and gas has very high autocovariance
           between the x values over time'

# Take the Log of x & Diff of xt - xt-1 & Plot
'Diff:  default lag=1'
oil_log = log(oil)
oil_diff = diff(oil_log)
plot(oil_diff, main='Log Oil + Diff')
acf(oil_diff)
' Observations:
  1.) The plot shows a stationary time series where only the first line shows autocorrelation (because its corr(x1,x1), 
      and only a few others go over the blue lines. 
'



## QUESTION #4 ---------------------------------------------------------
' For a moving average process of the form xt = wt-1 + 2wt + wt+1, 
  where wt are iid, determine the autocovariance and autocorrelation 
  functions as a function of lag h = s-t and draw the ACF as a function of h
'
# Generate Data
n <- 100
w = rnorm(n, mean=0, sd=1)
x <- 0
for (i in 2:n){
  x[i] <- w[i-1] + 2*w[i] + w[i + 1]
}

# Plot Results
plot(x[1:100], type='l', main='Plot MA Model')
# Plot ACF
acf(x[1:90])


