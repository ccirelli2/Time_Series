'
Step1: Generate white noise


'
n <- 500
w <- rnorm(n)
x <- 0     #set initial value of x to zero
# Use for loop and regression model to calculate future values
for (i in 2:n){
  x[i] = 0.9*x[i-1]+w[i]
}


plot(x[1:100], type='l')  # Not a lot of crossings 
abline(h=0)

acf(x, lag=100, main='phi 0.9')    # Wee see correlation gradually decreasing. 


# We can change the coefficient to a smaller value
'We woudl expect to see a lot more crossings as it is less dependent on prior observations. 
In addition we woudl expect the correlation to decrease more quickly. '


for (i in 2:n){
  x[i] = 0.5*x[i-1]+w[i]
}

plot(x[1:100], type='l')  # Not a lot of crossings 
abline(h=0)

acf(x, lag=100, main='phi 0.9')    # Wee see correlation gradually decreasing. 


# Now Set phi equal to zero
for (i in 2:n){
  x[i] = 0*x[i-1]+w[i]
}

plot(x[1:100], type='l')  # Not a lot of crossings 
abline(h=0)
acf(x, lag=100, main='phi 0.9')    # Wee see correlation gradually decreasing. 

'Theoretically we would only see one value outside the blue lines, which is the first observation, 
and the rest within blue lines.  ***THis is the desired ACF plot for the residuals.  This is a signature
related to zero.   This means that Xt = wt.  Meaning there is nothing in the time series to improve upon.
The best prediction that we can have is mean for tomorrow. 
'

# What about when Phi is -0.9?
for (i in 2:n){
  x[i] = -0.9*x[i-1]+w[i]
}

plot(x[1:100], type='l')  # Not a lot of crossings 
abline(h=0)
acf(x, lag=100, main='phi 0.9')    # Wee see correlation gradually decreasing. 

'Why is this difference than phi = 0.9?  -0.9 * Xt will flip the the value of the next prediction. 
 So if you xt is positive, the next observation will be negative.  
 Also, it looks symetric becuase -0.9 is close to -1.  Expect to see a lot of crossings and symetric patter ,
 becuase if xt = 10, then xt+1 = -10. 
'


'When phi is one, we would expect all values in the ACF plot to be close to 1. 
 When the ACF plot decreases linearly, you always div cov(x,y) by the total sample size. 
 So when you move forward in time, the number of samples decreases so you would expect the
 ACF plot to decrease as you move forward.  So if it decreases linearly that does not mean the 
 cov is decreasing. 
'





'arima.sim  helps us to simulate a series. 
 order=c(2,0,0)  tells R that you want to generate a AR(2) model
 ar=c(1.5, -0.75) tells R that phi1 = 1.5 and phi2 = -0.75

 *** Everytime you see a sesonality in the acf plot
     1.) Could be seasonlity
     2.) When you solve for the polynomial function it could be because you are using 
         complex numbers that may give some seasonality to the chart. 
     See if the seasonality is within the blue lines, which means it is not significant. 
     Also we can plot the data to see if there is seasonlity. 

    Two blue lines tell us whether the estimated ACF is significant or not. 
     

ARMAacf:   used to calculate the theoretical acf, i.e. what should the acf for my model theoretially look like. 
           does it look similar to the one that you generated?
           do we see more noise in the data?
           **everytime you see the two blue lines you know it is estimated, these are our confidence intervals. 
           If no confidence intervals, its theoretical, calculated without approximation and without error. 


check causality
           just specify the coefficients z = c(1, -1.5, 0.75)    ** Always starts from the constant, then the coefficeint in front of B 
           and then in front of B2.   
           * the polyroot function tells us the solution polyroot(z)


'










