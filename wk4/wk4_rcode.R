ar.sim <- arima.sim(model=list(ar=c(.9, - .2)), n=100)

# Take polinomial
z = c(1,-.9, .2)
polyroot(z)
# Shows that roots are > 1.  So we are good. 

# First way
# predict x3 using x1 and x2
# This is the first way that we can make a prediction, where phi = Gamma_n^1 * gamma_n
# THis works for any data, without knowing anything about the data. 
out = acf(ar.sim)    # take acf of simulated data. 
rho = out$acf[1:5]   # First 5 lines in acf plot.
rho  # note how rho[0] is = 1. 
G2 = matrix(c(rho[1], rho[2], rho[2], rho[1]), 2)    # construct a gamma matrix
g2 = c(rho[2], rho[3])   # i guess this is phi
solve(G2)%*%g2   # this is the product of our gamma matrix versus vector.  
                 # so this is how we calculate our coefficients. 

# We use this matrix vector multiplication to estimate our phi's. 



## Second way to make a prediction
'
We make use of the fact that it is an AR(2) model 
use the fact that the model is an AR(2)
model estination to get phi_1 and phi_2
use those phis to predict x3. 

YUler-Walker Equations - estimate using method of moments. Use to get estimate of phi1 and phi2. 


parameters are equal to gama2 inverse times gamma 2. or gamma p inverse times gamma p. 

If your phis have two subscripts, 


General form equation makes no assumption about data. 
We simply use the gamma matrix times gamma to calculate phis. 


Note that the third coefficient is almost zero. 

** solve(G3) gives us the inverse of the matrix. 

Note how she retrieves the gamme values from the gamma matrix. 

If you know the model, say its AR(2) you dont need to test if xt1 is diff from zero.
you just drop it from the equation. 

CHose model based on ACF and PACF. How does the ACF / PACF tell us which model we should use?
If it has a cut off, then its a MA model. 

*** the AR and MA ACF plots are inverses of one another.  with AR model the PACF it starts at 
h = 1 and cuts off at p.   this is the same for the MA ACF plot. 
Conversley the MA PACF is a slow declining curve. 



'








