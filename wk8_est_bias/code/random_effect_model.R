' Desc:  In class R code from wk8
  Diff-in-Diff:  What is this?
'

# Clear namespace ----------------------------------
rm(list=ls())

# Load Libraries -----------------------------------
require(foreign)
library('plm')
library("nlme")

# Load Data ----------------------------------------
jtrain= read.dta('/home/cc2/Desktop/repositories/Time_Series/wk8_est_bias/data/jtrain1.dta', convert.factors=FALSE)

# Random Effect Model ------------------------------
' Required Assumptions:  E(c) must be 0. 
  Approach:  Run both models and check if output is similar. 
'



# Remove Observations where scrapping rate is 'na'
jt = jtrain[!is.na(jtrain$scrap),]
T_ = 3              # how many observations for each firm or three yrs of data. 
N  = nrow(jt)/T_    # Total number of companies = num rows / 3
K  = 5              # Now 5 because we include the intercept. 


# Define variables
'rep(1,N) represents the intercept
 rep(1,N) returns a repetition of 1 N times where N = number of 
 rows divided by T_'
y  <- jt$lscrap
I88 <- (jt$year==1988)
I89 <- (jt$year==1989)
X   <- cbind(rep(1,N), I88, I89, jt$grant, jt$grant_1)

# Step1:  Estimate Omega Matrix using sigma_c^2 and sigma_v^2
' Omega:  Structure of the error term. 
  OLS:    Here we run OLS in order to get the estimates of the 
          residuals. 
'
beta_pols <- solve(t(X)%*%X)%*%(t(X)%*%y)
y_hat <- X%*%beta_pols
v_hat <- y - y_hat

' Reorganize v into matrix form.  First column for year 1, etc. 
'
v_mat <- matrix(v_hat, 3)
sigma2v <- sum(v_mat^2)/(N*T_ -K) # corr between error in yr1 and yr2
# Column 1*2 + Col1*Col3, etc. 
sigma2c <- sum(v_mat[1,]*v_mat[2,] + v_mat[1,]*v_mat[3,]+v_mat[2,]*v_mat[3,]) /
  (N*T_*(T_-1)/2-K)

Omega <- matrix(sigma2c, T_, T_) + diag(sigma2v-sigma2c, T_)
Omaga_inv <- solve(Omega)


# Results
' Check p-value to see if c^2 = 0
  if p-value is very small, then you reject your null-hypothesis and 
  c^2 is not equal to zero. 
  Different companies are having different productivity levels because the 
  c is fdifferent. 

  We need to compare the estimates from the random effect and fixed effect
  models.  
  
  You want to compare them relativ to their standard deviation. This will
  tell you how different they are. 
'



