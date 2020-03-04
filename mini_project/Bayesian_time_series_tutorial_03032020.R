' Bayesian Time Series Tutorial
  Source:  https://www.youtube.com/watch?v=SW_gbFg1F2w&list=PLWHTeWZGJD5ZOYv3HFsUgQG0CnPRVycbu
  R implementation = https://www.youtube.com/watch?v=iGTXP55Ic-c&list=PLWHTeWZGJD5ZOYv3HFsUgQG0CnPRVycbu&index=5
  R package: BSTS'



' Structural Time Series Models:

  1.) State Space Model
      - Another name for a structure TS Model. 
      - Assume data comes from unobserved process state space. 
      - We attempt to model the unobserved state space. 
      
      i.  Local Level Model
          yt = observed data
          mut = unobserved state.
          yt = mt + et
          mt+1 = mt + et. 
          parameters are the mean and varaince of the error terms. 
          local lvl model is the simplest ts model.
          
          
      ii. Local Linear Trend Model
          Add a trend to the local level model. 
          yt, mt: same as before. 
          vt:     slope (additional state component)
          yt = mt + et
          mt+1 = mt + vt + et
          vt+1 = vt + st
          
          mu depends on the previous state of mt
          plus the slope. 
          
      iii. Seasonal Component
      
          mt: the local linear trend model we just discussed. 
          Tt: tau t is the seasonal component. 
          S:  dummy variable (1 for each season). 
          
          yt = mt + Tt + et
          Tt = Sigma Tt-s + wt
          
          mut is equivalent to the local level model. 
          Tt is the seasonal component. 
          We model Tt using dummy variables for each season. 
          Four parameters, one for measurable noise, one for level, 
          one for trend and one for seasonal, 
          
      iv. Final model
          mt: local linear trend
          Tt: seasonal component
          BTxt: regression component. Looks like matrix notation
          
          yt = mt + Tt + Btxt + et
          
          
    Bayesian Model without Regression
      i. Prior Distribution:
      
      ii. Posterior Distribution:
    
    
    Bayesian Model w/ Regress:
      i. Use a Spike and Slab prior:
         - spike: prior for probability of regression coefficients
                  being zero. 
         - slab:  prior on nonzero regression coefficients and variance. 
          
      ii. Spike:            


'


