' Desc:  In class R code from wk8
  Diff-in-Diff:  What is this?
  
  
  Ref:  
  https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html
'

# Clear namespace ----------------------------------
rm(list=ls())

# Load Libraries -----------------------------------
require(foreign)
library('plm')
library("nlme")

# Load Data ----------------------------------------
jtrain= read.dta('/home/cc2/Desktop/repositories/Time_Series/wk8_est_bias/data/jtrain1.dta', convert.factors=FALSE)

# Fixed Effect Model -------------------------------
' Obj: Study the effects of job training grants on scrapping rate.
       Scraping:  how effectively they are producing their product. 
       If qpuality is high, scraping will be low

log(scrap)it = B0 + B1(1988) + B2(1989) + B3union + B4grant + B5 grant + 
                ci + uit

    B0: Intercept
    B1: Observation of scrapping rate from yr 1988
    B2: Observation of scrapping rate from yr 1989 
    B3: Union ?
    B4: Whether the factory has received a retraining grant for 1998
    B5: Same
    ci: Unknown constant parameter.  Represents possible correlation with x. 
'

# Inspect data -------------------------------------
' Note: that this is in the structure of panel data. 
        We have multiple subjects with data over multiple time periods. 
        Here you will need to combine the fcode and year to get the unique
        value for the observed factory and year. 
  Regression:  Scrape rate on (y variable) grant nad years d89 and d88

  Intercept:  If it is a fixed effect model we do not want to include the
        intercept.  Why?  because it is a variable that does not change overtime. 
        We dont want to include variables that are constant overtime because
        we already have this ci part, which does not change over time and we
        are trying to measure.  Union is the same, does not change over time, 
        so exclude it. 

  Variables:  Whether the year is 1988, 1989 and grant1 and grant2. 

  Scraping Rate:  There are a lot of NAs.  You need to exclude those 
        from your dataset and only include those with some lvl of scrapping
        rate. 

'
col_names = colnames(jtrain)
head(jtrain)

# Remove Observations where scrapping rate is 'na'
jt = jtrain[!is.na(jtrain$scrap),]
T_ = 3              # how many observations for each firm or three yrs of data. 
N  = nrow(jt)/T_    # Total number of companies = num rows / 3
K  = 4              # num of explanatory variables 


' Fixed Effect Model: 
  - We assume that the expectation is not equal to zero, i.e. ci and xi are
    correlated. 
  How do we construct a fixed effect model?
  - Construct two functions. Between equation and within equation. 
  
  Within equation:
  - For every y and x I remove the mean from it. 
  - **With a demeaned y and x we can just run the ordinary linear regression. 
 
'

# Define variables
y  <- jt$lscrap
x1 <- (jt$year==1998)
x2 <- (jt$year==1989)
x4 <- jt$grant
x5 <- jt$grant_1
X  <- cbind(x1, x2, x4, x5)

# Function to remove mean
dmean <- function(y, num_obs){
  'transform y into a matrix w/ three rows for each 
   observation.  Then apply the mean on each column'
  ybar <- apply(matrix(y, num_obs), 2, mean)
  ybar_long <- rep(ybar, each=T)
  y_dm <- y - ybar_long
  return (y_dm)
}


# Get De-Meaned version for Y and X
' Here we pass the column values for 
  each observation and return the mean'
y_dm  <- dmean(y, T_)
x1_dm <- dmean(x1, T_)
x2_dm <- dmean(x2, T_)
x4_dm <- dmean(x4, T_)
x5_dm <- dmean(x5, T_)
X_dm  <- cbind(x1_dm, x2_dm, x4_dm, x5_dm)

# Solve 
' %*% is used for matrix multiplication. A %*% B is the matrix
  product of matrices A and B. 
  t(m): function to transpose a matrix. 
'
beta_fe <- solve(t(X_dm)%*%X_dm)%*%(t(X_dm)%*%y_dm)




# Now use R functions to run fixed-effect model ----------------
' Use ordinary linear regression function lm()
  Include factor variables factor(fcode).  Sounds like dummy
  binary variables to indicate whether the data comes from 
  each fcode. 
  -1:  Indicates that you do not want to include the intercept. 

  Summary/results
  -  We have each of our coefficients d88, d90, grant, grant_1, and
  -  We have coefficients for each form level, which corresponds to 
     a factory.
  
  Interpretation:
  Ex:  factor(fcode)410523: Estimate -2.8, p value is significant. 
       It means that for this firm you would exect to see a scraping
       rate of -2.8 less than the average of the dataset. 

  Model:  if we see that almost all estimates of beta coefficients are
        close to zero, that means there are no firm level differences and 
        ** there is no firm level heterogeneity.  So all c values are
        close to zero. 
        
        If you see a lot of significant coefficients, which model should you use?
        - You should run fixed effect and not regular OLS as there is the presence
          of the ci variable. 
'
fit.fe <- lm(lscrap ~ d88+d89+grant+grant_1 + factor(fcode)-1, data=jt)
summary(fit.fe)


# Using Panel Linear Model For Regression -----------------------

# Within - Help us estimate the coefficients
fixed <- plm(lscrap ~ d88+d89+grant+grant_1, data=jt, 
             index=c("year", "fcode"), model="within")
summary(fixed)

# Between - Help us estimate all of the firm lvl fixed effects 
fixed <- plm(lscrap ~ d88+d89+grant+grant_1, data=jt, 
             index=c("year", "fcode"), model="between")
summary(fixed)

