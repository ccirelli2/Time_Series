# Homework 4 - Question 3
' Ref: https://rstudio-pubs-static.s3.amazonaws.com/11785_e937a5f782864e1c9053d90b2b66c796.html
  Ref: https://data.library.virginia.edu/getting-started-with-hurdle-models/
 https://tysonbarrett.com/Rstats/chapter-5-generalized-linear-models.html
  Fit the following four models for the Affairs data
  a.) Poisson, 
  b.) Negative Binomial: glm.nb(dependend ~., data=data)
  c.) Hurdle Poisson: hurdle(formula, data, link=logit)
  d.) Hurdle Negative Binomial: 
  Discuss results comparing Log likelihood, AIC, 
  Prediction vs Actual, rootgram. 

'

# Clear namespace -------------------------------------
rm(list=ls())

# Load Packages ---------------------------------------
library(AER)
library(MASS)
library(dplyr)
library(pscl)

# Load data -------------------------------------------
data("Affairs")

# Inspect Data ----------------------------------------
' y = affairs
'
?Affairs
names(Affairs)
hist(Affairs$affairs)
summary(Affairs)
Affairs %>% group_by(gender) %>% summarise(affairs=sum(affairs))

affairs.mu <- mean(Affairs$affairs)
affairs.var <- var(Affairs$affairs)
if affairs.var > affair.mu:
  print('Variance is greater than mean')


# Fit Poisson Model ----------------------------------
' Results:
    Residual Deviance:  2359 
    AIC:                2871
    Regressors:       Age, yearsmarried, religiousness, occupation and rating are all significant based
                      on the p-value. 
'
m.poisson <- glm(affairs ~., data=Affairs, 
                 family=poisson(link='log'))

summary(m.poisson)


# Fit Negative Binomial Regression -------------------
' Results:
    Residual Deviance:  339 (substantially less than the poisson model)
    AIC:                1476 (almost half the poisson model)
    Log-likelihood:     -1456
    Dispersion:         less than 1. 
    Regressors:         only yearsmarried, religiousness and rating are significant based on p-value. 
    Overall:            seems to be a better model based on the Residual erro and AIC scores and that the model
                        does not assume mu = var. 
'

m.nb <- glm.nb(affairs ~., data=Affairs)
summary(m.nb)

# Fit Hurdle Model -----------------------------------
' Results:
    Model:            Fit both truncated poisson with log link and binomial with logit link. 
    Log-likelihood:   -758.8
    Overall:          Appears to be a better fit based on teh log-likelihood value. 
    

'
m.hurdle.poisson <- hurdle(affairs~., data=Affairs, dist='poisson')

summary(m.hurdle.poisson)



