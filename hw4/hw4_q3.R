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


# Fit Poisson Model ----------------------------------
m.poisson <- glm(affairs ~., data=Affairs, 
                 family=poisson(link='log'))

summary(m.poisson)


# Fit Negative Binomial Regression -------------------
m.nb <- glm.nb(affairs ~., data=Affairs)

summary(m.nb)

# Fit Hurdle Model -----------------------------------
m.hurdle.poisson <- hurdle(affairs~., data=Affairs, dist='poisson')
m.hurdle.



