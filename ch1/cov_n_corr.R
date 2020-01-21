'Description:   The purpose of this script is just to get a better intuition for 
                covariance and correlation. 
'
# Clear Namespace
rm(list=ls())

# Load Data
data(ChickWeight)
weight <- ChickWeight$weight
age    <- ChickWeight$Time

# Plot Data w/ Regression Line
fit <- lm(weight~age, data=ChickWeight)
plot(age, weight, main='Chicken Weight vs Age')
lines(age, fitted(fit), col='blue')

# Calculate Covariance
mu_w <- mean(weight)
mu_a <- mean(age)
n    <- length(weight)
cov_w_a  <- sum((weight - mu_w) * (age - mu_a)) / n

# Calculate Correlation
var_w <- var(weight)
var_a <- var(age)
corr_w_a <- cov_w_a / sqrt(var_w * var_a)

corr_w_a     # So there is a positive 83% correlation between age and weight of chickens


  