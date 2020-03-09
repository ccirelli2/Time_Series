' Bayesian Structural Time Series - Implementation

  Source:  https://www.youtube.com/watch?v=pPO5av4HD90
  Bayesian Regression 
           yt = Mut + Tt + B^Txt + Et

  Unique aspect is the dot product between Beta and x. BSTS estimates the
  value of the Beta. 
  Spike & Slab Regression:  
      Source: http://www.batisengul.co.uk/post/spike-and-slab-bayesian-linear-regression-with-variable-selection/
      Bayesian variable section technique that is
      useful when the number of possible predictors is larger than the
      number of observations. 
      
      The spike and slab regers to a prior distribution on the mean
      terms for each beta.   
      Spike and Slab is a shrinkage method, much like ridge and 
      lasso regression, in the sense that it shrinks the weak beta
      values from the regression toward zero

      Spike: because the distribution spikes at zero. 
      Slab:  shrinks all other values in the distribution on either size of
             0

  Data:  10 different time series grouped by 10 meters of sea water
  Regression: Y= sea surface temperature
              X= underlying layers as input. 
  Single Var: We should try a single regression model. 
'

# Clear Namespace --------------------------------------
rm(list=ls())

# Import Libraries -------------------------------------
library(readr)
library(bsts)
library(oce)
library(ggplot2)
library(astsa)
library(astsa)

# Import Data ------------------------------------------
' STT:  Is our Target
  10m:  Each column represents one of our independent variables
        So 10m equals the temperature at 10 meters. 
  Drop: Time Index, which is just enumerates the dates'
setwd('/home/ccirelli2/Desktop/repositories/Time_Series/mini_project')
data <- read_csv('data.csv')

data$SST <- data$`0`
data$timeIdx <- NULL
data$`0` <- NULL
data$year <- substring(data$startDate, 1,4)

# Get Summary Statistics
summary(data)


# Plot 1 - All
ggplot(data, aes(data, x= data$startDate)) + 
  geom_line(aes(y=data$`10`), colour='red') +
  geom_line(aes(y=data$`20`), colour='green') + 
  geom_line(aes(y=data$`30`), colour='blue') + 
  geom_line(aes(y=data$`40`), colour='black') + 
  geom_line(aes(y=data$`50`), colour='purple') + 
  geom_line(aes(y=data$`60`), colour='yellow') + 
  geom_line(aes(y=data$`70`), colour='black') + 
  geom_line(aes(y=data$`80`), colour='grey') + 
  geom_line(aes(y=data$`90`), colour='red')  + 
  ylab('Temperature') + xlab('Dates') + ggtitle('TS - Gibralter Sea Surface Temperatures - By lvl') 

# Plot 2 - By Year
ggplot(data, aes(data, x= data$startDate)) + 
  facet_wrap(~ data$year) + 
  geom_line(aes(y=data$`10`), colour='red') +
  geom_line(aes(y=data$`20`), colour='green') + 
  geom_line(aes(y=data$`30`), colour='blue') + 
  geom_line(aes(y=data$`40`), colour='black') + 
  geom_line(aes(y=data$`50`), colour='purple') + 
  geom_line(aes(y=data$`60`), colour='yellow') + 
  geom_line(aes(y=data$`70`), colour='black') + 
  geom_line(aes(y=data$`80`), colour='grey') + 
  geom_line(aes(y=data$`90`), colour='red')  + 
  ylab('Temperature') + xlab('Dates') + ggtitle('TS - Gibralter Sea Surface Temperatures - By Yr') 

# Other Plots
ggplot(data, aes(x = year, y = `10`)) + geom_boxplot() + ggtitle('Box Plot Lvl 10')
ggplot(data, aes(x = data$startDate, y = `10`)) + geom_area() + ggtitle('Area Plot Lvl 10')
plot(x=data$startDate, y=data$`10`, type='l', main='TS - Gibralter Sea Surface Temperature - Layer 10')

# Check Stationarity (Constant Mean)
monthplot(data$`10`, main='TS - "10" - Mean by Month')

# ACF & PACF - Lvl '10'
acf2(data$`10`)
acf2(data$`20`)

# Observations
' Clearly there is an annual cyclicality to this data set. 
  
'

# Drop Unecessary Columns
' Slice:  -1 drops first column of dataframe'
data$startDate <- NULL
data$year <- NULL
names(data) = c("10m", "20m", "30m",
                "40m", "50m", "60m", 
                "70m", "80m", "90m", "SST")

# Tran / Test Split
nrows <- NROW(data)
ntrain <- floor(nrows*0.7)
ntest  <- nrows - ntrain
ts_train <- data[1:ntrain,]  # all columns, rows = 1: number of training instances
ts_test <- data[111 : nrows, ]



# Fit Local Linear, Seasonal, and One Linear Component Model
' nseasons:   Note that we should later find a way to calculate this
              explicitly
  bsts:       local linear seasonal and one linear component model. 
  ping:       print function
  model.size: sets our spike and slab parameters to one. We are expecting
              one depth to materially affect sea surface temperature. 
'
nseasons <- 11
ss <- list()
ss <- AddLocalLinearTrend(ss, y= ts_train$SST)

ss <- AddSeasonal(ss, ts_train$SST, nseasons = nseasons) 
model1 <- bsts(ts_train$SST ~ ., state.specification = ss, 
              data=ts_train, niter=1000, ping=0, 
              expected.model.size=1)
  
# Plot model
plot(model1, 'components', xlab='Date idx []', ylab='SST [deg C]')


# Plot Coefficients
' To see which depth is significant
  White bars represent positive betas, black negative betas. 
'
plot(model1, 'coefficients', main='Coefficients')


# Generate Prediction
' Horizon:  Used '
model1_pred <- predict(model1, newdata=ts_test, horizon=28)

# Change Size of Plot to fit forecast
par("mar")
par(mar=c(1,1,1,1))

# Plot Prediction
' Note:  The plot includes the confidence intervals, which 
         appear to be the dashed green line
         We should familiarize ourselves with each component of 
         the model'
plot(model1_pred, plot.original=90, 
     main='Seasonal', 
     xlab='Date idx []', 
     ylab='SST [deg C]')

plot(model1_pred$mean, type='l', main='Mean / Seasonality')
hist(model1_pred$distribution, main='Prior Distribution')


# Model 2 - Two Parameters ---------------------------------------
model2 <- bsts(ts_train$SST ~ ., state.specification = ss, 
              data=ts_train, niter=1000, ping=0, 
              expected.model.size=2)
model2_pred <- predict(model2, newdata=ts_test, horizon=28)

plot(model2_pred, plot.original=90, 
     main='Model 2', 
     xlab='Date idx []', 
     ylab='SST [deg C]')

# BP Model -------------------------------------------------------
' Specify Inclusion Probablity'
bp = c(0.6, 0.3, 0.3, 0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
bp_model = bsts(SST ~ ., state.specification = ss, 
                data=ts_train, niter=1000, ping=0, 
                expected.model.size=2, 
                prior.inclusion.probabilities=bp)

# Compare Models -------------------------------------------------
CompareBstsModels(lwd=4, 
                  model.list = list("Model1" = model1, 
                                    "Model2" = model2, 
                                    "Model3" = bp_model))
  



















