
# Clear Namespace & Charts-------------------------------
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# Import Libraries --------------------------------------
library(ggplot2)
library(readr)
library(bsts)
library(oce)
library(ggplot2)
library(astsa)
library(TSA)
library(pracma)
library(dplyr)
library(lubridate)
library(reshape)
library(tseries)
library(forecast)

# Import Data -------------------------------------------
setwd('/home/ccirelli2/Desktop/repositories/Time_Series/mini_project')
data <- read_csv('data.csv')



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






