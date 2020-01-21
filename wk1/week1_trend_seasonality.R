
# Clear Namespace
rm(list=ls())

# Load Libraries
require("fpp2")

# Load Data 
' a10:  Monthly anti-diabetic drug sales in Australia from 1991 to 2008
  Note: You can clearly see that there is a trend, seasonality and cyclicality in the 
        Data.  The Trend is increasing, there is a pique in sales on each year, and 
        that pique is increasing over time. '
?a10
autoplot(a10 , main='Monthly Anti-diabetic drug sales')


## Transformations - Log, diff(log)
'Notes: We can see how log seems to normalize or remove the trend toward increasing piques
        Seem to get really weird results for diff(log(a10))        

'
autoplot(log(a10), main='Log of Data')
autoplot(diff(log(a10)), main='Diff(Log()) of Data')


# Calculate Auto Correlation
'Note:  You can clearly see a difference in the ACF of the TS when we apply diff and log
        Log alone didnt seem to have the same affect as diff(log()) on the ACF. 
'
a10t = diff(log(a10))
par(mfrow=c(3,1))
acf(a10,lag=48, main='ACF of Original Data Lagged at 48')
acf(log(a10),lag=48, main='ACF log of original data')
acf(a10t,lag=48, main='ACF diff(log()) of data')


## Seasonality
' ggseasonplot:   Plots a seasonal plot as described in Hyndman and Athanasopoulos (2014, chapter 2). 
                  This is like a time plot except that the data are plotted against the seasons in 
                  separate years.
  Note:           This plot really helps you to see the seasonality at each increment in time. 
                  For instance, we can clearly see a seasonal trend in January and february where sales drop to 
                  probably their lowest point in the year.  Then they progressively increase until December.  
  Question:       I assume that ggseasonplot requires a ts object as I dont see where one specifies the time periods. 
'
?ggseasonplot
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


' ggsubseariesplot:  Plots the trend line vertically and creates a blue bar where each mean is for that
                     time period.  Almost like a box plot'

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")


## Decomposition
'Note:  Single function to decompose Time Series into Observed, trend, seasonal, and random white noise'

a10components <- decompose(a10)
plot(a10components)

a10lcomponents <- decompose(log(a10))
plot(a10lcomponents)










