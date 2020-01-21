## resource https://otexts.com/fpp2/appendix-for-instructors.html

install.packages("fpp2")
require("fpp2")  # same as library()


## creating and plotting a time series subject 
'Note:  When plotting a time series you need to specify that it is a ts.
        R will automatically assume that the ts is in years.  So you need
        to specify the start
        ts() is actually a function that creates a ts object. 
'

y <- ts(c(123,39,78,52,110), start=2012)
plot(y)


# Generate a TS plot from random variables
'Note:  Note the frequency is now 12 as we have 12*10 or 12 and 12 years.  That means
        10 years w/ 12 observations per year'
y <- ts(rnorm(12*10), start=2012,frequency = 12)
plot(y)


# The monthly Australian electricity demand series
'Note:  Australian monthly electric production: Jan 1956 - Aug 1995
        Frequency:  This is part of the forecast package.  Note that it appears to return 
                    the frequency of a time series.  So how many observations per time period.
                    In this case it returns 12'
plot(elec)
autoplot(elec) + xlab("Year") + ylab("GWh")
frequency(elec)
frequency(y)    # Note how we get the same result for y where we specified the frequency to be 12


# Transformations (log, sqrt, log + constant, diff(log()))
'Note:  Differences is the t+1 - t
        diff(log(values)):  Note that diff takes care of the upward trend and log takes care 
                            the large outlier values'

plot(elec, main='Plot No Transformation')
plot(log(elec), main='Plot Log of Values')
plot(sqrt(elec), main='Plot Square Root of Values')
plot(log(elec+1000), main='Plot Log of Values + 1000')
plot(diff(elec), main='Plot Difference of Values')
plot(diff(log(elec)), main='Plot Difference of Log of Values')

# Plot Original and diff(log())
par(mfrow=c(2,1))
plot(elec, main='Plot No Transformation')
plot(diff(log(elec)), main='Plot Difference of Log of Values')

## Auto Correlation Function
'Note:  Auto correlation measuring the correlation between t0 vs t + lag up to n
        Results:  All show that the x values are highly correlated with time, or with 
                  each other. 
 AutoCorrelation:
        For a time series x of length n, we consider the n-1 pairs of observations one time
        unit apart. The first such pair is x[1], x[2], the second pair is x[3], x[2]
        Each pair forms a x[t], x[t-1]

 Great Resource ACF http://rinterested.github.io/statistics/acf_pacf.html
'
acf(elec,lag=48)
acf(elec,lag=100)
ggAcf(elec, lag=48)





