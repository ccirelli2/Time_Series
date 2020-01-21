require("fpp2")
?scan
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthstimeseries)
acf(birthstimeseries,lag=48)

birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

# remove seasonality
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)
acf(birthstimeseriesseasonallyadjusted,lag=48)

# remove trend
time1 = abs(time(birthstimeseries)-1948)*(time(birthstimeseries)<1948)
time2 = abs(time(birthstimeseries)-1948)*(time(birthstimeseries)>=1948)
fit = lm(birthstimeseriesseasonallyadjusted~time1+time2)
birth <- fit$residuals
plot.ts(birth)


acf(birth,lag=48)

