require("fpp2")


?a10
autoplot(a10)

## transformation and detrend
autoplot(log(a10))
autoplot(diff(log(a10)))

a10t = diff(log(a10))

acf(a10,lag=48)
acf(log(a10),lag=48)
acf(a10t,lag=48)

## seasonality
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

a10components <- decompose(a10)
plot(a10components)

a10lcomponents <- decompose(log(a10))
plot(a10lcomponents)

