## resource https://otexts.com/fpp2/appendix-for-instructors.html

install.packages("fpp2")
require("fpp2")

## creating and plotting a time series subject 

y <- ts(c(123,39,78,52,110), start=2012)
plot(y)


y <- ts(rnorm(12*10), start=2012,frequency = 12)
plot(y)

# The monthly Australian electricity demand series
plot(elec)
autoplot(elec) + xlab("Year") + ylab("GWh")

frequency(elec)


# transformation
plot(log(elec))

plot(sqrt(elec))

plot(log(elec+1000))

plot(diff(log(elec)))
## acf
acf(elec,lag=48)
acf(elec,lag=100)

ggAcf(elec, lag=48)
