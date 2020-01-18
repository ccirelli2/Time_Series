
# Example 1.9 Moving Average
'rnorm:  generate vector of random variables from dist w/ mean=0, stdv=1
 rep:    creates a repetition of an value or list of values
 filter: used to create moving averages. filter(obj2filter, numofsides, (filter2apply))
 par:    can be used to pair charts together
 plot.ts looks like it is used ot plot a time series. 
'
w = rnorm(500,0,1)
r = rep(.3, 5)                                   # Note that this number can be as large as len(w)
v = filter(w, sides=2, filter=(r))               #sides is either 1 or 2.  So average on one or both sides.
par(mfrow=c(2,1))
plot.ts(w, main='white noise')
plot.ts(v, ylim=c(-3,3), main='moving average')


# Ex cumsum
x = cumsum(w)
plot(x)


# Covariance & Correlation
a = c(1,2,3,4,5)
b = c(10, 20, 30, 40, 50)
c = c(-1, -2, -3, -4, -5)
d = c(1,4,6,8,10)
par(mfrow=c(2,1))
plot(a)
plot(b)

# Cov
print(paste('Correlation A & B =>', cov(a, b)))
print(paste('Correlation A & C =>', cov(a,c)))
print(paste('Correlation B & C =>', cov(b, c)))

# Corr
cor(a,b)
cor(a,c)
cor(b,c)
cor(a,d)

help(acf)





