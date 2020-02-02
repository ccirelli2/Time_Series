## AR(1) with phi = 0.9

par(mfrow=c(2,1))

n = 500
w = rnorm(n)
x = 0
for(i in 2:n){
  x[i] = 0.9*x[i-1]+w[i]
}

par(mfrow=c(1,1))
plot(x[1:100],type="l")
acf(x,lag = 100,main=expression(paste(phi,"=0.9")))


## AR(1) with phi = -0.9
n = 500
w = rnorm(n)
x = 0
for(i in 2:n){
  x[i] = -0.9*x[i-1]+w[i]
}
par(mfrow=c(1,1))
plot(x[1:100],type="l")
acf(x,lag = 100,main=expression(paste(phi,"=-0.9")))


## AR(1) with phi = 1
n = 500
w = rnorm(n)
x = 0
for(i in 2:n){
  x[i] = x[i-1]+w[i]
}
plot(x[1:100],type="l",main=expression(paste(phi,"=1")))
acf(x,lag = 100,main=expression(paste(phi,"=1")))


