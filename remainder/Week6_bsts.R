## source: http://www.unofficialgoogledatascience.com/2017/07/fitting-bayesian-structural-time-series.html

install.packages("bsts")
library(bsts)     # load the bsts package
View(initial.claims)
data(iclaims)     # bring the initial.claims data into scope

ss <- AddLocalLinearTrend(list(), initial.claims$iclaimsNSA)
ss <- AddSeasonal(ss, initial.claims$iclaimsNSA, nseasons = 52)
model1 <- bsts(initial.claims$iclaimsNSA,
               state.specification = ss,
               niter = 1000)
plot(model1)
plot(model1, "components")  # plot(model1, "comp") works too!
plot(model1, "help")

pred1 <- predict(model1, horizon = 12)
plot(pred1, plot.original = 156)

