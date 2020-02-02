require("astsa")

###########################################################
##                                                       ##
## analysis of the GNP data                              ##
##                                                       ##
###########################################################


plot(gnp)
acf(gnp)

plot(diff(log(gnp))) ## growth rate
acf2(diff(log(gnp)))

out = ar(diff(log(gnp)),1) ## AR(1)

acf2(out$resid)
