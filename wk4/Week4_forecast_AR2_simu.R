ar.sim<-arima.sim(model=list(ar=c(.9,-.2)),n=10000) 

ar.sim

z = c(1,-.9,.2)
polyroot(z)


out = acf(ar.sim)
rho = out$acf[1:5]
G2 = matrix(c(rho[1],rho[2],rho[2],rho[1]),2)
rho
G2
g2
g2 = c(rho[2],rho[3])
solve(G2)%*%g2


G3 = matrix(c(rho[1],rho[2],rho[3],rho[2],rho[1],rho[2],rho[3],rho[2],rho[1]),3)
g3 = c(rho[2],rho[3],rho[4])
solve(G3)%*%g3
