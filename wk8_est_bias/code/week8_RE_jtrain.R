## description of the data set can be found at: http://fmwww.bc.edu/ec-p/data/wooldridge/jtrain.des
## N=3710, cross-sectional individual data

#install.packages("foreign")
install.packages("plm")
require(foreign)
library("plm")
library("nlme")

jtrain = read.dta("./Dropbox/teaching/MSA8200/spring 2020/Data/jtrain1.dta",convert.factors = FALSE)

head(jtrain)
jt = jtrain[!is.na(jtrain$scrap),]
T = 3
N = nrow(jt)/T
K = 5

### OLS and FGLS for panel data
### do it by hand

### pooled ols to get v_hat
y = jt$lscrap
I88 = (jt$year==1988)
I89 = (jt$year==1989)
X = cbind(rep(1,N),I88,I89,jt$grant,jt$grant_1)


## first step, estimate Omega matrix using sigma_c^2 and sigma_v^2
beta_pols = solve(t(X)%*%X)%*%(t(X)%*%y)
y_hat = X%*%beta_pols
v_hat = y - y_hat

### omegat matrix
v_mat = matrix(v_hat,3)
sigma2v = sum(v_mat^2)/(N*T - K)
sigma2c = sum(v_mat[1,]*v_mat[2,]+v_mat[1,]*v_mat[3,]+v_mat[2,]*v_mat[3,])/(N*T*(T-1)/2-K)

Omega = matrix(sigma2c,T,T) + diag(sigma2v-sigma2c,T)
Omega_inv = solve(Omega)

## step 2: plug in the numbers in the equation
xox = matrix(0,K,K)
for(i in 1:N) xox = xox + t(X[1:T+T*(i-1),])%*%Omega_inv%*%X[1:T+T*(i-1),]

xoy = rep(0,K)
for(i in 1:N) xoy = xoy + t(X[1:T+T*(i-1),])%*%Omega_inv%*%y[1:T+T*(i-1)]

beta_re = solve(xox)%*%(xoy)

### test for sigma_c^2

top = sum(v_mat[1,]*v_mat[2,]+v_mat[1,]*v_mat[3,]+v_mat[2,]*v_mat[3,])

bot = sqrt(sum((v_mat[1,]*v_mat[2,]+v_mat[1,]*v_mat[3,]+v_mat[2,]*v_mat[3,])^2))

tstat = top/bot
pvalue = pnorm(tstat,lower.tail = FALSE)
pvalue

#######
random <- plm(lscrap ~ d88 + d89 + grant + grant_1, data=jtrain, index=c("fcode", "year"), model="random")
summary(random)
