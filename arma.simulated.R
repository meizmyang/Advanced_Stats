setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class5")
rm(list=ls(all=TRUE)) 	# clear data
library("forecast")
library("tseries") 		# reqired for adf.test of stationarity

eps = rnorm(100)		# 100 normal random variates

## AR(p) simulation p = 1 and p = 2
## 0.5, 0.3 are coefficients for ar1 and ar2
ar1 = arima.sim(list(ar=c(0.5)), n = 100, innov = eps)
ar2 = arima.sim(list(ar=c(0.5,0.3)), n = 100, innov = eps)

par(mfrow = c(1,2))
ts.plot(ar1)
ts.plot(ar2)


par(mfrow = c(2,2))
acf(ar1)
acf(ar2)
pacf(ar1)
pacf(ar2)



## MA(q) simulation q = 1 and q = 2
ma1 = arima.sim(list(ma=c(0.5)), n = 100, innov = eps)
ma2 = arima.sim(list(ma=c(0.5,0.3)), n = 100, innov = eps)


par(mfrow = c(1,2))
ts.plot(ma1)
ts.plot(ma2)


par(mfrow = c(2,2))
acf(ma1)
acf(ma2)
pacf(ma1)
pacf(ma2)


  