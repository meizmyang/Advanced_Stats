setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class 5")
rm(list=ls(all=TRUE)) 	# clear data
library("forecast")
library("tseries") 		# reqired for adf.test of stationarity

data<-read.csv("data_week.csv", sep=",",dec=".",header=T) 	# weekly data
names(data)
head(data,5)

sales = data[,3]/1000000		# millions in xyz currency
temp = data[,11]				# centigrade



##### Represeniting Data as Time Series Object #####

yy = ts(sales, frequency = 52,start = c(2015,1))		
# coverts sales data as time series object with start date and frequency (weekly here)
plot.ts(yy)												
# ALWAYS plot time series to see patterns: trend, cycle, variance over time

xx = ts(temp, frequency = 52, start = c(2015,1))
plot.ts(xx)


##### Fitting ARIMA(p,d,q) Models #####

# Values of p, d, q are typically 0, 1, 2 or 3 -- not large integers like ARIMA(25,10,3)
# Intercept? Yes when d = 0 (included automatically). 
##### No intercept if d = 1 or more (as it gets cancelled when diff is computed). 
# Fit about dozen or so models and apply information criteria to select the best one
# ARMA models require STATIONARY time series as inputs 
##### (i.e., mean and variance are approx constant over time)
# If non-stationary, then difference the series d times, where d = 0, 1, or 2. 



## let's learn the process using sales series

## Step 1. Is Stationary? 

# Use Augmented Dickey-Fuller Test to test stationarity == > 
# large p value means nonstationary

plot.ts(yy)								
# ALWAYS plot series to eyeball the presence of trends and/or variability over disjoint regimes

# install and load "tseries" package 
adf.test(yy)							
# formal test for stationarity ==> if p is large (> 0.10), then nonstationary

yd = diff(yy,differences = 1)			
plot.ts(yd)								# looks stationary visually
adf.test(yd)							
# estimated p = 0.01 ==> small pvalue (< 0.10) ==> 
# so yd is stationary ==> fix d = 1 in ARIMA models to be fitted




## Step 2. Decide AR(p) or MA(q) or both ARMA(p,q). 
## Use the stationary series from Step 1. 

# To decide AR (p), plot Pacf. 
# AR(p) signature = Pacf becomes zero at some lag p

Pacf(yd, lag.max = 20)	# Pacf suggests p = 2 


# To decide MA, plot Acf. 
# MA(q) signature = Acf becomes zero at some lag q

Acf(yd, lag.max = 20)		 # MA(1)?


## Step 3. Fit several ARIMA models. 	
## note: differencing (d = 1) is specified in the "order"; 
## so fit the original series (yy, not yd)
m1 = arima(yy, order=c(1,1,0))			
m2 = arima(yy, order=c(2,1,0))			
m3 = arima(yy, order=c(1,1,1))	
# ... and so on

# Consider Seasonal ARIMA(p,d,q) x (P, D, Q) components 
########when seasonality is expected/suspected

m4 = arima(yy, order=c(1,1,0), seasonal = list(order = c(0,0,1), period = 52))
m5 = arima(yy, order=c(2,1,0), seasonal = list(order = c(1,0,0), period = 52))

# ... and so on




## Step 4. Compute Information Criteria using formulae 
############given in the previous class to select the best model

# To compute aic etc, you will need sigma^2 or SSE/T or MSE, 
############number of parameters, and sample size




## Step 5. Make Out-of-Sample Forecasts with Prediction Interval 
############based on your retained model

m.predict = forecast:::forecast.Arima(m5, h = 52, level = c(80, 90))
plot(m.predict)


################ SHORT-CUT #############
## Short-cut wraps the above steps in a function called "auto.arima". 

m6 = auto.arima(yy)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
m6


## So why use the above step-by-step procedure? 
## First and foremost, modeling is a process of discovery
## Second, auto.arima imposes the final model on you 
########### based ONLY on statistical considerations
## Whereas you need to incorporate business considerations ALSO  
## So do use it, but check that forecasts/conclusions/recommendations 
########### based on the retained model meet your business needs

## To emphasize this last point, consider auto.arima for temperature series. 
## Can you forecast temperatures better using data-driven approach from class 5a? Compare the forecasts from auto.arima and data-driven forecast. 

m7 = auto.arima(xx)
m7

m.predict = forecast:::forecast.Arima(m7, h = 52, level = c(80, 90))
plot(m.predict)



##  Deliverables for HW 8


# 1. Step-by-Step fit different ARIMA (p,d,q) x (P, D, Q) for both sales and temperature series, 
# Can you discover a better model than auto.arima?

# 2. Compute information criteria (AIC, AICC, BIC). Recommend the best model

# 3. For the retained model, present out-of-sample forecasts with prediction intervals






  