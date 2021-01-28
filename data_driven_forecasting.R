######################## 442 - Advance Statistics - HW7 - Data-Driven Forecasting ##########################

# Set the working directory
setwd("~/Desktop/Classes/442-Advance Statistics/Class 5/HW")

# Clear the data and variable space
rm(list=ls(all=TRUE)) 

# Load the required libraries
library("TTR")	
library("forecast")																   	
library("tseries")

# Read the dataset
data<-read.csv("data_week.csv", sep=",",dec=".",header=T) 
names(data)
head(data,5)
sales = data[,3]/1000000		# millions in xyz currency
temp = data[,11]				    # centigrade

################## Sales Model #######################
# Representing Data as Time Series Objects
yy = ts(sales, frequency = 52,start = c(2015,1))		# converts sales data as time series object with start date and frequency (weekly here)
plot.ts(yy)										                      # ALWAYS plot time series to see patterns: trend, cycle, variance over time

# Simple Moving Average
yy4 = SMA(yy,4)	  	# 4-week moving average
yy13 = SMA(yy,13)		# 13 week (quarterly) moving average
yy52 = SMA(yy,52)		# annual moving average
plot.ts(cbind(yy,yy4,yy13,yy52))		# understand the "smoothing" concept due to the averaging operation

# Time Series Decomposition 
# Sales: What's the growth rate? How do we obtain seasonally adjusted sales?
yd = decompose(yy) 
yd.trend = yd$trend
yd.seasonal = yd$seasonal
yd.random = yd$random
yy.season.adj = yy - yd.seasonal									# seasonally adjusted sales
plot.ts(cbind(yy,yd.trend, yd.seasonal, yd.random, yy.season.adj))

# Simple Exponential Smoothing with different combinations of alpha, beta & gamma
outs_1 = HoltWinters(yy, beta = FALSE, gamma = FALSE) 	
outs_2 = HoltWinters(yy, gamma = FALSE) 	
outs_3 = HoltWinters(yy, beta = FALSE) 
sales_mat = as.matrix(cbind(rbind(outs_1$SSE,outs_2$SSE,outs_3$SSE),
                            rbind(length(yy)*log(outs_1$SSE)+2*1,
                                  length(yy)*log(outs_2$SSE)+2*2,
                                  length(yy)*log(outs_3$SSE)+2*2)))
rownames(sales_mat) = c("beta,gamma=F","gamma=F","beta=F")
colnames(sales_mat) = c("SSE","AIC") # AIC should suffice in case of time-series modeling
sales_mat
# The third model with beta=False gives the minimum AIC, so we retain that model
outs_3																                   
outs_3$fitted 														               
plot(outs_3)														                
plot.ts(cbind(yy,outs_3$fitted[,1], outs_3$fitted[,2], outs_3$fitted[,3]))

# Out of Sample Forecasts
sales_out = forecast:::forecast.HoltWinters(outs_3, h = 52, level = c(80, 90, 95))
plot(sales_out)

# Checking if forecasts can be improved upon using information in the residuals using ACF, PACF, Box test
# Autocorrelation Function (ACF)
resid_sales = sales_out$residuals				 
corr.lag = Acf(resid_sales[2:130])		
# Partial Autocorrelation Function (PACF)
corr.lag = Pacf(resid_sales[2:130]) 	# No information contained as none of the residuals seem significant
# Using Ljung-Box test to automate the above process
Box.test(resid_sales, type = "Ljung-Box")		# p-value=0.3 > 0.05, hence model can't be improved further

# Seasonality Index
yd.fig = data.frame(yd$figure)
n = 4
aggregate(yd.fig,list(rep(1:(nrow(yd.fig)%/%n+1),each=n,len=nrow(yd.fig))),mean)[-1]

################## Temperature Model #######################
# Representing Data as Time Series Objects
xx = ts(temp, frequency = 52,start = c(2015,1))		# converts temp data as time series object with start date and frequency (weekly here)
plot.ts(xx)										                      # ALWAYS plot time series to see patterns: trend, cycle, variance over time

# Simple Moving Average
xx4 = SMA(xx,4)	  	# 4-week moving average
xx13 = SMA(xx,13)		# 13 week (quarterly) moving average
xx52 = SMA(xx,52)		# annual moving average
plot.ts(cbind(xx,xx4,xx13,xx52))		# understand the "smoothing" concept due to the averaging operation

# Time Series Decomposition 
## Temperature: Are temperatures increasing?
xd = decompose(xx) 
xd.trend = xd$trend
xd.seasonal = xd$seasonal
xd.random = xd$random
plot.ts(cbind(xx,xd.trend, xd.seasonal, xd.random))

# Simple Exponential Smoothing with different combinations of alpha, beta & gamma
outt_1 = HoltWinters(xx, beta = FALSE, gamma = FALSE) 	
outt_2 = HoltWinters(xx, gamma = FALSE) 	
outt_3 = HoltWinters(xx, beta = FALSE) 
temp_mat = as.matrix(cbind(rbind(outt_1$SSE,outt_2$SSE,outt_3$SSE),
                            rbind(length(xx)*log(outt_1$SSE)+2*1,
                                  length(xx)*log(outt_2$SSE)+2*2,
                                  length(xx)*log(outt_3$SSE)+2*2)))
rownames(temp_mat) = c("beta,gamma=F","gamma=F","beta=F")
colnames(temp_mat) = c("SSE","AIC") # AIC should suffice in case of time-series modeling
temp_mat
# The third model with beta=False gives the minimum AIC, so we retain that model
outt_3																                   
outt_3$fitted 														               
plot(outt_3)														                
plot.ts(cbind(xx,outt_3$fitted[,1], outt_3$fitted[,2]))

# Out of Sample Forecasts
temp_out = forecast:::forecast.HoltWinters(outt_3, h = 52, level = c(80, 90, 95))
plot(temp_out)

# Checking if forecasts can be improved upon using information in the residuals using ACF, PACF, Box test
# Autocorrelation Function (ACF)
resid_temp = temp_out$residuals				 
corr.lag = Acf(resid_temp[2:130])		
# Partial Autocorrelation Function (PACF)
corr.lag = Pacf(resid_temp[2:130]) 	# Some lags seem significant but to be sure, let's do Box test
# Using Ljung-Box test to automate the above process
Box.test(resid_temp, type = "Ljung-Box")		# p-value=0.88 > 0.05, hence model can't be improved further
