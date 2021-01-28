######################## 442 - Advance Statistics - HW8 - ARIMA Forecasting ##########################

# Set the working directory
setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class 5")

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
# Representing Data as Time Series Object #####
yy = ts(sales, frequency = 52,start = c(2015,1))		
plot.ts(yy)			

# Fitting ARIMA(p,d,q) Models step-by-step

# Step 1. Is Stationary? 
# Use Augmented Dickey-Fuller Test to test stationarity == > large p value (> 0.10) means nonstationary
adf.test(yy)							
yd = diff(yy,differences = 1)			# difference between itself and its previous value
plot.ts(yd)								        # looks stationary visually
adf.test(yd)		

# Step 2. Decide AR(p) or MA(q) or both ARMA(p,q). Use the stationary series from Step 1. 
# To decide AR (p), plot Pacf. AR(p) signature = Pacf becomes zero at some lag p
Pacf(yd, lag.max = 20)					
# To decide MA, plot Acf. MA(q) signature = Acf becomes zero at some lag q
Acf(yd, lag.max = 20)		

# Step 3. Fit several ARIMA models. 	
sales_m1 = auto.arima(yy)		# fits ARIMA automatically
sales_m2 = arima(yy, order=c(1,1,0))			
sales_m3 = arima(yy, order=c(2,1,0))			
sales_m4 = arima(yy, order=c(1,1,1))	

# Step 4. Compare AIC to select the best model (AIC should suffice in this case, as discussed in class)
sales_aic_mat = rbind(sales_m1$aic,sales_m2$aic,sales_m3$aic,sales_m4$aic)
rownames(sales_aic_mat) = c("Model1","Model2","Model3","Model4")
sales_aic_mat

# Model 3 has the minimum AIC, auto.arima also gives out the same model

# Step 5. Make Out-of-Sample Forecasts with Prediction Interval based on your retained model
m.predict = forecast:::forecast.Arima(sales_m3, h = 52, level = c(80, 90))
plot(m.predict)

################## Temperature Model #######################
# Representing Data as Time Series Object #####
xx = ts(temp, frequency = 52,start = c(2015,1))		
plot.ts(xx)			

# Fitting ARIMA(p,d,q) Models step-by-step

# Step 1. Is Stationary? 
# Use Augmented Dickey-Fuller Test to test stationarity == > large p value (> 0.10) means nonstationary
adf.test(xx)							
xd = diff(xx,differences = 1)			# difference between itself and its previous value
plot.ts(xd)								        # looks stationary visually
adf.test(xd)		

# Step 2. Decide AR(p) or MA(q) or both ARMA(p,q). Use the stationary series from Step 1. 
# To decide AR (p), plot Pacf. AR(p) signature = Pacf becomes zero at some lag p
Pacf(xd, lag.max = 20)					
# To decide MA, plot Acf. MA(q) signature = Acf becomes zero at some lag q
Acf(xd, lag.max = 20)		

# Step 3. Fit several ARIMA models. 	
temp_m1 = auto.arima(xx)		# fits ARIMA automatically
temp_m2 = arima(xx, order=c(1,1,0))			
temp_m3 = arima(xx, order=c(2,1,0))			
temp_m4 = arima(xx, order=c(1,1,1))	

# Step 4. Compare AIC to select the best model (AIC should suffice in this case, as discussed in class)
temp_aic_mat = rbind(temp_m1$aic,temp_m2$aic,temp_m3$aic,temp_m4$aic)
rownames(temp_aic_mat) = c("Model1","Model2","Model3","Model4")
temp_aic_mat

# Model 3 has the minimum AIC, and it gives better results than auto.arima

# Step 5. Make Out-of-Sample Forecasts with Prediction Interval based on your retained model
m.predict = forecast:::forecast.Arima(temp_m3, h = 52, level = c(80, 90))
plot(m.predict)



  
