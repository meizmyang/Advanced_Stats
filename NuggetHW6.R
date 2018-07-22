################################## 442 - Advance Statistics - HW6 - Big Data Regression ###################################

# Set the working directory
setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class 3 Non-Linear Regression")
rm(list=ls())
# Load the required packages
library(glmnet)
library(ggplot2)

data=read.csv("Cars_Data.csv", header=T)    # read csv file and label the data as "data"
View(data)
y=data[,17]
x=as.matrix(data[,2:16])
out.linreg=lm(y~x) 							            
# cannot estimate all coefficients b/c p > n
out.linreg
############  Lasso to find two most important predictors #########
# Lasso
out.lasso = glmnet(x, y, alpha = 1) 
# Coefficient Plots
plot(out.lasso, xvar="lambda", label=TRUE)	      
# plots estimates vs log(lambda) values 
#See how *all* estimates tend to zero as lambda increases. 	
title("Lasso Coefficients Plot",line=2.5)

# Optimal Lambda
cvlasso=cv.glmnet(x, y, type.measure="mse", nfolds = 3)   
# 3-fold instead of 10-fold validation
plot(cvlasso, main = "Select Best Lambda")								
# plot of MSE vs Lambda
lam.est = cvlasso$lambda.min
lam.est
# best lambda --> one that minimizes mse
lasso.est = coef(out.lasso, s = lam.est)
lasso.est
# lasso estimates with best lambda

############  Linear Regression to find unbiased estimates of the two most important predictors #########
# Linear Regression 
lasso.lam1 = coef(out.lasso, s = 1)	
lasso.lam1
# Set lambda = 1 that happens to fit two best predictors

######################## Looking at the two best variables - Attractiveness & Quiet ###############################
linreg=lm(y~x[,1:2])
linreg
# Fit linear regression with those predictors 

# Predicted Preferences
yhat = predict(linreg)					
sse = sum((y - yhat)^2)								# sum of squares of errors 
sst = sum((y-mean(y))^2)							# sum of squares total
r2 = 1-sse/sst										    # R square

# Ideal Vector 										
b1=as.vector(coef(linreg)[2])
b2=as.vector(coef(linreg)[3])
b1
b2

slope.iso.preference = - b1/b2					
slope.ideal.vector = b2/b1 						

angle.iso.preference = atan(slope.iso.preference)*180/pi	
angle.ideal.vector = atan(slope.ideal.vector)*180/pi

# Build positioning map for car brands when number of attributes exceed the number of brands in the market
# Using only the first two used in regression
ggplot(data = data[,1:3], aes(x=data[,3],y=data[,2],color=data[,1])) + geom_point(size=data[,17]) +
  geom_text(data=data[1:3],aes(x=data[,3],y=data[,2],label=data[,1]), color="black", size=3.5) +
  guides(colour=FALSE) +
  geom_hline(yintercept = mean(data[,2])) +
  geom_vline(xintercept = mean(data[,3])) +
  xlab("Quiet") +
  ylab("Attractiveness") +
  ggtitle("Positioning Map for Car brands") +
  geom_abline(slope = slope.iso.preference, intercept = 5) +
  geom_segment(aes(x = mean(data[,3]), y = mean(data[,2]), xend = mean(data[,3])+b1, yend = mean(data[,2])+b2), 
               arrow = arrow(length = unit(0.3, "cm")))
 
############################ Looking at two other variables - Roomy, Sporty ###################################
linreg=lm(y~x[,c(7,9)])							         	# Fit linear regression with those predictors 

# Predicted Preferences
yhat = predict(linreg)					
sse = sum((y - yhat)^2)								# sum of squares of errors 
sst = sum((y-mean(y))^2)							# sum of squares total
r2 = 1-sse/sst										    # R square

# Ideal Vector 										
b1=as.vector(coef(linreg)[2])
b2=as.vector(coef(linreg)[3])

slope.iso.preference = - b1/b2					
slope.ideal.vector = b2/b1 						

angle.iso.preference = atan(slope.iso.preference)*180/pi	
angle.ideal.vector = atan(slope.ideal.vector)*180/pi

# Build positioning map for car brands when number of attributes exceed the number of brands in the market
# Using only the first two used in regression
ggplot(data = data, aes(x=data[,7],y=data[,9],color=data[,1])) + geom_point(size=data[,17]) +
  geom_text(data=data,aes(x=data[,7],y=data[,9],label=data[,1]), color="black", size=3.5) +
  guides(colour=FALSE) +
  geom_hline(yintercept = mean(data[,9])) +
  geom_vline(xintercept = mean(data[,7])) +
  xlab("Sporty") +
  ylab("Roomy") +
  ggtitle("Positioning Map for Car brands") +
  geom_abline(slope = slope.iso.preference, intercept = 5) +
  geom_segment(aes(x = mean(data[,7]), y = mean(data[,9]), xend = mean(data[,7])+b1, yend = mean(data[,9])+b2), 
               arrow = arrow(length = unit(0.3, "cm")))

####################### Looking at two other variables - Easy Service, Economical ##############################
linreg=lm(y~x[,c(10,13)])							         	# Fit linear regression with those predictors 

# Predicted Preferences
yhat = predict(linreg)					
sse = sum((y - yhat)^2)								# sum of squares of errors 
sst = sum((y-mean(y))^2)							# sum of squares total
r2 = 1-sse/sst										    # R square

# Ideal Vector 										
b1=as.vector(coef(linreg)[2])
b2=as.vector(coef(linreg)[3])

slope.iso.preference = - b1/b2					
slope.ideal.vector = b2/b1 						

angle.iso.preference = atan(slope.iso.preference)*180/pi	
angle.ideal.vector = atan(slope.ideal.vector)*180/pi

# Build positioning map for car brands when number of attributes exceed the number of brands in the market
# Using only the first two used in regression
ggplot(data = data, aes(x=data[,10],y=data[,13],color=data[,1])) + geom_point(size=data[,17]) +
  geom_text(data=data,aes(x=data[,10],y=data[,13],label=data[,1]), color="black", size=3.5) +
  guides(colour=FALSE) +
  geom_hline(yintercept = mean(data[,13])) +
  geom_vline(xintercept = mean(data[,10])) +
  xlab("Easy Service") +
  ylab("Economical") +
  ggtitle("Positioning Map for Car brands") +
  geom_abline(slope = slope.iso.preference, intercept = 5) +
  geom_segment(aes(x = mean(data[,10]), y = mean(data[,13]), xend = mean(data[,10])+b1, yend = mean(data[,13])+b2), 
               arrow = arrow(length = unit(0.3, "cm")))
#log(x)=8, then exp(8)=x


