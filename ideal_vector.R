setwd("/Users/prasad/Desktop/BAX442/Lectures/Class4")
library(glmnet)


data=read.csv("Cars_Data.csv", header=T)    # read csv file and label the data as "data"

y=data[,17]
x=as.matrix(data[,2:16])
out.linreg=lm(y~x) 							# cannot estimate all coefficients b/c p > n


############  Lasso to find two most important predictors #########

# Lasso
out.lasso = glmnet(x, y, alpha = 1) 

# Coefficient Plots

plot(out.lasso, xvar="lambda", label=TRUE)	      # plots estimates vs log(lambda) values 
#See how *all* estimates tend to zero as lambda increases. Why? 	
title("Lasso Coefficients Plot",line=2.5)

# Optimal Lambda
cvlasso=cv.glmnet(x, y, type.measure="mse", nfolds = 3)    				# Why 3-fold cross-validation? 
plot(cvlasso, main = "Select Best Lambda")								# plot of MSE vs Lambda
lam.est = cvlasso$lambda.min											# best lambda --> one that minimizes mse
lasso.est = coef(out.lasso, s = lam_est)								# lasso estimates with best lambda


############  Linear Regression to find unbiased estimates of the two most important predictors #########

# Linear Regression 
lasso.lam1 = coef(out.lasso, s = 1)					# Set lambda = 1 that happends to fit two best predictors
linreg=lm(y~x[,1:2])								# Fit linear regression with those predictors 
													# Why fit lm and not just move on with the lasso estimates?

# Predicted Preferences

yhat = predict(linreg)					
sse = sum((y - yhat)^2)								# sum of squares of errors 
sst = sum((y-mean(y))^2)							# sum of squares total
r2 = 1-sse/sst										# R square


# Ideal Vector 										# read my note on Positioning Maps

b1=as.vector(coef(linreg)[2])
b2=as.vector(coef(linreg)[3])

slope.iso.preference = - b1/b2						# Why? Exam Q
slope.ideal.vector = b2/b1 							# Why? Exam Q

angle.iso.preference = atan(slope.iso.preference)*180/pi	
angle.ideal.vector = atan(slope.ideal.vector)*180/pi

# Are these slopes perpendicular? Why so? Exam Q


##### Deliverables for HW 6 #####

# 1. Build positioning map for car brands when number of attributes exceed the number of brands in the market

# 2. Explain iso-preference line and its difference from the regression line (or plane)

# 3. Explain why the ideal vector indicates the direction of increasing preferences 

# 4. Computing orientation of the ideal vector. Place an arrow at the origin on x1-x2 plot with 10 brands as points with labels (you can use Excel). 

# 5. Your recommendation to Infinity brand managers on how to improve their product's design

# 6. Hard Q (not graded): How will you find 90% confidence interval for the angle of the ideal vector? 






  