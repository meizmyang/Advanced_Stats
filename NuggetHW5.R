################################## 442 - Advance Statistics - HW5 - Big Data Regression ###################################

# Set the working directory
setwd("~/Desktop/Classes/442-Advance Statistics/Class 4/HW")

# Load the required packages
library(glmnet)

# Create simulated data 
set.seed(1234) # set seed to generate the same data every time you run the code; for different repetition, comment out this line
n = 100 									# sample size
p = 120										# p > n => Big-p Data Problem
pp = 10 									# true variables 
beta1=rbind(1,1,1,1,1) 						# 5 coefficients for first five vars as a 5 x 1 vector
beta2=rbind(1,1,1,1,1)						# 5 coeffs for for second five vars as a 5 x 1 vector
beta0 = t(t(rep(0,1,110)))				# 110 insignificant betas as 110 x 1 vector

x=matrix(runif(n*p, min=0, max=10), n, p)	# randon uniform variates from U(0, 10) arranged in matrix n x p
z1=x[,1:5] %*% beta1						# first simulated factor z1 = sum of x1 through x5
z2=x[,6:10] %*% beta2						# second factor z2
yy= z1 + 10*sqrt(z2) 						# true dependent variable
y = yy + x[,11:120] %*% beta0 + rnorm(n)	# observed dependent variable with 100 regressors insignificant vars but we don't know that ex ante

# Partition training and test data sets
m = 0.8*n									# 80% training size ==> 20% holdout sample
ts = sample(1:n,m)				# random draw of 80 out of 100 rows
x.train = x[ts,]							
y.train = y[ts]
x.test = x[-ts,]
y.test = y[-ts]



############  Part A: Regularization models (Ridge, Lasso, Elastic Net) #########

######## Lasso Regression when alpha = 1 ########
out.lasso = glmnet(x.train, y.train, alpha = 1)     # fits lasso becasue alpha = 1 vanishes the quadratic penalty
# Coeffcients plots
plot(out.lasso, xvar="lambda", label=TRUE)	      # plots estimates vs log(lambda) values. See how *all* estimates tend to zero as lambda increases.	
title("Lasso Coefficients Plot",line=2.5)

# Extract LASSO estimates for specific lambda values
est_1 = coef(out.lasso, s = 0.01)			  	  
# estimates at lambda = 0.01 => many vars deemed significant, yet their magnitudes differ b/w sig and nonsig vars
est_2 = coef(out.lasso, s = 0.5)					  
# estimates when lambda = 0.5 => separation b/w sig an insignifcant vars improves substantially

# Optimal Lambda value?  Select by using n-fold cross-validation 
cvlasso=cv.glmnet(x.train, y.train, type.measure="mse", 
                  alpha = 1, nfolds = 10)    # 10-fold cross-validation
plot(cvlasso, main = "Select Best Lambda")							               	# plot of MSE vs Lambda
lam_est = cvlasso$lambda.min
lam_est
# best lambda --> one that minimizes mse
lasso_est = coef(out.lasso, s = lam_est)							                 	# best parameter estimates 

# Prediction Using Test Sample Data
yhat = predict(cvlasso, s = lam_est, newx=x.test)					# x.test provides data from holdout sample
sse.test = sum((y.test - yhat)^2)									        # sum of square errors in holdout sample
sst.test = sum((y.test-mean(y.test))^2)								    # total sum of squares at ybar in holdout sample
lasso_r2 = 1-sse.test/sst.test											      # R square = 1 - sum of squares errors with the model/sum of squares errors w/o the model (ie just ybar) 
plot(x=y.test,y=yhat, main = "Y - Actual vs. Predicted")  # Plot the actual vs. predicted y from test set

######## Ridge Regression when alpha = 0 ########
out.ridge = glmnet(x.train, y.train, alpha = 0)   

# Coeffcients plots
plot(out.ridge, xvar="lambda", label=TRUE)	      # plots estimates vs log(lambda) values.	
title("Ridge Coefficients Plot",line=2.5)

# Extract Ridge estimates for specific lambda values
est_1 = coef(out.ridge, s = 0.01)			  	  
# estimates at lambda = 0.01 => 
# many vars deemed significant, yet their magnitudes differ b/w sig and nonsig vars
est_2 = coef(out.ridge, s = 0.5)					  # estimates when lambda = 0.5 => separation b/w sig an insignifcant vars improves substantially

# Optimal Lambda value?  Select by using n-fold cross-validation 
cvridge=cv.glmnet(x.train, y.train, type.measure="mse", 
                  alpha = 0, nfolds = 10)    # 10-fold cross-validation
plot(cvridge, main = "Select Best Lambda")							               	# plot of MSE vs Lambda
lam_est = cvridge$lambda.min
lam_est
# best lambda --> one that minimizes mse
ridge_est = coef(out.ridge, s = lam_est)							                 	# best parameter estimates 

# Prediction Using Test Sample Data
yhat = predict(cvridge, s = lam_est, newx=x.test)					# x.test provides data from holdout sample
sse.test = sum((y.test - yhat)^2)									        # sum of square errors in holdout sample
sst.test = sum((y.test-mean(y.test))^2)								    # total sum of squares at ybar in holdout sample
ridge_r2 = 1-sse.test/sst.test											      # R square = 1 - sum of squares errors with the model/sum of squares errors w/o the model (ie just ybar) 
plot(x=y.test,y=yhat, main = "Y - Actual vs. Predicted")  # Plot the actual vs. predicted y from test set

######## Elastic Net Regression when alpha between 0.1-0.9 ########
seq_en = seq(0.1,0.9,by=0.1)
mse.min = c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
j = 1
for (i in seq_en){
  cven=cv.glmnet(x.train, y.train, type.measure="mse", alpha = i, nfolds = 10) 
  mse.min[j] <- min(cven$cvm)
  j = j+1
}
en_tbl = as.matrix(cbind(seq_en,mse.min))
colnames(en_tbl) = c("alpha","Min MSE")
en_tbl
a = en_tbl[en_tbl[,2] == min(en_tbl[,2]),1]
a

out.en = glmnet(x.train, y.train, alpha = a)

# Coeffcients plots
plot(out.en, xvar="lambda", label=TRUE)	      # plots estimates vs log(lambda) values.	
title("ElasticNet Coefficients Plot",line=2.5)

# Extract ElasticNet estimates for specific lambda values
est_1 = coef(out.en, s = 0.01)			  	  # estimates at lambda = 0.01 => many vars deemed significant, yet their magnitudes differ b/w sig and nonsig vars
est_2 = coef(out.en, s = 0.5)					  # estimates when lambda = 0.5 => separation b/w sig an insignifcant vars improves substantially

# Optimal Lambda value?  Select by using n-fold cross-validation 
cven=cv.glmnet(x.train, y.train, type.measure="mse", alpha = a, nfolds = 10)    # 10-fold cross-validation
plot(cven, main = "Select Best Lambda")							               	# plot of MSE vs Lambda
lam_est = cven$lambda.min											                      # best lambda --> one that minimizes mse
en_est = coef(out.en, s = lam_est)							                 	# best parameter estimates 

# Prediction Using Test Sample Data
yhat = predict(cven, s = lam_est, newx=x.test)					# x.test provides data from holdout sample
sse.test = sum((y.test - yhat)^2)									        # sum of square errors in holdout sample
sst.test = sum((y.test-mean(y.test))^2)								    # total sum of squares at ybar in holdout sample
en_r2 = 1-sse.test/sst.test											      # R square = 1 - sum of squares errors with the model/sum of squares errors w/o the model (ie just ybar) 
plot(x=y.test,y=yhat, main = "Y - Actual vs. Predicted")  # Plot the actual vs. predicted y from test set

####### Combining Lasso, Ridge and Elastic Net estimates and r2 ########
Estimates = cbind(lasso_est,ridge_est,en_est)
colnames(Estimates) = c("Lasso","Ridge","ElasticNet")
R2 = cbind(lasso_r2,ridge_r2,en_r2)
colnames(R2) = c("Lasso","Ridge","ElasticNet")
lambda = cbind(cvlasso$lambda.min,cvridge$lambda.min,cven$lambda.min)
colnames(lambda) = c("Lasso","Ridge","ElasticNet")
final_tbl = list(Estimates,R2,lambda)
final_tbl
