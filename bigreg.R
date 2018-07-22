setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class4")
install.packages("glmnet")
install.packages("pls")
library(glmnet)
library(pls)


# Create simulated data 

set.seed(1234) # set seed to generate the same data every time you run the code; for different repetition, comment out this line
n = 100 									# sample size
p = 120										# p > n => Big-p Data Problem
pp = 10 									# true variables 
beta1=rbind(1,1,1,1,1) 						
# 5 coefficients for first five vars as a 5 x 1 vector
beta2=rbind(1,1,1,1,1)						
# 5 coeffs for for second five vars as a 5 x 1 vector
beta0 = t(t(rep(0,1,110)))					
# 110 insignificant betas as 110 x 1 vector

x=matrix(runif(n*p, min=0, max=10), n, p)	
# randon uniform variates from U(0, 10) arranged in matrix n x p
z1=x[,1:5] %*% beta1						
# first simulated factor z1 = sum of x1 through x5
z2=x[,6:10] %*% beta2						# second factor z2
yy= z1 + 10*sqrt(z2) 						# true dependent variable
y = yy + x[,11:120] %*% beta0 + rnorm(n)	
# observed dependent variable with 100 regressors insignificant vars but we don't know that ex ante

# Partition training and test data sets

m = 0.8*n									# 80% training size ==> 20% holdout sample
ts = sample(1:n,m)							# random draw of 80 out of 100 rows
x.train = x[ts,]							
y.train = y[ts]

x.test = x[-ts,]
y.test = y[-ts]


############  Part A: Regularization models (Ridge, Lasso, Elastic Net) #########


# LASSO when alpha = 1. 
# Ridge Regression when alpha = 0. 
# Elastic Net when alpha = strictly positive fraction Why? Exam Q


# Lasso Regression

out.lasso = glmnet(x.train, y.train, alpha = 1)     
# fits lasso becasue alpha = 1 vanishes the quadratic penalty

# Coeffcients plots

plot(out.lasso, xvar="lambda", label=TRUE)	      # plots estimates vs log(lambda) values. See how *all* estimates tend to zero as lambda increases. Why? Exam Q	
title("Lasso Coefficients Plot",line=2.5)

# Extract LASSO estimates for specific lambda values

est_1 = coef(out.lasso, s = 0.01)	
est_1
# estimates at lambda = 0.01 => 
# many vars deemed significant, 
# yet their magnitudes differ b/w sig and nonsig vars
est_2 = coef(out.lasso, s = 0.5)					  
# estimates when lambda = 0.5 ==> 
# separation b/w sig an insignifcant vars improves substantially

      
# Optimal Lambda value?  Select by using n-fold cross-validation 

cvlasso=cv.glmnet(x.train, y.train, type.measure="mse", nfolds = 10)    
# 10-fold cross-validation
plot(cvlasso, main = " Select Best Lambda")								
# plot of MSE vs Lambda
lam_est = cvlasso$lambda.min											
# best lambda --> one that minimizes mse
lasso_est = coef(out.lasso, s = lam_est)								
# best parameter estimates 


# Prediction Using Test Sample Data

yhat = predict(cvlasso, s = lam_est, newx=x.test)
# x.test provides data from holdout sample
sse.test = sum((y.test - yhat)^2)	
sse.test
# sum of square errors in holdout sample
sst.test = sum((y.test-mean(y.test))^2)	
sst.test
# total sum of squares at ybar in holdout sample
r2 = 1-sse.test/sst.test											
r2
# R square = 1 - sum of squares errors with the model/sum of squares errors w/o the model (ie just ybar) 
?predict



############  Part B: Dimension Reduction models (PCR and PLS) #########

# data must be in dataframe form and not as a matrix
xydata = data.frame(y = y, x = x)


#### Partial Least Squares Regression ####

# runs partial least sqaures regression with p > n
out.pls = plsr(y ~ x, ncomp = 90, data = xydata, 
               validation = "LOO", jackknife = TRUE)
?plsr
# Select number of components to retain by cross-validation --> 
# prodcues plot with std error bars
best.pls.k = selectNcomp(out.pls, method = "onesigma", 
                         ncomp = 90, plot = TRUE)  
# We retain 4 components. Why not 5 as indicated by graph?

# weights for all 120 variables in each component. 
# In class slides, the values of w_ik, i = variable, k = component. 
#Also known as the "loadings" matrix, which has p x ncomp dimension

load.mat.pls = loadings(out.pls)[,1:best.pls.k]  
# allows interpretation of each component's meaning based on which variables 'load" on a given component

# Coefficients or parameter estimates and their significance 
est.pls = jack.test(out.pls, ncomp = best.pls.k)
est.pls

# Prediction (Overall sample, but can be done in test sample)

yhat = predict(out.pls, ncomp = best.pls.k)			# based on *first 4 components* and not just the 4th component
r2 = cor(y,yhat)^2
r2



#### Principal Components Regression ####

# runs principal components regression with p > n
out.pcr = pcr(y ~ x, ncomp = 90, data = xydata, 
              validation = "LOO", jackknife = TRUE)

# Select number of components to retain by cross-validation --> 
# prodcues plot with std error bars
best.pcr.k = selectNcomp(out.pcr, method = "onesigma", 
                         ncomp = 90, plot = TRUE)  
# We retain 1 component. 

# weights for all 120 variables in each component. 
# In class slides, the values of w_ik, i = variable, k = component. 
# Also known as the "loadings" matrix, which has p x ncomp dimension

# loadings for the first through best.pcr.k number of factors 
load.mat.pcr = loadings(out.pcr)[,1:best.pcr.k]  
# allows interpretation of each component's meaning based on which variables 'load" on a given component

# Coefficients (ie. parameter estimates) and their significance 
est.pcr = jack.test(out.pcr, ncomp = best.pcr.k)


# Prediction (Overall sample, but can be done in test sample)

yhat = predict(out.pcr, ncomp = best.pcr.k)			
# based on *first 4 components* and not just the 4th component
r2 = cor(y,yhat)^2
r2



##### Deliverables for HW 5 #####

# Use the above simulated data so that we know the operating model 
######(unlike in real data)

# IGNORE PLS and PCR in HW 5 submission, mainly b/c I have already written 
###### up the code. But run it to study its output and how it works

# Do Lasso (as above) and then modify code to do Ridge and Elastic Net Regressions

# Set alpha = 0 and re-do all the steps. You will be running Ridge Regression. For example, out.ridge = glmnet(x.train, y.train, alpha = 0) 

# Set alpha = 0.1 to 0.9 in steps of 0.1. You will be running Elastic Net Regression (9 runs). For example, out.en.1 = glmnet(x.train, y.train, alpha = 0.1)

# Report the following results from your total analysis alpha = 0 (Ridge), 0.1, ...0.9, 1.0 (Lasso)

# 1. Provide and Explain the Coefficients Plots for Ridge, Best-alpha EN, Lasso. Here, Best-alpha EN refers to EN results corresponding to the alpha value that minimizes mse (i.e., tabulate mse at lambda.min for various values of alpha from 0.1 to 0.9 to find the best alpha value)

# 2. Provide and Explain MSE vs Lambda Plots for Ridge, Best-alpha EN, Lasso

# 3. Present in One Table the parammeter estimates from Ridge, Best-alpha EN, Lasso -- all at their respective best lambdas

# 4. Plot predictions (ie yhat) in test sample vs. actual y.test for Ridge, Best-alpha EN, Lasso and report R2 for each method

# 5. Conclusions: Why does OLS fail when p > n? How do these methods circumvent that problem? Explain their "nested strcuture." Which method was best suited for this data? How well did these methods discover the true variables? fasle positives?








  