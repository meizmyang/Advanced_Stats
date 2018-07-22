setwd("/Users/prasad/Desktop/BAX442/Lectures/Class1")

exam=read.csv("exam.csv", header=T)    # read csv file and label the data as "exam"

## PART ONE: OLS from first principles

x=as.matrix(exam[,1])        # midterm as x variable
y=as.matrix(exam[,2])        # finals as y variable
n=nrow(x)                    # number of observations

one=matrix(1,n,1)           # creates a column of ones for the intercept
xx=as.matrix(cbind(one,x))  # attaches ones to the x data
p=ncol(xx)                  # number of variables in xx data

### Estimation
xpx=t(xx) %*% xx             # X'X matrix, t() denotes transpose
xpxi=solve(xpx)              # solve() inverts the matrix X'X
xpy=t(xx) %*% y              # X'Y matrix
bhat=xpxi %*% xpy            # estimate bhat = Inverse(X'X)*X'Y as per OLS estimator in class notes

### Prediction
yhat=xx %*% bhat             # forecast y values = X*bhat
err=y-yhat                   # compute error = y - yhat
sse=t(err) %*% err           # compute sum of squared errors to compute error variance s2
s2=sse/(n-p)                 # error variance, sigma^2 in class notes

### Inference
se=sqrt(s2*diag(xpxi))       # extrat standard errors of bhat via the diagonal of the square-root of sigma^2 * Inverse(X'X)
tval=bhat/se                 # obtain t-values = estimates divided by their std errors

my_out=cbind(bhat, se, tval)    # output from my OLS from scratch 


# Compare with the built-in lm() function         

lm_out=summary(lm(y~x))          # R built-in OLS

print("My Estimates, Std Errors, t-values"); my_out


print("R built-in Output"); lm_out$coefficients



## A function automates the code
## It has input arguments (to be supplied by users).
## It has output arguments (i.e., output of your code)
## 
## For us, inputs are x=as.matrix(exam[,1]) and y=as.matrix(exam[,2])
## For us, outputs are Estimates (bhat), Std Errors (se), t-values (tval)


### Note the structure of any function as explained below:


# # A function starts with a mnemonic string name = function (x, y, z, your input arguments) { 
# 
#   my code here
#   
#   then specfy output via return()
#   
#   end the function with a curly bracket to match with the starting curly bracket
#   
#   return () }. 

## The curly brackets indicate the start and end of your function


# PART TWO: Build my very own lm-type *function*

my_lm = function(y,x) {
  
  n=nrow(x)                    # number of observations
  
  one=matrix(1,n,1)           # creates column of ones for the itercept
  xx=as.matrix(cbind(one,x))   # attaches ones to the x data
  p=ncol(xx)                   # number of variables in xx data
  
  
  xpx=t(xx) %*% xx             # X'X matrix, t() denotes transpose
  xpxi=solve(xpx)              # solve() inverts the matrix X'X
  xpy=t(xx) %*% y              # X'Y matrix
  bhat=xpxi %*% xpy            # estimate bhat = Inverse(X'X)*X'Y as per OLS estimator in class notes
  
  yhat=xx %*% bhat             # forecasted y values = X*bhat
  err=y-yhat                   # residuals = y - yhat
  sse=t(err) %*% err           # sum of square residuals to compute error variance s2
  
  s2=sse/(n-p)                 # error variance, sigma^2 in class notes
  se=sqrt(s2*diag(xpxi))       # standard errors of bhat given by diagonal of sigma^2 * Inverse(X'X)
  tval=bhat/se                 # t-values = estimates divided by their std errors
  
  out=cbind(bhat, se, tval)    # output from my OLS 
  
  return(out)
}



# Check my_lm() gives the same results or not, else debug

y = as.matrix(exam[,2])
x = as.matrix(exam[,1])
mylm_out=my_lm(y,x)


print("Results from my very own *my_lm()* function!"); mylm_out



  