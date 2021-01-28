############################## 442 - Advance Statistics - HW4 - Price & Ad Spend Optimization ###############################

# Set the working directory
setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class 3 Non-Linear Regression")

# Load the required packages
library(dplyr)
library(numDeriv)
library(GenSA)
library(ggplot2)
library(reshape2)

# Read the input csv file and label it as "data"
data=read.csv("class_data.csv", header=T) 
glimpse(data)
# Extract the attributes
time=data[,1]
sales=data[,2]
price=data[,3]
tv=data[,4]
print=data[,5]
radio=data[,6]
psearch=data[,7]
psocial=data[,8]
shopper=data[,9]
ntdola=data[,10]
ntdolv=data[,11]
tdola=data[,12]
tdolv=data[,13]
email=data[,14]
linqia=data[,15]
linqia_shopper=data[,16]
dfsi=data[,17]
newsfsi=data[,18]
wmx=data[,19]

##### Function for Nonlinear Regression  #####
nonlin_fn <- function(y,x1,x2,x3,ind) {
  # Step 1. Scale variables. Most important step. Scale them such that parameter estimates acquire similar magnitudes
  zy=y/sd(y)
  zx1=x1/sd(x1)
  zx2=x2/sd(x2)
  zx3=x2/sd(x3)
  
  # Step 2. Find good starting values. To this end, let's apply Global Optimization
  # Step 2A. Specify the model function
  nlsmean=function(b) {
    if (ind == 1)                                       # Price as exponential decay and Ad spend as diminishing return (concave)
      mu <- b[1] + b[2]*exp(-b[3]*zx1) + b[4]*(zx2)^b[5] + b[4]*(zx3)^b[5]
    else if (ind == 2)                                  # Price as exponential decay and Ad spend as Saturation S-shaped
      mu <- b[1] + b[2]*exp(-b[3]*zx1) + (b[4]*exp(-b[5]*zx2))/(b[6]+exp(-b[5]*zx2)) + (b[4]*exp(-b[5]*zx3))/(b[6]+exp(-b[5]*zx3))
    else if (ind == 3)                                  # Price as hyperbolic decay and Ad spend as diminishing return (concave)
      mu <- b[1] + b[2]/(zx1)^b[3] + b[4]*(zx2)^b[5] + b[4]*(zx3)^b[5]	
    else if (ind == 4)                                  # Price as hyperbolic decay and Ad spend as Saturation S-shaped
      mu <- b[1] + b[2]/(zx1)^b[3] + (b[4]*exp(-b[5]*zx2))/(b[6]+exp(-b[5]*zx2)) + (b[4]*exp(-b[5]*zx3))/(b[6]+exp(-b[5]*zx3))
    return(mu)                                          # mu denotes the mean function
  }
  
  # Step 2B. Specify SSE function
  sse=function(b) {
    sse=sum((zy-nlsmean(b))^2)
  }
  
  # Step 2C. Find good starting values using global minimizer like Simualted Annealing (or Genetic Algorithm or Particle Swarm)
  if (ind == 1 | ind == 3) {
    par=c(1,1,1,1,1)								  # starting values for the global optimizer
    lower=c(-10,-10,-10,-10,-10)  		# lower bounds on parameter values
    upper=c(10,10,10,10,10)           # upper bounds on parameter values
  }
  else {
    par=c(1,1,1,1,1,1)								 
    lower=c(-10,-10,-10,-10,-10,-10)  				
    upper=c(10,10,10,10,10,10) 
  }
  
  out=GenSA(par, sse, lower, upper)	
  
  # Step 3. Nonlinear Regression from first principles
  par=out$par												   # use GenSA solution as the starting values for nls()
  fit=optim(par, sse, method = "BFGS", hessian = T) 	# optim = solver that minimizes a specified fcn (here sse)
  est=fit$par									# final estimates from optim solver
  
  # Inference: Used for Confidence Intervals (Range for beta hat)
  nn=ncol(t(zy))								# sample size
  pp=ncol(t(est))								# number of parameters
  yhat=nlsmean(est)								# forecast zsales
  err=zy-yhat								# residuals
  sig2=sum(err^2) /(nn-pp)						# sigma^2 of error term
  jmat=jacobian(nlsmean,est)						# jmat = gradient of the mean function at the estimated parameters
  
  if (ind == 1 | ind == 3)
    varp=sig2*solve(t(jmat) %*% jmat+0.1*diag(5)) 		# variance-covariance matrix of parameters. I added small ridge regularization	to ensure inverse
  else
    varp=sig2*solve(t(jmat) %*% jmat+0.1*diag(6))
  
  se=sqrt(diag(varp))							# diag elements are variances, sqrt makes them std dev
  tvals=est/se									# tvals for inference: abs(tval) > 1. 65 => 90% confidence level; abs(tval) > 1.96 => 95% CI
  
  # Get the estimates, std errors and t-values in one matrix
  model_mat <- cbind(est,se,tvals)
  colnames(model_mat) <- c("Estimates","Std Errors","t-values")
  if (ind == 1 | ind == 3)
    rownames(model_mat) <- c("beta0","beta1","beta2","beta3","beta4") 		
  else
    rownames(model_mat) <- c("beta0","beta1","beta2","beta3","beta4","beta5") 
  
  # Information Criteria: Used for Model Selection
  aic = nn*log(sig2) + 2*pp						# Use when nn is large (i.e., pp/nn < 5%)
  aicc = nn*log(sig2) + nn*(nn+pp)/(nn-pp-2)			# Use when pp/nn > 5% => invented by Prof. Tsai at GSM and Hurvich at NYU
  bic = nn*log(sig2) + pp*log(nn)					# Use when nn is large (i.e., pp/nn < 5%)
  # Combine in one matrix
  ic_mat <- rbind(aic,aicc,bic)
  rownames(ic_mat) <- c("AIC","AICC","BIC")
  
  # Merge the two results into one function return list object
  model_out <- list(model_mat,ic_mat)
  return(model_out)
}


# EDA and Descriptive Stats
summary(data)
# Keeping sales, price and only non zero 25%ile Adv spend - tv, psearch, psocial, ntdola, tdola, email, dfsi
cormat <- round(cor(data[,c(1,2,3,4,7,8,10,12,14,17)]),2)
melted_cormat <- melt(cormat)
# Creating a correlation heat map
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Checking individual variable trends
ggplot(data, aes(x=time, y=sales)) + geom_line()
ggplot(data, aes(x=time, y=price)) + geom_line()
ggplot(data, aes(x=time, y=tv)) + geom_line()
ggplot(data, aes(x=time, y=psearch)) + geom_line()
ggplot(data, aes(x=time, y=psocial)) + geom_line()
ggplot(data, aes(x=time, y=ntdola)) + geom_line()
ggplot(data, aes(x=time, y=tdola)) + geom_line()
ggplot(data, aes(x=time, y=email)) + geom_line()
ggplot(data, aes(x=time, y=dfsi)) + geom_line()

ggplot(data, aes(sales)) + geom_histogram()
ggplot(data, aes(price)) + geom_histogram()

ggplot(data, aes(x=tv, y=sales)) + geom_point() + geom_smooth()
ggplot(data, aes(x=psearch, y=sales)) + geom_point() + geom_smooth()
ggplot(data, aes(x=psocial, y=sales)) + geom_point() + geom_smooth()
ggplot(data, aes(x=ntdola, y=sales)) + geom_point() + geom_smooth()
ggplot(data, aes(x=tdola, y=sales)) + geom_point() + geom_smooth()
ggplot(data, aes(x=email, y=sales)) + geom_point() + geom_smooth()
ggplot(data, aes(x=dfsi, y=sales)) + geom_point() + geom_smooth()

# tv and dfsi seem to be the best candidates for our non linear model

# Call the function with sales as y and price and spend variables as X
model1 <- nonlin_fn(sales, price, tv, dfsi, 1) # last parameter defines which model to use. See fn definition for details
model2 <- nonlin_fn(sales, price, tv, dfsi, 2)
model3 <- nonlin_fn(sales, price, tv, dfsi, 3)
model4 <- nonlin_fn(sales, price, tv, dfsi, 4)

aicc_comp <- rbind(t(model1[[2]]),t(model2[[2]]),t(model3[[2]]),t(model4[[2]]))
rownames(aicc_comp) <- c("Model1","Model2","Model3","Model4")
aicc_comp
sprintf("The model with minimum AICC is %s", rownames(aicc_comp)[aicc_comp[,2]==min(aicc_comp[,2])])

# Print the estimates, std errors, t-values for the model with the minimum information criteria
model1

# After model selection with minimum AICC, use that to find optimal price and Ad spend
# Step 1. Parameter values from nonlinear regression
b <- model1[[1]][,1]
c=1;     # variable cost to make additonal units

# Step 2. Build the profit function
profit <- function(x) {
  sales = b[1] + b[2]*exp(-b[3]*x[1]) + b[4]*(x[2])^b[5] + b[4]*(x[3])^b[5]	# sales model with x[1] = zprice, and x[2],x[3] = zad spends. 
  profit = (x[1]-c)*sales - x[2] - x[3]						# profit = revenues-cost. Revenues = price*units sold. Cost = ad dollars spent
  return(profit)
}

prc <- seq(from = 0, to = 100, by = 1)
tvSp <- seq(from = 0, to = max(tv), by = (max(tv)/100))
dfSp <- seq(from = 0, to = max(dfsi), by = (max(dfsi)/100))
profit_mat <- rep(0,length(prc))
for (i in 1:length(prc)){
  x0 <- c(prc[i],tvSp[i],dfSp[i])
  profit_mat[i] <- profit(x0)
}
df <- as.data.frame(cbind(profit_mat,prc,tvSp,dfSp))
p1 <- ggplot(df, aes(x=prc, y=profit_mat)) + geom_line(color="green") + geom_point(color="green") + ggtitle("Profit vs. Price")
p2 <- ggplot(df, aes(x=tvSp, y=profit_mat)) + geom_line(color="blue") + geom_point(color="blue") + ggtitle("Profit vs. TV Spend")
p3 <- ggplot(df, aes(x=dfSp, y=profit_mat)) + geom_line(color="black") + geom_point(color="black") + ggtitle("Profit vs. Digital Spend")
cowplot::plot_grid(p1,p2,p3, labels="AUTO") # Install the cowplot package, don't need to load it though

# Step 3. Check profit function makes sense for positive (x1, x2) values
profit(c(runif(1,0,5),runif(1,0,5),runif(1,0,5)))				

# Step 4. Maximize profit numerically
x0=cbind(mean(price),mean(tv),mean(dfsi))
out = optim(x0, profit, method = "BFGS", hessian=TRUE)
#out = optim(x0, profit, method = "L-BFGS-B", lower = rep(0,length(x0)), upper = rep(Inf,length(x0)), hessian=TRUE)
#out = optim(x0, profit, method = "SANN", hessian = TRUE)

print("Max Profit, Optimal Price, Optimal TV Ad Spend, Optimal Digital Ad Spend"); cbind(out$value,out$par[1], out$par[2], out$par[3])
