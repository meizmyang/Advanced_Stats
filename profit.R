setwd("/Users/prasad/Desktop/BAX442/Lectures/Class3")

# Step 1. Parameter values from nonlinear regression

b1=2
b2=10
b3=0.5
b4=1
b5=0.5; 	# should be less than 1. Why?

# Step 2. Build the profit function

profit = function(x) {
	
	sales = b1 + b2*exp(-b3*x[1]) + b4*(x[2])^b5	# sales model with x[1] = zprice, and x[2] = zad spends. 
	profit = x[1]*sales - x[2]						# profit = revenues-cost. Revenues = price*units sold. Cost = ad dollars spent
	return(profit)
}

# Step 3. Check profit function makes sense for positive (x1, x2) values

profit(c(runif(1,0,5),runif(1,0,5)))				

# Step 4. Maximize profit numerically 

x0=c(10,10)
out = optim(x0, profit, method = "L-BFGS-B", lower = rep(0,2), hessian=TRUE)


print("Max Profit, Optimal Price, Optimal Ad Spend"); cbind(out$value,out$par[1], out$par[2])



### Remarks

# "out" gives optimal price and optimal ad spend in zvalues. 
# x0 is starting values
# profit is the function built in Step 1
# method L-BFGS-B allows lower and upper bounds. If not needed, better use method = "BFGS"
# lower specifies lower bound is 0 for both x variables
# hessian =T provides the output of second partial derivatives at convergence. 
# If profit function is maximized, then hessian matrix should be negative definite. 
# How would you verify negative definiteness of a matrix?

### Most importantly, only numerical or only analytical maximization may not work. But,tTogether, they can and often do. 


  