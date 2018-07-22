setwd("C:\\Users\\mibiy\\Desktop\\DavisSpring\\442 Advance Stat\\class 3")
library(numDeriv)

# mu denotes the mean function
nlsmean=function(b) {
	mu <- b[1] + b[2]*exp(-b[3]*zprice) + b[4]*(zad)^b[5] 		
	return(mu)
}

print("My Estimates, Std Errors, t-values"); my_nlsout

# import csv data
df_full <- read.csv("class_data.csv")
View(df_full)
head(df)
summary(df_full)
str(df_full)

# subset dataframe with 3 variables needed
df <- df_full[ , c("Unit.Volume","Price","TV.Spend")]

# rename column names
df <- setNames(df, c("sales","price","tv"))
View(df)

# check missing values in each column
colSums(is.na(df))

# Step 1: Scale variables
zsales <- df$sales/sd(df$sales)
zprice <- (df$price - mean(df$price))/sd(df$price)
zad <- df$tv/sd(df$tv)

# Step 2: Try to get good staring values with a linear regression
pout <- lm(zsales~exp(-0.05*zprice)+sqrt(zad))
summary(pout)

# Step 3: Do NLS built in R
nlsout=nls(zsales~b1+b2*exp(-b3*zprice)+b4*(zad)^b5,
           start=list(b1=0.5,b2=1.2,b3=0.05,b4=0.28,b5=0.5))
summary(nlsout)











