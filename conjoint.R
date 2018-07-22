setwd("/Users/mibiy/Desktop/DavisSpring/442 Advance Stat/Class 1 OLS & Conjoint Analysis")

data=read.csv("pn_hdtv.csv", header=T)    # reads csv file 
head(data, 5)                             # shows first 5 rows

prefer=data[,1];
screen52=data[,2]
screen65=data[,3]
tech3D=data[,4]
sony=data[,5]
price=data[,6]

out=lm(prefer~screen52+screen65+tech3D+sony+price, data)            
# runs linear regression model and keeps the results in "out"

summary(out)                           # displays the results stored in "out"




  