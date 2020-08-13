# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression

fix(Boston)
names(Boston)

# lm.fit=lm(medv~lstat) : Error bc no data=Boston

lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)

# linear regression coefficients
lm.fit

# more specific info(residuals, coefficeints, errors, r^2, F statistic)
summary(lm.fit)

names(lm.fit)
coef(lm.fit)

# confidence interval
confint(lm.fit)

# confidence vs prediction
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

# plot
plot(lstat,medv)
abline(lm.fit)
# with different shape, color
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

# 4 plots about regression model
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)# regression with all variables
summary(lm.fit)

# variation inflation factor(multicollinearity)
library(car)
vif(lm.fit)

# without 'age'
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors
# square of lstat
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

# to fifth order poly.
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# log transformation
summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors

fix(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)

# Writing Functions

LoadLibraries
LoadLibraries()

LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()