# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression
# explore Boston data 
fix(Boston)
names(Boston)

lm.fit=lm(medv~lstat) #Error

# lm() 함수를 사용하는 방법
lm.fit=lm(medv~lstat,data=Boston)

attach(Boston)
lm.fit=lm(medv~lstat)

# lm() 결과 확인 하기
lm.fit

summary(lm.fit) 

names(lm.fit) #lm.fit의 확인 가능한 결과 목록

coef(lm.fit) # lm.fit$coefficients 동일
confint(lm.fit) #계수의 신뢰구간

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence") #신뢰구간
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction") #예측구간

plot(lstat,medv) #실제 x,y의 분포
abline(lm.fit) #lm.fit(SLR)의 회귀선

#plot 옵션들
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

#lm.fit을 활용한 잔차 도표 등등
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


# Multiple Linear Regression

#변수를 선택하여 fitting
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

#모든 변수에 대해 fitting
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

library(car)
vif(lm.fit) #변수 중요도

lm.fit1=lm(medv~.-age,data=Boston)# age를 제외한 모든 변수 fitting
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age) #위와 다른 방식

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston))#*를 사용해 상호작용항 추가

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2)) #비선형 x
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2) 
#lm.fit의 잔차도표에서는 2차식같은 곡선이 눈에 띄었으나,
#lm.fit2는 해결된듯 보인다.

lm.fit5=lm(medv~poly(lstat,5)) #5차항까지 넣은 모델
summary(lm.fit5)

summary(lm(medv~log(rm),data=Boston)) #log변환도 가능

# Qualitative Predictors
#질적 변수에 대한 처리
fix(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit) # shelveLoc이 Good, Medium일 때 계수값이 산출됨

attach(Carseats)
contrasts(ShelveLoc) #두 dummy변수로 3가지 상황을 표현함.

# Writing Functions, 잡기술

LoadLibraries
LoadLibraries()

LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
