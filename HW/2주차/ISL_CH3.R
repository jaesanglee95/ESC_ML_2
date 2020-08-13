# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR) # 그전에 먼저 install.packages를 먼저 해야하는 듯

# Simple Linear Regression

fix(Boston)  # Text editor에서 컬럼명과 변수명을 변경할 수 있다.
names(Boston) # 변수명 확인


lm.fit=lm(medv~lstat) #Error

lm.fit=lm(medv~lstat,data=Boston) # 회귀분석 
attach(Boston)
lm.fit=lm(medv~lstat)

lm.fit # 적합된 계수 확인

summary(lm.fit) # anova table 확인

names(lm.fit) # lm.fit에 있는 변수명 확인 
coef(lm.fit) # 계수 불러오기

confint(lm.fit) # 신뢰구간 확인

predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence") # 주어진 회귀분석의 결과로 Y_hat 추정
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction") # 예측값은 같으나 신뢰구간의 크기가 달라짐

plot(lstat,medv)  # 산점도
abline(lm.fit) # 1분위계수와 3분위계수를 이용한 적합선

abline(lm.fit,lwd=3) # 선의 두께만 변경 => lwd
abline(lm.fit,lwd=3,col="red") # 색상 변경 => col
plot(lstat,medv,col="red") # 점의 색상 변경 => col
plot(lstat,medv,pch=20) # 점의 종류 변경     => pch
plot(lstat,medv,pch="+") # 점의 종류 변경
plot(1:20,1:20,pch=1:20) # 점의 종류를 확인하기 위한 방법 

par(mfrow=c(2,2))  # 2*2의 형태로 윈도우를 분할
plot(lm.fit) # 이렇게 할 수 있는거 처음 알았다..... 써먹어 보자
# Scale_Location => 표준화 잔차와 비교 ,이상치를 보는것. 잘 적합되었는지 알아보는 것 기울기 0인게 좋다. 
# 
plot(predict(lm.fit), residuals(lm.fit)) # 이게 뭘 의미하는걸까
plot(predict(lm.fit), rstudent(lm.fit)) # 잔차를 표준오차로 나눈값과 비교
plot(hatvalues(lm.fit)) # 이상값을 관찰하는 함수. 단, 영향관측치 일수도 있다고 함 
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston) # 변수 전부 다 입력. cross term은 없음
summary(lm.fit)

library(car)
vif(lm.fit)  # 다중공선성 수치를 확인하는 함수

lm.fit1=lm(medv~.-age,data=Boston) # -붙은 변수만 제외하고 회귀분석
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)  # 이런 식으로 변수 하나를 제거한 회귀분석을 표현할 수 있는 듯

# Interaction Terms

summary(lm(medv~lstat*age,data=Boston)) # Interaction term을 고려한 회귀분석

# Non-linear Transformations of the Predictors

lm.fit2=lm(medv~lstat+I(lstat^2)) # polynomial 
summary(lm.fit2)

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2) # 처음 알게 된 기능. 이렇게 모델 2개를 비교할 수 있구나

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5=lm(medv~poly(lstat,5)) # 5차항까지 생성하는 함수 => poly
summary(lm.fit5)

summary(lm(medv~log(rm),data=Boston)) # 독립변수에 로그 취한 형태

# Qualitative Predictors
Carseats
fix(Carseats)  
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats) # 원하는 cross term만 +로 추가, interaction은 :으로 표시
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc) # 실험계획법에 나오는 contrast 개념..?

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
