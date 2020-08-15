# Chapter 5 Lab: Cross-Validation and the Bootstrap

#The Validation Set Approach
library (ISLR)
set.seed (1)
train=sample (392 ,196) #392까지 숫자 중 196개를 선별

lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train ) #subset을 통해 data중 train에 해당하는 부분으로 fitting

attach (Auto)
mean((mpg-predict (lm.fit, Auto))[-train ]^2) #vadation set에 대한 MSE

lm.fit2=lm(mpg~poly(horsepower,2) , data=Auto , subset=train ) #변수를 달리하여 모델링(2차항)
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)

lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset =train ) # 3차항
mean((mpg - predict ( lm.fit3 , Auto))[-train ]^2)

set.seed(2) #seed를 달리하여 train/ test split을 달리하여 같은 과정을 반복
train=sample (392 ,196)
lm.fit = lm( mpg ~ horsepower, subset =train)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
lm.fit2=lm(mpg ~ poly(horsepower,2) , data=Auto , subset =train)
mean((mpg - predict (lm.fit2, Auto))[-train ]^2)
lm.fit3=lm(mpg ~ poly(horsepower,3), data=Auto ,subset =train )
mean(( mpg - predict (lm.fit3, Auto))[-train ]^2)
# 두 과정의 MSE를 확인하면 1차보다 2차항이 포함될 때 개선되고, 3차항은 비슷하므로, 굳이 3차항까지 갈 이유는 부족해보인다. 


#Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower ,data=Auto) #logistic regression을 위해 사용하던 glm이지만 family를 설정해주지 않으면 일반 선형 회귀 역할을 한다.
coef(glm.fit)

glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit) #glm과 lm의 모델링 결과가 동일함을 확인할 수 있다.
#glm은 cv.glm을 통해 cross-validation을 쉽게 할 수 있다.

library (boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit) #cv.glm()에서 argument K의 default는 데이터의 개수로, LOOCV를 진행한다.
cv.err$delta

cv.error = rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
}
cv.error #glm 모델에 따라 prediction error를 for문을 통해 산출했다. 
#앞에서 봤듯이 2차 모델에서 개선되고 그 이후에는 큰 개선도가 보이지 않는다.

#k-Fold Cross-Validation
set.seed (17)
cv.error.10= rep (0 ,10)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm (Auto ,glm.fit, K=10)$delta[1] # K-fold CV (K=10)을 진행
  }
cv.error.10 #K=10으로 K-fold CV를 진행하여 모델을 비교했다.
# K-fold CV에서도 마찬가지 결과를 확인할 수 있다.


#The Bootstrap
alpha.fn=function (data ,index){
  X=data$X [index]
  Y=data$Y [index]
  return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
  #5.7 식을 보면 된다. alpha를 추정하는 계산식이라고 보면 됨.
} 
alpha.fn(Portfolio ,1:100) 

set.seed (1)
alpha.fn(Portfolio, sample(100, 100, replace =T)) #bootstrap기법의 1회 예시, 이런 반복추출을 통한 추정치를 아주 많이 구해서 분포를 확인해야 한다.
boot(Portfolio, alpha.fn, R=1000) #1000번 시행후 alpha의 s.e 확인 가능

boot.fn=function (data ,index ) #boot를 통해 lm의 정확도를 확인한다.
  return (coef(lm(mpg ~ horsepower ,data=data ,subset =index)))
boot.fn(Auto, 1:392)

set.seed (1) 
boot.fn(Auto, sample(392, 392, replace =T)) 
boot.fn(Auto, sample(392, 392, replace =T)) #위처럼 표본 추출을 달리하여 정확도를 비교
boot(Auto ,boot.fn ,1000)# 두 계수의 s.e를 확인 가능하다.
summary(lm(mpg~horsepower ,data=Auto))$coef #lm의 계수의 s.e와 다소 차이가 있음을 확인할 수 있다.
#lm의 가정을, bootstrap은 해결해주므로 오히려 bootstrap의 결과가 더 정확한 값이다.

boot.fn=function (data ,index )
  coefficients(lm(mpg~horsepower +I( horsepower ^2) ,data=data ,subset =index))
set.seed (1)
boot(Auto ,boot.fn ,1000)
summary (lm(mpg~horsepower +I(horsepower ^2) ,data=Auto))$coef
