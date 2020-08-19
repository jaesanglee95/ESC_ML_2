# Chaper 5 Lab: Cross-Validation and the Bootstrap

# The Validation Set Approach

library(ISLR)
set.seed(1)

train=sample(392,196) #1부터 392까지 랜덤하게 196개 추출
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)

# train set으로 학습 후 train set 빼고 검증
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# 회귀식 차수 증가
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

# seed를 다르게 해서 다른 train set으로 학습 후 검증
# 당연히 결과도 조금 다르다
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# 결론은 quadratic이 제일 낫다. cubic도 좋긴 하지만, 확신이 적다.

# Leave-One-Out Cross-Validation
# 우선 glm으로 똑같이 선형회귀
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

# 그런데 glm 쓰는 이유는 cv가 바로 가능해서!
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta # n개의 mse의 평균

# 1~5차일 때 차례대로 cv값 산출
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
# 선형만 아니면 몇 차든 비슷한 정도

# k-Fold Cross-Validation
# cv.glm()에 k만 추가해주면 끝

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
# 역시나 선형회귀만 아니면 비슷하게 낮게 나온다.

# The Bootstrap
# alpha 통계량 계산하는 함수
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# 1에서 100까지의 alpha 값
alpha.fn(Portfolio,1:100)

# 1에서 100까지 랜덤하게 추출했을 때의 alpha 값
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

# 1000번 뽑아서 bootstrap
boot(Portfolio,alpha.fn,R=1000)
# 진짜 값과 비교/ 조금은 다를 수밖에 없다
summary(lm(mpg~horsepower, data=Auto))$coef


# Estimating the Accuracy of a Linear Regression Model
# 그냥 선형회귀 bootstrap
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
# 상당히 비슷하다
summary(lm(mpg~horsepower,data=Auto))$coef

# 이번에는 quadratic 
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
# 이번에도 standard deviation과 estimate 모두 많이 유사하다.