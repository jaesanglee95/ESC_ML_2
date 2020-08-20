#The Validation Set Approach
library (ISLR)
set.seed (1)
train=sample (392 ,196)

lm.fit =lm(mpg~horsepower ,data=Auto ,subset =train ) # Auto 자료에서 train set 추출 후 분석

attach (Auto)
mean((mpg-predict (lm.fit, Auto))[-train ]^2) # train set으로 추정한 값의 정확도를 validation set으로 확인


lm.fit2=lm(mpg~poly(horsepower,2) , data=Auto , subset=train ) # 2차 다항식
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2) # 위와 같음

lm.fit3=lm(mpg~poly(horsepower,3), data=Auto, subset =train )
mean((mpg - predict ( lm.fit3 , Auto))[-train ]^2)

set.seed(2)
train=sample (392 ,196) # 다시 랜덤으로 train set 추출하기 위한 작업
lm.fit = lm( mpg ~ horsepower, subset =train)  # 위와 같은 과정 반복
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
lm.fit2=lm(mpg ~ poly(horsepower,2) , data=Auto , subset =train)
mean((mpg - predict (lm.fit2, Auto))[-train ]^2)
lm.fit3=lm(mpg ~ poly(horsepower,3), data=Auto ,subset =train )
mean(( mpg - predict (lm.fit3, Auto))[-train ]^2)


#Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower ,data=Auto) # generalized linear regression
coef(glm.fit)
glm.fit=glm(mpg~horsepower ,data=Auto)
coef(glm.fit)

library (boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit) # cross-validation 적용 -> 데이터를 하나만 남겨서 하는 방식인듯
cv.err$delta # 평균 mse ??
 
cv.error = rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
}
cv.error  # 1차 다항식부터 5차 다항식까지의 모델의 cv를 계산해본 결과

#k-Fold Cross-Validation
set.seed (17)
cv.error.10= rep (0 ,10)
for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm (Auto ,glm.fit, K=10)$delta[1]
  }
cv.error.10 # 1차 다항식부터 5차 다항식까지의 모델의 kcv를 계산해본 결과 
            # 오버피팅된 경우에는 오히려 mse가 증가하는 것을 볼 수 있다


#The Bootstrap
alpha.fn=function (data ,index){  # alpha 계산함수
  X=data$X [index] 
  Y=data$Y [index]
  return ((var(Y)-cov (X,Y))/(var(X)+var(Y) -2* cov(X,Y)))
}
alpha.fn(Portfolio ,1:100)
set.seed (1)
alpha.fn(Portfolio, sample(100, 100, replace =T))
boot(Portfolio, alpha.fn, R=1000)

boot.fn=function (data ,index )
  return (coef(lm(mpg ~ horsepower ,data=data ,subset =index)))
boot.fn(Auto, 1:392)
set.seed (1)
boot.fn(Auto, sample(392, 392, replace =T))
boot.fn(Auto, sample(392, 392, replace =T))
boot(Auto ,boot.fn ,1000)
summary(lm(mpg~horsepower ,data=Auto))$coef  # 결과를 비교

boot.fn=function (data ,index )
  coefficients(lm(mpg~horsepower +I( horsepower ^2) ,data=data ,subset =index)) # 새로운 모델 생성
set.seed (1)
boot(Auto ,boot.fn ,1000)
summary (lm(mpg~horsepower +I(horsepower ^2) ,data=Auto))$coef # 결과를 비교
