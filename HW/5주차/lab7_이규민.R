# Chapter 7 Lab: Non-linear Modeling

library(ISLR)
attach(Wage)

# Polynomial Regression and Step Functions
# orthogonal polynomials
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

# age, age^2, ... directly
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))

# same as the above with different shape
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
# also same
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
coef(fit2b)
# grid 설정 후 예측
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
# 그림그리기(파란색이 예측치)
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# 아까 말했듯이 basis가 orthogonal이든 아니든 상관없다
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))

# 얼마나 높은 차수로 예측하는 게 좋은지 정해보자 by ANOVA
fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)
# 3차나 4차가 제일 적합. 판단은 p value로.
# 직접 아래로 t value 구한 후 제곱해서 F값 비교해도 됨.
coef(summary(fit.5))
(-11.983)^2
# 말했듯이 orthogonal 아니어도 ANOVA로 비교할 수 있다.
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
# 3차 식이 제일 나은 것 같다.

# 이번엔 잘버나 못버나 logistic으로 분석
fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)
preds=predict(fit,newdata=list(age=age.grid),se=T)
# 값을 구하기 위해 logit 꼴로 바꾼다.
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
#type 지정으로 바로 예측치 구할 수도 있긴 하다.
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
# 그림그리기
plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
# 40살과 60 초반에서 큰 피크가 보인다.

# step function 만들기 by cut
table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

# Splines
# bs로 knot 지정하여 spline 구하기, default가 cubic
library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred=predict(fit,newdata=list(age=age.grid),se=T)
# 이제 그려보자
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")
# 6차원 확인. df=6은 6차원에 맞게 3개 knot를 uniform 하게 나눠줌
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")
# 33.75, 42, 51살이 균등하게 잘린 knot

# natural spline
# 차원이 4로 줄었다.
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

# smoothing spline
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
# fit은 df를 16으로 지정. fit2는 cv를 통해 정해짐(6.794596으로)

# local regression
# span으로 local하게 분석하는 구간 길이 정하기
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

#확실히 차원이 낮을수록, span이 클수록 smooth해진다.

# GAMs
# 뭐 그냥 선형 회귀도 gam이긴 하다.
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

# 제대로 gam하기 위해 library 불러오자
library(gam)
# s는 smoothing spline이다
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.Gam(gam1, se=TRUE, col="red")
# year가 없는 gam, 1차인 gam, spline으로 하는 gam 비교
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# 확실히 2번째 그냥 1차로 넣는게 좋아보인다.

summary(gam.m3)
# 다시 봐도 year는 굳이 차수가 높지 않아도 될 것 같다.

preds=predict(gam.m2,newdata=Wage)
# local regression으로 예측
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)
plot.Gam(gam.lo, se=TRUE, col="green")
# 이건 변수 2개. interaction between year and age와 education
gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)
# 2차원 그래프 그려보자
library(akima)
plot(gam.lo.i)
# logistic regression gam
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))
# 그래프, 표로 확인할 수 있듯, hs를 못나온 그룹은 많이 버는 사람이 없다
# 그래서 그 그룹을 제외한 logistic regression gam 했다.
gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
