#lab 7  Non-linear Modeling
library (ISLR)
attach(Wage)


#Polynomial Regression and Step Functions
fit=lm(wage~poly(age,4),data=Wage) #4차항까지 orthogonal matrix
coef(summary (fit)) 

fit2=lm(wage~poly(age,4,raw=T),data=Wage) #raw = T : 데이터 그대로
coef(summary (fit2)) 

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage) 
coef(fit2a) 

fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage) 

#age에 대한 wage를 그래프로 나타나기 위한 작업 
agelims =range(age)
age.grid=seq(from=agelims [1],to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE) 
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex=.5,col="darkgrey ")
title("Degree -4 Polynomial",outer=T)
lines(age.grid ,preds$fit ,lwd=2,col="blue") #polynomial prediction
matlines (age.grid ,se.bands,lwd=1,col="blue",lty=3)  # se가 점점 커지는 모습을 확인

preds2= predict(fit2 ,newdata =list(age=age.grid),se=TRUE) 
max(abs(preds$fit -preds2$fit )) 

fit.1=lm(wage~age,data=Wage) 
fit.2=lm(wage~poly(age,2),data=Wage) 
fit.3=lm(wage~poly(age,3),data=Wage) 
fit.4=lm(wage~poly(age,4),data=Wage) 
fit.5=lm(wage~poly(age,5),data=Wage) 
anova(fit.1,fit.2,fit.3,fit.4,fit.5) #1차 ~ 5차 poly모형

coef(summary (fit.5)) # Anova의 F-value는 poly : 계수의 t-value 제곱
(-11.983)^2 
#education도 추가
fit.1=lm(wage~education+age,data=Wage) 
fit.2=lm(wage~education+poly(age,2),data=Wage) 
fit.3=lm(wage~education+poly(age,3),data=Wage) 
anova(fit.1,fit.2,fit.3)
#wage>250 여부를 targer으로 poly-logistic regression
fit=glm(I(wage >250)~poly(age,4),data=Wage,family=binomial) 
preds=predict (fit ,newdata =list(age=age.grid),se=T)

pfit=exp(preds$fit)/(1+exp(preds$fit)) #logit 꼴 이용
se.bands.logit = cbind(preds$fit +2* preds$se.fit , preds$fit -2* preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict (fit,newdata =list(age=age.grid),type="response", se=T)

plot(age ,I(wage>250),xlim=agelims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage >250)/5),cex=.5,pch="|", col="darkgrey")
lines(age.grid ,pfit ,lwd=2, col="blue")
matlines (age.grid ,se.bands,lwd=1,col="blue",lty=3)

#age를 4구간으로 나눠서 linear model
table(cut(age ,4))
fit=lm(wage~cut(age,4),data=Wage) 
coef(summary (fit)) 



#Splines
library(splines) 
#B-spline basis matrix를 만든다.
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage) #bs : B-spline  
pred=predict(fit ,newdata =list(age=age.grid),se=T) 
plot(age ,wage ,col="gray") 
lines(age.grid ,pred$fit ,lwd=2) 
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

dim(bs(age ,knots=c(25,40,60))) 
dim(bs(age ,df=6)) 
attr(bs(age ,df=6) ,"knots") 

#natural spline
fit2=lm(wage~ns(age,df=4),data=Wage) #ns : natural spline
pred2=predict(fit2 ,newdata=list(age=age.grid),se=T) 
lines(age.grid , pred2$fit ,col="red",lwd=2)

plot(age ,wage ,xlim=agelims ,cex=.5,col="darkgrey") 
title("Smoothing Spline") 
fit=smooth.spline(age ,wage ,df=16) # 자유도를 지정
fit2=smooth.spline(age ,wage ,cv=TRUE) # cv를 통한 적절한 람다를 찾고, 자유도 설정
fit2$df # 이 데이터 경우 6.8
lines(fit ,col="red",lwd=2)
lines(fit2 ,col="blue",lwd=2) 
legend ("topright",legend=c("16 DF","6.8 DF"), col=c("red","blue"),lty=1,lwd=2,cex=.8)

plot(age ,wage ,xlim=agelims ,cex=.5,col="darkgrey")
title("Local Regression") 
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage) 
lines(age.grid ,predict (fit ,data.frame(age=age.grid)), col="red",lwd=2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)), col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"), col=c("red","blue"),lty=1,lwd=2,cex=.8)



#GAMs
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage) 

library (gam)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3)) 
plot(gam.m3, se=TRUE ,col="blue") #각 변수마다 다른 모영의 식이 쓰임을 볼 수 있다.
plot.gam(gam1 , se=TRUE , col="red")

gam.m1=gam(wage~s(age,5)+education, data=Wage)
gam.m2=gam(wage~year+s(age,5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F") # year변수의 필요성에 대해 볼 수 있는 모델 비교
#anova 결과만 보면, M2가 괜찮아 보임
summary (gam.m3)
# 변수들을 보면, age는 non-linear한 증거가 분명래 보인다.

preds=predict (gam.m2,newdata =Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage) 
plot(gam.lo, se=TRUE , col="green") 

gam.lo.i=gam(wage~lo(year,age,span=0.5)+education, data=Wage) 

library (akima) 
plot(gam.lo.i) 

gam.lr=gam(I(wage >250)~year+s(age,df=5)+education, family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education ,I(wage >250))
gam.lr.s=gam(I(wage >250)~ year+s(age,df=5)+education,family= binomial,data=Wage,subset =(education !="1. < HS Grad")) 
plot(gam.lr.s,se=T,col="green")

#GAM : 변수마다 각각 다른 조건을 부여하여, 시각화 시에 다양하게 확인이 가능해보인다.