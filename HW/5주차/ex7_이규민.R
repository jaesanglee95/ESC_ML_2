# ch.7
# exercise 7.9.9
library(MASS)
attach(Boston)
# (a) cubic regression
poly.fit=glm(nox~poly(dis,3),data=Boston)
summary(poly.fit)
# cubic이 적합한 것으로 보인다.
par(mfrow=c(1,1))
plot(Boston[,c('dis','nox')])
pred=predict(poly.fit,
             data.frame(dis=seq(min(dis),max(dis),length.out = 100)))

lines(seq(min(dis),max(dis),length.out = 100),
      pred,col='red')
# 비교적 곡선이 데이터 분포를 잘 나타낸다.

# (b) different degrees of poly.
x=seq(min(dis),max(dis),length.out = 100)
clrs=rainbow(10)
plot(Boston[,c('dis','nox')])

# 차수 별 다른 색으로 regression한 plot을 그린다.
rss=c()
for(d in 1:10){
  poly.fit=glm(nox~poly(dis,d),data = Boston)
  pred=predict(poly.fit,data.frame(dis=x))
  lines(x,pred,col=clrs[d])
  
  rss=c(rss,sum(poly.fit$residuals^2))
}
legend(x='topright',legend = 1:10,col=clrs,lty = c(1,1),lwd = c(2,2))
# rss도 비교해보자
plot(rss,xlab='Degree of Polynomials',ylab='RSE',type='l')
# rss 역시 degree 커질수록 작아진다.

# (c) cv 로 optimal degree 찾기
library(boot)
set.seed(32)
poly.mse=c()
for(degree in 1:10){
  poly.fit=glm(nox~poly(dis,degree,raw=T),data=Boston)
  mse=cv.glm(poly.fit,data = Boston,K=10)$delta[1]
  poly.mse=c(poly.mse,mse)
}
# k=10인 cv 수행 후 그래프로 찾자
plot(poly.mse,type='l',xlab='Polynomial Degree',ylab='Cross Validation MSE')
points(which.min(poly.mse),poly.mse[which.min(poly.mse)],col='red',pch=20,cex=2)
# 빨간색 점으로 표시된 degree=3일 때가 최소!

# (d) regression spline, df=4 로 수행
library(splines)
library(MASS)
spline.fit=lm(nox~bs(dis,df =4),data=Boston)
x=seq(min(Boston[,'dis']),max(Boston[,'dis']),length.out = 100)
y=predict(spline.fit,data.frame(dis=x))
# 그리면 초록 선이 regression spline 이용한 선이다.
plot(Boston[,c('dis','nox')],ylim=c(0,1))
lines(x,y,col=clrs[4])
# knot 는 1개이다. df로 나눠서  uniform 하게 정해졌다.
attr(bs(dis,df=4),"knots")

# (e) regression spline의 df 최적화
# 아까랑 비슷하게 for 문으로 다 해보자
plot(Boston[,c('dis','nox')],ylim=c(0,1))
clrs=rainbow(10)

x=seq(min(Boston[,'dis']),max(Boston[,'dis']),length.out = 100)

rss=c()
for(df in 3:10){
  spline.fit=lm(nox~bs(dis,df=df),data=Boston)
  y=predict(spline.fit,data.frame(dis=x))
  lines(x,y,col=clrs[df])
  
  rss=c(rss,sum(spline.fit$residuals^2))
}
legend(x='topright',legend=3:10,text.col=clrs[3:10],text.width=0.5,bty = 'n',horiz = T)
# df=3, 4일때를 제외하면 거의 완벽하게 똑같다. 3, 4일때에도 끝부분만 다르지 거의 비슷하다.
plot(3:10,rss,xlab='Df',ylab='Train RSS',type='l')
# 역시 df가 가장 큰 10일 때 rss가 가장 작았다.

# (f) cv까지 해서 df 최적화
library(boot)
set.seed(32)
spline.mse=c()
for(df in 3:10){
  Boston.model=model.frame(nox~bs(dis,df=df),data=Boston)  # Note that because we are automatically setting the 
  names(Boston.model)=c('nox','bs.dis')                    # cutoffs we must do so in the entire dataset, otherwise
  # predictions cannot be made.
  
  spline.fit=glm(nox~bs.dis,data=Boston.model)
  mse=cv.glm(spline.fit,data=Boston.model,K=10)$delta[1]
  spline.mse=c(spline.mse,mse)
}
# cv 후 가장 mse 낮았던 df 찾자.
plot(3:10,spline.mse,type='l',xlab='Df',ylab='Cross Val. MSE for Splines')
x=which.min(spline.mse)
points(x+2,spline.mse[x],col='red',pch=20,cex=2)
# 이번에는 train만 돌렸을 때와 같이 df=10일때가 최적이었다.