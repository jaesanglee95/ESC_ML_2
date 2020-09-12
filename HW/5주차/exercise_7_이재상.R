# 5주차 과제 이재상

#7.9
library(MASS)
str(Boston)

#(a)
#fit a cubic polynomial regression
fit.3=lm(nox~poly(dis,3),data=Boston) 

#regression ouput
summary (fit.3)

#plot 
attach(Boston)
dislims =range(dis)
dis.grid=seq(from=dislims [1],to=dislims [2])
preds=predict (fit.3 ,newdata =list(dis=dis.grid),se=TRUE) 
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)

plot(dis ,nox ,xlim=dislims ,cex=.5,col="darkgrey ")
lines(dis.grid ,preds$fit ,lwd=2,col="blue")
matlines (dis.grid ,se.bands,lwd=1,col="blue",lty=3)


#(b)
df.b <- data.frame(df = rep(0,10), rss= rep(0,10))
for (i in 1:10){
  fit = lm(nox ~ poly(dis, i), data = Boston)
  preds = predict(fit, newdata = list(dis = dis.grid), se = TRUE)
  lines(dis.grid, preds$fit, lwd = 2, col = rainbow(10)[i])
  
  df.b[i,] <- c(i, sum(fit$residuals^2))
}

df.b


#(c)
library(boot)

df.c <- data.frame(df = rep(0,10), cv.error = rep(0,10))

for (i in 1:10){
  cv.fit=glm(nox~poly(dis,i) ,data=Boston)
  df.c[i,]=c(i,cv.glm (Boston ,cv.fit)$delta [1])
  
}

df.c


#(d)
#fit spline 
bs.fit <- lm(nox ~ bs(dis, df = 4), data = Boston)
summary(bs.fit)

attr(bs(dis ,df=4) ,"knots") 

pred=predict(bs.fit ,newdata =list(dis=dis.grid),se=T) 
plot(dis ,nox ,xlim=dislims ,cex=.5,col="darkgrey ")
lines(dis.grid ,pred$fit ,lwd=2) 



#(e)
df.e <- data.frame(df = rep(0,10), rss= rep(0,10))
for (i in 1:10){
  fit = lm(nox ~ bs(dis, df = i), data = Boston)
  preds = predict(fit, newdata = list(dis = dis.grid), se = TRUE)
  lines(dis.grid, preds$fit, lwd = 2, col = rainbow(10)[i])
  
  df.e[i,] <- c(i, sum(fit$residuals^2))
}

df.e

#(f)


df.f <- data.frame(df = rep(0,10), cv.error = rep(0,10))

for (i in 1:10){
  cv.fit=glm(nox~bs(dis,df = i) ,data=Boston)
  df.f[i,]=c(i,cv.glm (Boston ,cv.fit)$delta [1])
  
}

df.f
