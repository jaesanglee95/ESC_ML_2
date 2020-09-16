# Chapter 9 Lab: Support Vector Machines
# 패키지 받기
install.packages("LiblineaR")
install.packages("e1071")
# Support Vector Classifier
# x, y 점 찍기
set.seed(10)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))
# 데이터 프레임으로 만들어서 svm 돌리자 
dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)
# 그려보자
plot(svmfit, dat)
# 아까 산점도를 -1과 1로 색으로 구분되어 표시한다
# 인덱스 정보
svmfit$index
summary(svmfit)
# 4개의 sv 사용. 각각 2개씩 다른 class였다.
# cost를 작게 해보자
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat)
svmfit$index
# 기울기가 좀더 가팔라졌다
# 이젠 cv까지 사용
set.seed(1)
tune.out=tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

summary(tune.out)
# cost가 0.1일 때 가장 좋다
# 가장 좋은 parameter로 돌려보자
bestmod=tune.out$best.model
summary(bestmod)
# 13개의 sv사용. 각각 7, 6개가 하나의 class
# 이젠 예측하자
# 먼저 test set 만들고
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
# 제일 좋았던 svm으로 예측
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)
# 20개 중에 15개를 잘 맞췄다
# cost 더 작게 하면?
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)
# 20개 중 16개 맞췄다. 성능 올랐다
# hyperplane 찾아보자
# 우선 구분 시켜주자
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
# cost 많이 줘서 확실하게 구분시키자
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
# 딱 2개의 sv로 구분! but 동그라미가 margin에 가깝다
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)
# cost를 줄였더니 sv가 늘었고, 동그라미가 margin에서 더 멀어졌다.
# Support Vector Machine
# 이젠 nonlinear kernel
# nonlinear한 class boundary 설정 후 그리기
set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

plot(x, col=y)
#kernel radial을 써서 train data fit
train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])

summary(svmfit)
# sv가 31개, 그래프를 보면 nonlinear 한 bound
#  그런데 training error가 있다
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
# error는 줄었지만, 더 이상한 boundary 나옴
# cv 시행
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
# 1 이 가장 좋은 cost, gamma는 0.5
# predict 해보자
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))
# 0.88의 정확도

# ROC Curves
install.packages("ROCR")
library("ROCR")
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
# ROC curve 그리자
svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
# 상당히 정확하다는 것을 알수있다
# 더 좋은 성능 by gamma increase
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
# test set에도 적용. gamma 2일때 가장 좋더라!
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with Multiple Classes
# 자료가 3가지 class
set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
# svm 시행
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
# 각각 test, trainl이 63, 20개
table(Khan$ytrain)
table(Khan$ytest)
# 관측치에 비해 feature가 너무 많음 2308개... 그래서 linear로!
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(out$fitted, dat$y)
# training error가 없다!
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
# cost 10으로 했는데 test error 2개만 나왔다! 