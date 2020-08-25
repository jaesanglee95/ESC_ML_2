# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection
# 잘 설명하는 변수들만 뽑자!
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)# 결측치 제거
dim(Hitters)
sum(is.na(Hitters))
# leaps의 regsubsets가 알아서 좋은 변수만 골라준다!
# 각 행이 변수 개수, 행 별로 *표시된 변수들 셋이 제일 좋은 변수 셋
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
# default는 변수8개이므로 변수 더 늘리고 싶을때 nvmax 활용
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)
# 19개 다 넣었을 때의 r squared 측정
reg.summary$rsq
# rss와 adjusted r squared plot
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
# rss 는 변수가 늘수록 줄어든다
# adjusted r squared 는 변수가 늘수록 커지다가 너무 변수가 많아지면 줄어든다.(미세하게 관찰된다.)
# 9~10 부근에서 감소하는 것을 볼 수 있다.

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
# 찾아보니 11에서 최대라고 한다...

# Mallow's Cp를 그리고 언제 최소인지 보자.
# r squared 빼고는 다 최소인게 좋은 것!
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
# 10에서 최소!

# BIC 도 확인한다.
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
# 6에서 최소!

# 그림 그리는데 margin이 너무 크다고 해서 줄였다.
par("mar")
par(mar=c(1,1,1,1))
# 변수와 지표간 그래프
# 검정색일수록 택한다는 뜻.
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
# 변수 개수별 coefficient 확인
coef(regfit.full,6)

# Forward and Backward Stepwise Selection
# 변수 제거/선택 방식 별로 해보자
# forward 부터
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
# backward 도
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
# 각 방법 별로 뽑히는 변수가 조금 다르다...

# Choosing Among Models
# 본격 cv 까지 사용하며 제대로 변수 선택해보자
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
# 반반으로 train, test 나눔
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors)
# validation 해보니 변수 7개일 때가 제일 좋다
coef(regfit.best,7) # 그떄의 계수들

# 우리가 직접 predict 함수 만든다....하지만 밑에 좋은게 있네요
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
# 변수 10개를 쓰면 뭐가 제일 좋을까?
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

#이젠 10 fold validation으로 최적 변수 정하기
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
# 변수 개수: 열, validation 쓰인 fold가 행
mean.cv.errors=apply(cv.errors,2,mean) # 평균 취한 후
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b') # 그림 그리기
#10개일때 가장 평균 error가 작다.

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,10)
# 그때의 모델과 coef들 확인

# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1] # 심지어 질적 변수 바로 dummy 변수로 변환
y=Hitters$Salary
# 타자 정보에 따른 연봉 맞추기 시작!

# Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # alpha=0이면 ridge

dim(coef(ridge.mod)) # lambda 100개에 대한 ridge 시행

ridge.mod$lambda[50] # default가 standardize되는 것.
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# lambda 50일 때 ridge
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# test set, training set 나눠서 test error 구하기
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
# ridge 진행
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
# lambda 4일 때랑 비교
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
# 그냥 train 평균으로 예측했다면?
mean((mean(y[train])-y.test)^2)
# 훨씬 mse가 크다!

# 이번엔 엄청 큰 lambda로 학습
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# 엄청 비슷한 mse가 나옴

# lambda 0일 때와 비교(linear regression)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2) # ridge 보다 큰 mse
# 당연히 그런데 linear regression하면 lm으로 한다.
# 더 좋은 정보들이 많이 나온다.
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]

# 본경 cv 사용해서 제대로된 lambda 구하자
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0) # default는 10 fold
plot(cv.out) # 멋있는 그림
bestlam=cv.out$lambda.min
bestlam
# 326정도가 최적의 lambda라고 나왔다. 그렇다면 이제 이것으로 ridge 진행한다.

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2) # 그 전의 lambda들을 썼을 때보다 더 작은 mse가 나옴
# 이 때의 coefficient들 구하기
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso
# ridge에서 alpha만 1로 바꿔주면 됨
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod) # 어떤 변수는 거의 0으로!

# 바로 cv 이용해서 test error 구하자
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out) # 또 멋있는 그림
bestlam=cv.out$lambda.min
bestlam # 9.28정도가 최적의 lambda로 나왔다
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
# ridge보다는 큰 mse가 나왔다..

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef # bestlam일때의 coefficient들 도출
lasso.coef[lasso.coef!=0] # 그중 0이 안 된거만! 무려 8개가 없어졌다.


# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
# scale=True이므로 standardization 진행
# validation까지 자동으로(default는 10fold)
summary(pcr.fit)
# mse의 root 출력해주는 것에 유의

# validation score 출력해주는 다른 함수
validationplot(pcr.fit,val.type="MSEP")
# 확실히 16개 변수일 때 msep가 최소인 게 보인다.

# 이젠 train set, test set 구분해서 진행
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
# 5일때 최소인 것 같다.

# 구한 최적의 변수 개수일 때의 pcr mse 출력
pcr.pred=predict(pcr.fit,x[test,],ncomp=5)
mean((pcr.pred-y.test)^2)

# 이젠 구한 parameter들로 전체 data에 적용
pcr.fit=pcr(y~x,scale=TRUE,ncomp=5)
summary(pcr.fit)

# Partial Least Squares
# pcr이랑 거의 비슷
set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
# 변수 1개가 제일 잘 설명....

pls.pred=predict(pls.fit,x[test,],ncomp=1)
mean((pls.pred-y.test)^2)
# 최적의 parameter에 대한 mse... 이전의 다른 방법들에 비해서 꽤 높은 편이다

pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=1)
summary(pls.fit)
# 그래도 확실히 % variance explained를 보면 43%이다. 5개변수를 쓴 pcr보다 1%p 정도 안좋을뿐!