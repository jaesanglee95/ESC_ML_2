# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection

library(ISLR)
fix(Hitters)#데이터 살펴보기
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) # Salary에 NA : 59, 

Hitters=na.omit(Hitters)
dim(Hitters) #59 rows 제거
sum(is.na(Hitters))

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters) 
#best subset selection, 변수선택기준은 RSS 
summary(regfit.full)

#최대 19까지, 변수의 개수를 정했을 때, 뽑힐 best 변수들
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)

names(reg.summary)

reg.summary$rsq #각 모델의 R-squraed

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

which.max(reg.summary$adjr2) #n=11일 때, adj-R이 가장 높다.
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp) #Cp기준으로는 n = 10
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic) #BIC 기준으로는 n= 6
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# 모델 선택 기준에 따른 선택된 변수
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

coef(regfit.full,6)
# 6개의 변수만 고를 때 위의 결과와 같다.


# Forward and Backward Stepwise Selection

# Forward
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
# Backward
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
# method에 따라 선택되는 변수가 다르다!



# Choosing Among Models

# train / test split
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
# test matrix를 미리 만듦
test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
#변수 1~19개 까지사용한 모델을 test에 적용 시켜서 error를 계산
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

val.errors
which.min(val.errors) #testset에 대해 봤을 때, n = 7인 모델이 가장 error가 낮음
coef(regfit.best,7) #n=7 모델의 변수

#regsubsets에는 predict()가 적용이 되지 않으므로, 다음과 같은 function을 임의로 만든다.
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

#k-fold CV (k = 10)
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

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors #K-fold CV를 통한 각 모델의 error의 평균
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

which.min(mean.cv.errors)
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,10)



# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) #Ridge : alpha = 0
#lamda 값을 줄여가며, 각 계수들의 변화 추이를 확인한다.
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
#lambda가 줄어들면서 전체적인 계수가 커지는 것을 볼 수 있다.
predict(ridge.mod,s=50,type="coefficients")[1:20,] # 50번째 lambda( =11497.57)로 Ridge regression할 시, 모델의 계수

# train / test split
set.seed(12)
train=sample(1:nrow(x), nrow(x)/2) # 5:5 split
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) #lambda를 4로 하고 testset에 대해 predict
mean((ridge.pred-y.test)^2) # Test MSE : 142199.2

mean((mean(y[train])-y.test)^2) # Test MSE 예시

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,]) #lambda를 10^10
mean((ridge.pred-y.test)^2) # Test MSE : 224669.8, lambda = 4 의test MSE가 더 낮음 

# lambda = 0으로 하면? linear regression과 같다.
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2) # MSE도 커진 모습 : 168588.6
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0) 
#Ridge를 cross-validation 하면서
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #CV한 결과 MSE가 가장 낮은 lambda 값

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2) 
# 위에서 구한 lambda를 넣은 모델의 TestMSE : 139856.6
# random.seed에 따라 결과가 다른것 같다. 
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]


# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid) #alpha = 1로 하면, LASSO
plot(lasso.mod) #람다가 작을수록 0이되는 계수가 늘어남,

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1) # Lasso with CV
plot(cv.out)
bestlam=cv.out$lambda.min #CV의 결과로 구한 best 람다
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) #best 람다로 구한 모델의 test MSE : 143673

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,] 
#best 람다로 fitting할 때, 탈락하는 변수를 확인
lasso.coef
lasso.coef[lasso.coef!=0]



# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
#scale=TRUE : 변수를 표준화해줌
#validation="CV" : 10-fold CV
summary(pcr.fit)
#결과의 RMSEP는 root MSE, 
validationplot(pcr.fit,val.type="MSEP") 
# component의 수에 따른 MSE, 16에서 최소

# train / test split 후 시도
set.seed(104)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP") #trainset에서 M = 7에서 최소

pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2) #M=7으로 PCA 시 Test MSE

pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit) # 변수 설명력

# Partial Least Squares

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP") #M = 2에서 최소

pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2) #test MSE

pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
