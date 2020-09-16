# ch.9 svm
# exercise 8
# (a) train, test 만들기
set.seed(10)
train=sample(1:1070,800)
test=(1:1070)[-train]

tb=c()
res=c()
# (b) svm by cost 0.01
require(ISLR)
require(e1071)

svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='linear')
summary(svm.fit)
# 451개의 sv
# (c) train, test error 확인
library("knitr")
# train
svm.pred=predict(svm.fit,OJ[train,])
kable(table(OJ[train,'Purchase'],svm.pred))

mean(OJ$Purchase[train] != svm.pred) # error가 0.18
res=cbind(res,'train'=mean(OJ$Purchase[train] != svm.pred))

# test
svm.pred=predict(svm.fit,OJ[test,])
kable(table(OJ[test,'Purchase'],svm.pred))

mean(OJ$Purchase[test] != svm.pred) # error가 0.15
res=cbind(res,'test'=mean(OJ$Purchase[test] != svm.pred))

# (d) optimal cost 고르기
svm.tune=tune(svm,Purchase~.,data=OJ[train,],ranges=data.frame(cost=seq(0.01,10,25)),kernel='linear')
summary(svm.tune)
# 10 fold error가 0.18
res=cbind(res,'CV'=svm.tune$best.performance)

# (e) 이 cost로 train, test error
# train
svm.pred=predict(svm.tune$best.model,OJ[train,])
kable(table(OJ[train,'Purchase'],svm.pred))

mean(OJ$Purchase[train] != svm.pred) # error가 0.18
res=cbind(res,'train.tuned'=mean(OJ$Purchase[train] != svm.pred))
# test
svm.pred=predict(svm.tune$best.model,OJ[test,])
kable(table(OJ[test,'Purchase'],svm.pred))

mean(OJ$Purchase[test] != svm.pred) # error가 0.15
res=cbind(res,'test.tuned'=mean(OJ$Purchase[test] != svm.pred))

tb=rbind(tb,res)
res=c()
# (f) radial kernel로 해보자
# (b)
svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='radial')
summary(svm.fit)
# cost 0.01
# train
svm.pred=predict(svm.fit,OJ[train,])
kable(table(OJ[train,'Purchase'],svm.pred))
mean(OJ$Purchase[train] != svm.pred) # error 0.3862

res=cbind(res,'train'=mean(OJ$Purchase[train] != svm.pred))


# test
svm.pred=predict(svm.fit,OJ[test,])
kable(table(OJ[test,'Purchase'],svm.pred))
mean(OJ$Purchase[test] != svm.pred) #error 0.4

res=cbind(res,'train'=mean(OJ$Purchase[test] != svm.pred))

svm.tune=tune(svm,Purchase~.,data=OJ[train,],ranges=data.frame(cost=seq(0.01,10,25)))
summary(svm.tune) # 10 fold cv error 0.386

res=cbind(res,'CV'=svm.tune$best.performance)

# train
svm.pred=predict(svm.tune$best.model,OJ[train,])
kable(table(OJ[train,'Purchase'],svm.pred))
mean(OJ$Purchase[train] != svm.pred) # error 0.386

res=cbind(res,'train.tuned'=mean(OJ$Purchase[train] != svm.pred))


# test
svm.pred=predict(svm.tune$best.model,OJ[test,])
kable(table(OJ[test,'Purchase'],svm.pred))
mean(OJ$Purchase[test] != svm.pred) # error 0.4

res=cbind(res,'test.tuned'=mean(OJ$Purchase[test] != svm.pred))

tb=rbind(tb,res)
res=c()
# (g) poly with degree 2 kernel로 해보자
# (b) cost 0.01
svm.fit=svm(Purchase~.,data=OJ,subset=train,cost=0.01,kernel='polynomial')
summary(svm.fit)
# train
svm.pred=predict(svm.fit,OJ[train,])
kable(table(OJ[train,'Purchase'],svm.pred))
mean(OJ$Purchase[train] != svm.pred) # error 0.386
res=cbind(res,'train'=mean(OJ$Purchase[train] != svm.pred))

# test
svm.pred=predict(svm.fit,OJ[test,])
kable(table(OJ[test,'Purchase'],svm.pred))
mean(OJ$Purchase[test] != svm.pred) # error 0.4 
res=cbind(res,'test'=mean(OJ$Purchase[test] != svm.pred))

svm.tune=tune(svm,Purchase~.,data=OJ[train,],ranges=data.frame(cost=seq(0.01,10,25)),kernel='polynomial')
summary(svm.tune) # error 10 fold cv 0.386
res=cbind(res,'CV'=svm.tune$best.performance)
# train
svm.pred=predict(svm.tune$best.model,OJ[train,])
kable(table(OJ[train,'Purchase'],svm.pred))
mean(OJ$Purchase[train] != svm.pred)# error 0.386
res=cbind(res,'train.tuned'=mean(OJ$Purchase[train] != svm.pred))

# test
svm.pred=predict(svm.tune$best.model,OJ[test,])
kable(table(OJ[test,'Purchase'],svm.pred))
mean(OJ$Purchase[test] != svm.pred)# error 0.4
res=cbind(res,'test.tuned'=mean(OJ$Purchase[test] != svm.pred))

tb=rbind(tb,res)

# (h) 모두 합쳐서 비교하자
rownames(tb)=c('LINEAR','POLYNOMIAL','RADIAL')
kable(tb)
# 놀랍게도 linear 모델이 압도적으로 test error가 낮았다. 