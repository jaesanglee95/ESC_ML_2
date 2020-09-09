# ch. 8 tree based models
# ex 8.10 boosting
library(ISLR)
attach(Hitters)
# (a) remove unknowns and log transform
Hitters.unknown=is.na(Hitters[,"Salary"])
Hitters=Hitters[!Hitters.unknown,]
Hitters[,"Salary"]=log(Hitters[,"Salary"])

summary(Hitters)

# (b) creating train and test set
Hitters.train=Hitters[1:200,]
Hitters.test=Hitters[-c(1:200),]

# (c) boosting with 1000 trees
# (d) produce a plot about (c)
library(gbm)

train.mse=c()
test.mse=c()

for(shr in seq(0,0.08,0.002)){
  Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = shr,n.trees = 1000,distribution = 'gaussian')
  
  Hitters.pred=predict(Hitters.gbm,Hitters.train,n.trees = 1000)
  train.mse=rbind(train.mse,mean((Hitters.pred-Hitters.train[,'Salary'])^2))
  
  Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
  test.mse=rbind(test.mse,mean((Hitters.pred-Hitters.test[,'Salary'])^2))
}
par(mfrow=c(1, 1))
plot(seq(0,0.08,0.002),train.mse,type='l',xlab='shrinkage',xlim = c(0.003,0.07),ylab='MSE')
lines(seq(0,0.08,0.002),test.mse,col='red')
legend(x='top',legend = c('train MSE','test MSE'),col=c('black','red'),lty=1,text.width = 0.005)
# shrinkage 는 특정 점 이상의 값에서는 큰 변동 없음.

# (e) comparison
# 우선 boosting의 MSE 저장
tb=c()

Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = 0.01,n.trees = 1000,distribution = 'gaussian')
Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
tb=cbind(tb,'Boost'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch3 - linear regression 과 비교
Hitters.lm=lm(Salary~.,Hitters.train)
Hitters.pred=predict(Hitters.lm,Hitters.test)
tb=cbind(tb,'Linear'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch6 - ridge regression 과도 비교
library(glmnet)
x = model.matrix(Salary ~ ., data = Hitters.train)
x.test = model.matrix(Salary ~ ., data = Hitters.test)
y = Hitters.train$Salary

Hitters.glm=glmnet(x,y,alpha = 0)
Hitters.pred=predict(Hitters.glm,x.test)
tb=cbind(tb,'Ridge'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))
# 비교
table(tb)
# ridge regression 은 그냥 회귀보다 오히려 성능이 안 좋고, 확실히 boosting이 확연하게 좋았다.

# (f) finding important variable
table(summary(Hitters.gbm),row.names = F)
summary(Hitters.gbm)
# 압도적으로 CAtBat가 중요한 변수라는 것을 알 수 있다.

# (g) apply bagging to the train set
library(randomForest)
Hitters.rf=randomForest(Salary~.,data = Hitters.train,mtry=ncol(Hitters.train)-1) # bagging m=p
Hitters.pred=predict(Hitters.rf,Hitters.test)
mean((Hitters.pred-Hitters.test[,'Salary'])^2)
# boosting보다 bagging한 결과가 더 낮은 mse를 기록