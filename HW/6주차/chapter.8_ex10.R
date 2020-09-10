# exercise 10
#a
install.packages("ISLR")
require(ISLR)
Hitters.unknownSal=is.na(Hitters[,"Salary"])
Hitters=Hitters[!Hitters.unknownSal,]
Hitters[,"Salary"]=log(Hitters[,"Salary"])

head(Hitters)

summary(Hitters)
#b
#train , test data set 생성.
Hitters.train=Hitters[1:200,]
Hitters.test=Hitters[-c(1:200),]

#c,d
install.packages("gbm")
require(gbm)
#gbm(gradient boosting machine)으로 부스팅. 

train.mse=c()
test.mse=c()
#tree 갯수1000
for(shr in seq(0,0.08,0.002)){
  Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = shr,n.trees = 1000,distribution = 'gaussian')
  
  Hitters.pred=predict(Hitters.gbm,Hitters.train,n.trees = 1000)
  train.mse=rbind(train.mse,mean((Hitters.pred-Hitters.train[,'Salary'])^2))
  
  Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
  test.mse=rbind(test.mse,mean((Hitters.pred-Hitters.test[,'Salary'])^2))
}

#그림.
plot(seq(0,0.08,0.002),train.mse,type='l',xlab='shrinkage',xlim = c(0.003,0.07),ylab='MSE')
lines(seq(0,0.08,0.002),test.mse,col='red')
legend(x='top',legend = c('train MSE','test MSE'),col=c('black','red'),lty=1,text.width = 0.005)

#e
tb=c()

Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = 0.01,n.trees = 1000,distribution = 'gaussian')
Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
tb=cbind(tb,'Boost'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch3 - linear regression
# 선형회귀와 리지모델과 에러비교
require(glmnet)
library(knitr)
Hitters.lm=lm(Salary~.,Hitters.train)
Hitters.pred=predict(Hitters.lm,Hitters.test)
tb=cbind(tb,'Linear'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

x = model.matrix(Salary ~ ., data = Hitters.train)
x.test = model.matrix(Salary ~ ., data = Hitters.test)
y = Hitters.train$Salary

Hitters.glm=glmnet(x,y,alpha = 0)
Hitters.pred=predict(Hitters.glm,x.test)
tb=cbind(tb,'Ridge'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

kable(tb)
## linear , Ridge 의 에러값이 0.49인데 반해 Boost의 에러는 0.27로 현저히 낮다!

#f
kable(summary(Hitters.gbm),row.names = F)
## |CAtBat    | 22.6364528|
# CAtBat 's relative influence is the highest one with 22.6364528

#g
require(randomForest)
Hitters.rf=randomForest(Salary~.,data = Hitters.train,mtry=ncol(Hitters.train)-1) # bagging m=p
Hitters.pred=predict(Hitters.rf,Hitters.test)
mean((Hitters.pred-Hitters.test[,'Salary'])^2)
## [1] 0.2290211
