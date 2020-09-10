# Fitting Classification Trees


library(tree)
library(ISLR)

attach(Carseats)    #attach 중첩주의..
High=factor(ifelse(Sales<=8,"No","Yes")) # 8을 기준으로 binary 변수 만들어줌
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats) #sale 빼고 모든 변수 fit 해줌. tree()는 lm()과 유사한 syntax
summary(tree.carseats)  
# error rate 이 9%

plot(tree.carseats)    
text(tree.carseats,pretty=0) #node label 확인.
tree.carseats

# 분류성능을 평가하려면 test set 을 봐야함.

set.seed(2) 
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(104+50)/20
##[1] 0.77

#pruning의 효과를 검증 with cv.tree()
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) 
#Fun 부분은 default 인 deviance가 아닌 classification error rate을 얻기 위함.
names(cv.carseats)
cv.carseats

#size 와 k 를 변수로하는 error rate 함수 그리기.
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)  # prunt.misclass : 최적의 9개 노드 트리 위한 함수.
plot(prune.carseats)
text(prune.carseats,pretty=0)

#예측 성능 확ㅇ
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(95+58)/200
##[1] 0.765 비슷한 성능. 어쨋든 단순해진 모델.

#best node 개수 조절.
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(102+53)/200
## [1] 0.775 성능 향상


# Fitting Regression Trees
install.packages("MASS")
library(MASS)
attach(Boston)
# training set 만들어주고 fit.
head(Boston)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)   #medv : med of houseprice
summary(tree.boston) # 나머지 13개의 변수 중 4가지 변수로 트리 구성함. [1] "rm"    "lstat" "crim"  "tax" 

#그림.
par(mfrow=c(1,1))
plot(tree.boston)
text(tree.boston,pretty=1, cex=0.7)

# whether to prune or not
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

# the most complex tree is selecte by CV but we can prune some anyway if we want to.
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

# we use unpruned tree to make predictions on the test set
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)   
#결과 값에 루트씌우면 MSE = 5정도 나온다. 
# 즉 model 이 test prediction하면 $5000 오차범위 내로 실제 집값 중위수값을 예측한다.


# Bagging and Random Forests
# 결국 bagging 은 m=p인 randomforest 와 마찬가지.
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
#13개의 predictor 전부 필요하다.

#성능확인.
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#single tree 에 비해 error 절반 감소.

# tree 개수 변경.
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
# [1] 16.16772

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
# [1] 13.17422 #random forest가 bagging 보다 나은 성능 보여줌.
importance(rf.boston)
#변수별 중요도 - MSE , Node impurity (변수 쪼갬으로써 줄어드는 node impurity 합 RSS)
# rm (방 갯수) lstst(사회경제수준) 이 중요해보임.

varImpPlot(rf.boston)
#그래프로도 확인 가느

