#lab 8 : Decision Trees

# Fitting Classification Tree

library(tree)
library(ISLR)
attach(Carseats)

High=ifelse(Sales<=8,"No","Yes") # Sales를 이용해 binary 변수를 생성
High <-as.factor(High) #factor화 시키지 않으면, tree()에서 오류 발생


Carseats=data.frame(Carseats,High)
#fitting tree
tree.carseats=tree(High~. - Sales ,Carseats ) 

summary(tree.carseats)

#tree 시각화
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats #모델의 세부내용

set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train ,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales ,Carseats ,subset =train )
tree.pred=predict(tree.carseats,Carseats.test ,type ="class") #tree의 예측 성능 확인
table(tree.pred ,High.test)
# (104+50)/200 

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) #최적의 파라미터를 찾기 위한 CV 
names(cv.carseats)
cv.carseats


par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b") #size : 8
plot(cv.carseats$k,cv.carseats$dev,type="b")

prune.carseats=prune.misclass(tree.carseats,best=8) 
#purning을 하므로, 해석이 훨씬 용이해질 것
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred=predict(prune.carseats,Carseats.test ,type="class")
table(tree.pred ,High.test) 
# (89+62)/200

prune.carseats=prune.misclass(tree.carseats,best=15)
# best를 더 높여서 시도하면 정확도가 떨어질 것이다
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test ,type="class")
table(tree.pred ,High.test)
# (102 +53)/200

# Fitting Regression Trees

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset =train)
summary(tree.boston) 
# 4개의 변수만 사용했음을 주목하자.

par(mfrow = c(1,1))
plot(tree.boston)
text(tree.boston ,pretty =0)

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
# 가장 복잡한 tree ( size : 7)이 CV에 의해 선택된다.

#하지만, pruning을 위해 적은 best값을 넣어서 tree를 구성해보았다.
prune.boston=prune.tree(tree.boston,best=5) 
plot(prune.boston)
text(prune.boston ,pretty =0) 

#CV의 결과에서도 보았듯이, 가장 복잡한 tree를 통해 예측해본다.
yhat=predict(tree.boston,newdata=Boston[-train ,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2) #MSE : 35.28688


# Bagging and Random Forests

library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston ,subset =train ,mtry=13, importance =TRUE)
# mtry =13 : 13개의 feature가 모두 반영되는 bagging 
bag.boston

yhat.bag = predict(bag.boston ,newdata =Boston [-train ,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2) 
#bagging의 MSE : 23.66716

#tree의 개수를 조절할 수 있다.
bag.boston=randomForest(medv~.,data=Boston ,subset =train , mtry=13, ntree =25) 
yhat.bag = predict(bag.boston ,newdata =Boston [-train ,])
mean((yhat.bag-boston.test)^2)
# MSE : 23.45478

# RF가 default로 택하는 mtry는 regression tree에서는 p/3개이다.
# (classification tree 에선 sqrt(p)개), 이 경우에는 6개를 넣어서 시도해본다.
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston ,subset =train ,mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# RF MSE : 19.62021

# 변수 중요도 확인
importance(rf.boston)
varImpPlot(rf.boston)

# Boosting

library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston [train ,], distribution="gaussian",n.trees =5000, interaction.depth =4)
# distribution - gaussian : regression problem, bernoulli : binary classification 
summary(boost.boston) #중요도와 함께 plot으로도 나타내준다.

par(mfrow=c(1,2))
#partion dependence plot
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees =5000)
mean((yhat.boost -boston.test)^2)
#gbm MSE : 18.84709

# 다른 파라미터 값을 부여하고 GBM fitting
boost.boston=gbm(medv~.,data=Boston [train ,], distribution="gaussian",n.trees =5000, interaction.depth =4, shrinkage =0.2,verbose =F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees =5000)
mean((yhat.boost -boston.test)^2)
# MSE : 18.33455 다소 개선된 모습