#lab8
# fitting classification tree
install.packages('tree')
library(tree)

library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,'no','yes') # sales 8 이하인지 아닌지 binary var로 짠다
High = as.character(High)# High를 병합한 후 tree 분석
Carseats=data.frame(Carseats,High)
tree.carseats=tree(as.factor(High)~.-Sales,data=Carseats)
summary(tree.carseats)
#거의 error가 9%
# tree는 그림 그리기도 쉽다
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
# 너무 노드가 많아서 난잡한 것 같다

#이제 제대로 test set도 나눠서 error 계산
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train ,]
High.test=High[-train]
tree.carseats=tree(as.factor(High)~.-Sales ,Carseats ,subset =train )
tree.pred=predict(tree.carseats,Carseats.test ,type ="class")
table(tree.pred ,High.test)
# test set 정확도 77%
154/200
# pruning 해서 성능 높이자
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
# 그림 그려서 size, k어떻게 나눌지 생각
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
# size는 9, k는 1.75일 때 최소
# 이때의 pruned tree를 그리고,
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
# 그때의 예측 정확도를 보자
tree.pred=predict(prune.carseats,Carseats.test ,type="class")
table(tree.pred ,High.test)
# 정확도는 77.5%로 조금 더 좋아짐!
155/200
# 만약 node 늘린다면?
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test ,type="class")
table(tree.pred ,High.test)
# 정확도는 155/200로 9개일 때랑 같은데 보통 더 에러가 커진다고 한다.

# fitting regression tree
# 이번엔 회귀 도전
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset =train)
summary(tree.boston)
# 그려보자
plot(tree.boston)
text(tree.boston ,pretty =0)
# rm, dis, lstat에 따라 집값 결정
# 이번에도 prune 해보자
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
# size가 5만 돼도 충분히 낮아보인다.
# 5일 때도 그려보자 
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston ,pretty =0) 

# prune 안된걸로 예측 정확도 평가해보자.
yhat=predict(tree.boston,newdata=Boston[-train ,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
sqrt(35.2868)
# 즉, test prediction에서 5940 달러 정도 반경에 진짜 집값이 있다고 예측.

# bagging and random forest
install.packages('randomForest')
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston ,subset =train ,mtry=13, importance =TRUE)
bag.boston
# 85%의 분산을 설명!
# 이제 예측해보자
yhat.bag = predict(bag.boston ,newdata =Boston [-train ,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
# 앞선 tree보다 훨씬 예측 반경이 줄었다. 즉 정확해졌다.
# 이제 tree 수 바꿔보자
bag.boston=randomForest(medv~.,data=Boston ,subset =train , mtry=13, ntree =25)
yhat.bag = predict(bag.boston ,newdata =Boston [-train ,])
mean((yhat.bag-boston.test)^2)
# 오히려 살짝 오차가 늘었다...
# 이번엔 random forest로! mtry를 줄이자.
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston ,subset =train ,mtry=6, importance =TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# 무려 19.6으로 줄었다.
# random forest는 바로 feature importance를 구하고 그림 그릴 수 있다.
importance(rf.boston)
varImpPlot(rf.boston)
# 역시 lstat와 rm이 중요하다는 것을 알 수 있다.

# boosting
install.packages('gbm')
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston [train ,], distribution="gaussian",n.trees =5000, interaction.depth =4)
summary(boost.boston)
# 역시 여기서도 rm, lstat가 압도적으로 중요하다고 나왔다.
# 각 중요 변수에 따른 그래프 그려보자
par(mfrow=c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
# 이제 boosting 모델로 예측하자
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees =5000)
mean((yhat.boost -boston.test)^2)
# 최저치가 나왔다.
# 다른 shrinkage parameter를 이용해서 boosting 해보자
boost.boston=gbm(medv~.,data=Boston [train ,], distribution="gaussian",n.trees =5000, interaction.depth =4, shrinkage =0.2,verbose =F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees =5000)
mean((yhat.boost -boston.test)^2)
# 더 적은 값이 나왔다. lambda가 0.2인 게 더 낮은 mse로 이어졌다. 가중치를 확주는게 더 좋았다.