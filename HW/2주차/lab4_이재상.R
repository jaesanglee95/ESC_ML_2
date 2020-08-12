# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

#explore Smarket data
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

# correlation 확인
pairs(Smarket)

cor(Smarket) #numeric 변수만 확인 가능
cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

# Logistic Regression
# logistic regression fitting
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)

coef(glm.fits) #계수
summary(glm.fits)$coef 

summary(glm.fits)$coef[,4] # p-vlaue

glm.probs=predict(glm.fits,type="response") #P(Y=1|X) 기준으로 확률을 구해냄
glm.probs[1:10]
contrasts(Direction) 
#0에 가까우면 Down, 1에 가까우면 Up
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
#분류 정확도 확인
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

train=(Year<2005) #2005년 이전/이후로 데이터 나눔
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
# 2005년 이전으로 fitting, 2005년 데이터에 대한 정확도 확인
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
#변수를 줄여서 fitting후 정확도 비교
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit #lda fitting 후 결과 확인
plot(lda.fit)

lda.pred=predict(lda.fit, Smarket.2005)#예측
names(lda.pred)

lda.class=lda.pred$class #타겟에 대한 예측 class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=.5) #0.5를 threshold로 하여 분류 해봄
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9) #threshold 변경 가능하다! 그러나 정확도가 크게 떨어질 수 있다.
#최적의 threshold를 찾는것도 중요

# Quadratic Discriminant Analysis
# fitting QDA
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
#예측 결과 확인
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-Nearest Neighbors
#KNN은 matrix단위로 함수에 넣어야 해서, 데이터를 미리 가공한다.
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005) #knn(k=1)을 통한 예측과 결과
(83+43)/252

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)#k=3일 때 정확도가 향상한 모습

# An Application to Caravan Insurance Data
#explore Caravan data
dim(Caravan)
attach(Caravan)
summary(Purchase) #target : Purchase
348/5822

standardized.X=scale(Caravan[,-86]) #X에 대한 표준화 실행
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

#train / test split 과정
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1) #k=1로 knn
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)
9/(68+9)

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15

glm.fits=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fits,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)

# 타깃에 있어서 1종 오류와 2종 오류를 비교하여
# 결과에 치명적인 부분을 줄이는 방향으로 고려해야 할 듯 