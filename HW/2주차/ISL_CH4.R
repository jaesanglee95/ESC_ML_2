# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

library(ISLR)
names(Smarket)
dim(Smarket)  # 1250*9 사이즈의 데이터프레임
summary(Smarket) # 열별 요약통계량 산출

pairs(Smarket) # 각 변수끼리 산점도를 생성

cor(Smarket)
cor(Smarket[,-9]) # 9번째 열만 제거

attach(Smarket)
plot(Volume)

# Logistic Regression

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial) # Generalize linear regression model
summary(glm.fits)

coef(glm.fits)
summary(glm.fits)$coef # names(summary(glm.fits))로 변수명 확인가능

summary(glm.fits)$coef[,4]

glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

table(glm.pred,Direction) # 범주형 자료인 경우 table로 정렬 
(507+145)/1250  # Accuracy
mean(glm.pred==Direction)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005) # 2005년도에 해당하는 행이 252
Direction.2005=Direction[!train] # 2005년도에 해당하는 Direction 추출

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) # (77+44)/(77+97+34+44)과 같음을 확인
mean(glm.pred!=Direction.2005)

glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response") # lag1과 lag2를 열로 가지는 newdata
 # 결과는 행별로 나오네..? lag1 la2 2개를 넣어서 모델을 만들었으니까 dataframe형태로 값을 제시하는 듯
# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)  # LDA하는 방법
lda.fit
plot(lda.fit)

lda.pred=predict(lda.fit, Smarket.2005) # predict는 다양한 상황에서도 사용할 수 있구나 => lm,glm,lda,시계열관련 함수들에서도...
names(lda.pred)

lda.class=lda.pred$class
table(lda.class,Direction.2005) # 범주형 자료를 table로 정리
mean(lda.class==Direction.2005) # Accuracy

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train) # QDA하는 방법
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]  # 특정조건만 만족하는 행만 추출
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)  # 랜덤추출의 결과가 다음에도 같은 값으로 나올 수 있게 고정
knn.pred=knn(train.X,test.X,train.Direction,k=1)  # KNN 방법
table(knn.pred,Direction.2005)
(83+43)/252

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized.X=scale(Caravan[,-86])  # 표준화하는 과정
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test,]  # train과 test로 구분
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
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
