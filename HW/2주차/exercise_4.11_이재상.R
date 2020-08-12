#4.11 

library(ISLR)
str(Auto)

#a
Auto$mpg1 <- ifelse(Auto$mpg > median(Auto$mpg),1,0)

#b
pairs(Auto)
#displacement, horsepower, weight, acceleration

#cylinders 확인
boxplot(Auto$cylinders, Auto$mpg1)


#c
ind <- sample(2,dim(Auto)[1], replace = T, prob = c(0.7,0.3))
train <- Auto[ind==1,]
test <- Auto[ind==2,]


#d
library(MASS)
lda.auto <- lda(data = train, mpg1 ~ cylinders + displacement + horsepower + weight+ acceleration)

lda.class.auto <- predict(lda.auto, test)$class
table(lda.class.auto, test$mpg1)
mean(lda.class.auto == test$mpg1)

#e
qda.auto <- qda(data = train, mpg1 ~ cylinders + displacement + horsepower + weight+ acceleration)

qda.class.auto <- predict(qda.auto, test)$class
table(qda.class.auto, test$mpg1)
mean(qda.class.auto == test$mpg1)

#f
glm.auto=glm(data = train, mpg1 ~ cylinders + displacement + horsepower + weight+ acceleration)

glm.pred.auto <- predict(glm.auto, test, type = "response")
glm.class.auto <- rep(0,length(glm.pred.auto))
glm.class.auto[glm.pred.auto > 0.5] <- 1
table(glm.class.auto, test$mpg1)
mean(glm.class.auto == test$mpg1)

#g
library(class)
str(train)
train.X <- train[,c(2,3,4,5,6)]
train.Y <- train$mpg1
test.X <- test[,c(2,3,4,5,6)]
test.Y <- test$mpg1

knn.auto1 <- knn(train.X, test.X,train.Y, k =1 )
table(knn.auto1, test$mpg1)
mean(knn.auto1 == test$mpg1)

knn.auto3 <- knn(train.X, test.X,train.Y, k =3 )
table(knn.auto3, test$mpg1)
mean(knn.auto3 == test$mpg1)

knn.auto5 <- knn(train.X, test.X,train.Y, k =5 )
table(knn.auto5, test$mpg1)
mean(knn.auto5 == test$mpg1)

knn.auto10 <- knn(train.X, test.X,train.Y, k =10 )
table(knn.auto10, test$mpg1)
mean(knn.auto10 == test$mpg1)
