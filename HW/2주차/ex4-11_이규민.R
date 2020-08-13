library(ISLR)
library(MASS)

# (a)
data(Auto)
mpg01 <- Auto$mpg
mpg01[Auto$mpg > median(Auto$mpg)]<-1
mpg01[!(Auto$mpg > median(Auto$mpg))]<-0

df <- data.frame(Auto, mpg01)
df

# (b)
# scatter plot
pairs(df, plot='box')
# displacemetn, horsepower, weight가 correlated된 것 같음
boxplot(displacement~mpg01, data=Auto)
boxplot(horsepower~mpg01, data=Auto)
boxplot(weight~mpg01, data=Auto)
# 이 세 변수가 가장 잘 설명할 것 같기도 함.(scatter plot에서 가장 0, 1 변화가 뚜렷함)

# (c)
set.seed(11)
# 80% train, 20% test
train_index <- sample(1:nrow(df), nrow(df)*0.8 , replace=F)  
train <- df[train_index,]
test <- df[-train_index,]

# (d)
# LDA
fit.lda <- lda(mpg01~displacement+horsepower+weight, data=train)
fit.lda.pred <- predict(fit.lda, test)$class
fit.lda.pred
table(fit.lda.pred, test$mpg01)
# accuracy
36+32/79

# (e)
# QDA
fit.qda <- qda(mpg01~displacement+horsepower+weight, data=train)
fit.qda.pred <- predict(fit.qda, test)$class
table(fit.qda.pred, test$mpg01)
# accuracy
39+30/79

# (f)
# logistic
fit.logit <- glm(mpg01~displacement+horsepower+weight, data=train, family=binomial)
logit.prob <- predict(fit.logit, test, type="response")
logit.pred <- logit.prob
logit.pred[logit.prob > 0.5]<-1
logit.pred[!(logit.prob > 0.5)]<-0

table(logit.pred, test$mpg01)

# accuracy
38+31/79

# (g)
# KNN
library(class)
train.X <- cbind(train$displacement, train$horsepower, train$weight)
test.X <- cbind(test$displacement, test$horsepower, test$weight)
knn.pred <- knn(train.X, test.X, train$mpg01, k=1)
table(knn.pred, test$mpg01)
# accuracy
38+31/79

knn.pred <- knn(train.X, test.X, train$mpg01, k=5)
table(knn.pred, test$mpg01)
# accuracy
39+29/79

knn.pred <- knn(train.X, test.X, train$mpg01, k=10)
table(knn.pred, test$mpg01)
# accuracy
37+29/79

knn.pred <- knn(train.X, test.X, train$mpg01, k=30)
table(knn.pred, test$mpg01)
# accuracy
37+29/79

knn.pred <- knn(train.X, test.X, train$mpg01, k=50)
table(knn.pred, test$mpg01)
# accuracy
37+29/79

# k=5 일 때 성능이 최고다!

