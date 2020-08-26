# ISLR chapter 6 ,Q9

#a)
library(ISLR)
data("College")
attach(College)

set.seed(17)
train = sample(1:dim(College)[1], dim(College)[1] / 2)
#train / test set 으로 나누기 위해 indexingn number 생성

test <- -train
College.train <- College[train, ]
College.test <- College[test, ]

#b)
fit.lm <- lm(Apps ~ ., data = College.train)
pred.lm <- predict(fit.lm, College.test)
mean((pred.lm - College.test$Apps)^2)

##[1] 1548357   
# test MSE = 1528357

#c)
install.packages('glmnet') #glmnet 함수 활용
library(glmnet)
train.mat <- model.matrix(Apps ~ ., data = College.train) #행렬 생성
test.mat <- model.matrix(Apps ~ ., data = College.test)
grid <- 10 ^ seq(4, -2, length = 100)
fit.ridge <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12) #수렴 선택의 기준값을 의미.default는 1e-7이
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
  
pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
mean((pred.ridge - College.test$Apps)^2)
## [1] 1548313 릿지의 test MSE 가 OLS의 MSE 보다 크다.

#d)
fit.lasso <- glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12) #라쏘 기법
cv.lasso <- cv.glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - College.test$Apps)^2)
##[1] 1548296 릿지와 비슷한 수준의 MSE

predict(fit.lasso, s = bestlam.lasso, type = "coefficients")

#e)
install.packages('pls')
library(pls)
fit.pcr <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, College.test, ncomp = 10)
mean((pred.pcr - College.test$Apps)^2)
##[1] 1791589 pcr MSE 가 가장 높게 나타남.

fit.pls <- plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")

pred.pls <- predict(fit.pls, College.test, ncomp = 10)
mean((pred.pls - College.test$Apps)^2)
## 라소와 릿지보다 높은 PLS MSE 
## PLS는 supervised이므로 bias는 감소하지만 그만큼 variance가 높아지기에 상대적으로 성능이 떨어질 수 있음


#e)
# R^2 를 활용하여 모델을 평가
test.avg <- mean(College.test$Apps)
lm.r2 <- 1 - mean((pred.lm - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
ridge.r2 <- 1 - mean((pred.ridge - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
lasso.r2 <- 1 - mean((pred.lasso - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pcr.r2 <- 1 - mean((pred.pcr - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pls.r2 <- 1 - mean((pred.pls - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)

# 0.86의 pcr 을 제외하면 대부분 0.88로 유사한 값을 갖고 있다.
# 현 데이터에서는 lasso ridge의 MSE 값이 거의 동일하지만 
#  데이터 특성에 따라 다르므로 적절히 선택하면 될 듯 (유의미한 변수가 적을때는 lasso, 많을 때는 Ridge가 더 높은 성능을 보인다고 한다)
