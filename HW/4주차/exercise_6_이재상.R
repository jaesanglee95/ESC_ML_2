# 4주차 과제_이재상

#6.9

library(ISLR)

str(College)

#(a) : split
set.seed(37)
idx <- sample(1:nrow(College),nrow(College)*0.7)
TrainSet <- College[idx,]
TestSet <- College[-idx,-2]
TestY <- College[-idx,2]

#(b) : Fit a linear model
fit_lm <- lm(Apps~ . ,data = TrainSet)
pred_lm <- predict(fit_lm, TestSet)
mse_lm <- mean((pred_lm- TestY)^2)
mse_lm
# Test MSE : 823760.9


#(c) : Fit a ridge regression 

library(glmnet)
x <- model.matrix(Apps~.,College)[,-1]
y <- TrainSet[,2]
fit_ridge <- cv.glmnet(x[idx,] , y ,alpha = 0)
plot(fit_ridge)
BestLambda_ridge <- fit_ridge$lambda.min
BestLambda_ridge # lambda = 392.7404

pred_ridge <- predict(fit_ridge, s = BestLambda_ridge, newx = x[-idx,])
mse_ridge <- mean((pred_ridge- TestY)^2)
mse_ridge
# Test MSE : 867774.4


# (d) : Fit a lasso model

fit_lasso <- cv.glmnet(x[idx,] , y ,alpha = 1)
plot(fit_lasso)

BestLambda_lasso <- fit_lasso$lambda.min
BestLambda_lasso # lambda = 17.8103

predict(fit_lasso,type="coefficients",s=BestLambda_lasso)
#공교롭게도 계수가 0이 되는 변수가 없다...

pred_lasso <- predict(fit_lasso, s = BestLambda_lasso, newx = x[-idx,])
mse_lasso <- mean((pred_lasso- TestY)^2)
mse_lasso
# Test MSE : 811363.8


# (e) : Fit a PCR model
library(pls)

fit_pcr <- pcr(Apps ~. , data = TrainSet , scale = T, validation = "CV")
validationplot(fit_pcr, val.type = "MSEP")
summary(fit_pcr)
# M = 17

pred_pcr <- predict(fit_pcr,TestSet,ncomp=17)
mse_pcr <- mean((pred_pcr- TestY)^2)
mse_pcr
# Test MSE : 823760.9


# (f) : Fit a PLS model

fit_pls <- plsr(Apps ~. , data = TrainSet , scale = T, validation = "CV")
validationplot(fit_pls, val.type = "MSEP")
summary(fit_pls)
# M = 13

pred_pls <- predict(fit_pls,TestSet,ncomp=13)
mse_pls <- mean((pred_pls- TestY)^2)
mse_pls
# Test MSE : 827792.8

# (g) 

mse_lm
mse_ridge
mse_lasso
mse_pcr
mse_pls

#test MSE는 Lasso가 가장 낮게 나타났다. 
#ridge가 가장 큰 MSE를 보였으며, 심지어 linear model보다도 훨씬 높았다.
#계수값이 모서리해를 갖는 경우가 많았다고 생각할 수도 있겠다.