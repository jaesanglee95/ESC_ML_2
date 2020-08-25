# 6.8 exercise
# 9. college data analyzing
# (a) split
library(ISLR)
set.seed(1)
sum(is.na(College)) # 결측치 확인
dim(College)[1] # 777개
summary(College) # 분석해야할 자료는 Apps

# 70%만 train으로!
train.size = dim(College)[1] * 0.7
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]

# (b) least square
lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test[, "Apps"] - lm.pred)^2)
# mse 는 1261630

# (c) ridge
library(glmnet)
train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(10, -2, length=100)
ridge.mod = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best1 = ridge.mod$lambda.min
lambda.best1 # 최적의 lambda는 0.01

ridge.pred = predict(ridge.mod, newx=test.mat, s=lambda.best1)
mean((College.test[, "Apps"] - ridge.pred)^2)
# mse 는 1261598
# 조금 줄었다...

# (d) lasso
lasso.mod = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best2 = lasso.mod$lambda.min
lambda.best2 # 최적의 lambda는 0.01

lasso.pred = predict(lasso.mod, newx=test.mat, s=lambda.best2)
mean((College.test[, "Apps"] - lasso.pred)^2)
# mse 는 1261591
# 조금 더 줄었다...

# (e) pcr
library(pls)
pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP") # 16개 변수일 때 최소

pcr.pred = predict(pcr.fit, College.test, ncomp=16)
mean((College.test[, "Apps"] - data.frame(pcr.pred)[,1])^2)
# mse 는 1332330
# 오히려 증가했다. 변수도 16개라 가장 안 좋아 보인다.

# (f) pls
pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP") # 12개 변수 이상일 때 최소

pls.pred = predict(pls.fit, College.test, ncomp=12)
mean((College.test[, "Apps"] - data.frame(pls.pred)[,1])^2)
# mse 는 1262604
# 다시 많이 줄긴 했다.

# (g) 
# mse 비교하는 코드를 통해 그래프 그려보자
# 여기서는 test r squared니까 클수록 좋은 것
test.avg = mean(College.test[, "Apps"])
lm.test.r2 = 1 - mean((College.test[, "Apps"] - lm.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((College.test[, "Apps"] - ridge.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((College.test[, "Apps"] - lasso.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pcr.pred)[,1])^2) /mean((College.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pls.pred)[,1])^2) /mean((College.test[, "Apps"] - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared", ylim=c(0, 1.5))
# 그래프로는 비교가 힘들다... 모두 다 0.9 언저리
# pcr이 제일 작아보이지만 차이가 거의 없어 판단하기 힘들다.

# 얼마나 적은 변수를 사용하는지도 보자
ridge.mod = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=0)
predict(ridge.mod, s=lambda.best1, type="coefficients")
# 변수가 그대로...

lasso.mod = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(lasso.mod, s=lambda.best2, type="coefficients")
# 변수가 여전히 그대로....

# 종합적으로 판단했을 때 lasso가 가장 작은 mse값을 가졌다. 
# lasso, ridge 모두 변수는 그대로 19개였지만, 
# pcr은 16개, pls은 12개로 상당히 줄었다.
# 성능만 보면 lasso가 좋지만, 변수를 7개나 줄였으면서 비슷한 mse를 가진 pls도 사용하면 좋을 것 같다.