# chapter 5 Resampling
# Exercise 1
# Q: prove that alpha = (sd(Y)^2-cov(X, Y))/(sd(X)^2+sd(Y)^2-2cov(X, Y)) minimize var(alpha*X+(1-alpha)*Y)

# A: var(alpha*X+(1-alpha)*Y)
# = alpha^2*var(X) + (1-alpha)^2*var(Y)+2*alpha*(1-alpha)*cov(X, Y)
# 최소화하는 alpha 찾기 위해서 alpha에 대해 미분

# 2*alpha*var(X)+2*(alpha-1)*var(Y)+2*(1-alpha)*cov(X, Y)-2*alpha*cov(X, Y)
# = 2*alpha*var(X)+2*(alpha-1)*var(Y)+2*(1-2alpha)*cov(X, Y)
# = 2*alpha*(var(X)+var(Y)-2*cov(X, Y))-2*(var(Y)-cov(X, Y))

# 위 식이 0이 되는 alpha를 찾는다.
# alpha =(var(Y)-cov(X, Y)) / (var(X)+var(Y)-2*cov(X, Y))

# Exercise 5
# (a)
library(ISLR)
data(Default)
summary(Default)
dim(Default)*1/6

#logistic regression 실행
set.seed(21)
fit.logit <- glm(default~income+balance, data=Default, family=binomial)

# (b)
logit.cv = function() {
  # first : train set 구분
  train = sample(dim(Default)[1], round(dim(Default)[1]*5/6))
  # second : logistic 돌리기
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # third : yes no로 예측
  glm.pred = rep("No", 10000-round(dim(Default)[1]*5/6))
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # fourth : 결과 다른 것들 평균
  return(mean(glm.pred != Default[-train, ]$default))
}
logit.cv()
# 0.0252 / 97.48%정확하다

# (c)
logit.cv = function() {
  # first : train set 구분
  train = sample(dim(Default)[1], round(dim(Default)[1]*4/5))
  # second : logistic 돌리기
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # third : yes no로 예측
  glm.pred = rep("No", 10000-round(dim(Default)[1]*4/5))
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # fourth : 결과 다른 것들 평균
  return(mean(glm.pred != Default[-train, ]$default))
}
logit.cv()
# 0.0295

logit.cv = function() {
  # first : train set 구분
  train = sample(dim(Default)[1], round(dim(Default)[1]*2/3))
  # second : logistic 돌리기
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # third : yes no로 예측
  glm.pred = rep("No", 10000-round(dim(Default)[1]*2/3))
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # fourth : 결과 다른 것들 평균
  return(mean(glm.pred != Default[-train, ]$default))
}
logit.cv()
# 0.0258

logit.cv = function() {
  # first : train set 구분
  train = sample(dim(Default)[1], round(dim(Default)[1]*3/4))
  # second : logistic 돌리기
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # third : yes no로 예측
  glm.pred = rep("No", 10000-round(dim(Default)[1]*3/4))
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # fourth : 결과 다른 것들 평균
  return(mean(glm.pred != Default[-train, ]$default))
}
logit.cv()
#0.0256

# (d)
logit.cvdum = function() {
  # first : train set 구분
  train = sample(dim(Default)[1], round(dim(Default)[1]*5/6))
  # second : logistic 돌리기
  glm.fit = glm(default ~ ., data = Default, family = binomial, 
                subset = train)
  # third : yes no로 예측
  glm.pred = rep("No", 10000-round(dim(Default)[1]*5/6))
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # fourth : 결과 다른 것들 평균
  return(mean(glm.pred != Default[-train, ]$default))
}
logit.cvdum()
# 0.0252
# 더 낮긴 한데, 유의미한 차이가 있는 것 같진 않다...
