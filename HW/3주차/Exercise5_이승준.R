
##--------conceptual 1--------------##
# A: var(alpha*X+(1-alpha)*Y)
# = alpha^2*var(X) + (1-alpha)^2*var(Y)+2*alpha*(1-alpha)*cov(X, Y)
# alpha에 대해 미분

# 2*alpha*var(X)+2*(alpha-1)*var(Y)+2*(1-alpha)*cov(X, Y)-2*alpha*cov(X, Y)
# = 2*alpha*var(X)+2*(alpha-1)*var(Y)+2*(1-2alpha)*cov(X, Y)
# = 2*alpha*(var(X)+var(Y)-2*cov(X, Y))-2*(var(Y)-cov(X, Y))
# alpha = (sd(Y)^2-cov(X, Y))/(sd(X)^2+sd(Y)^2-2cov(X, Y)



##-----Applied 5 ---------------------##
library(ISLR)
Default
str(Default)
dim(Default)

# set model
set.seed(1)
glm(default~ balance + income , data= Default, family = "binomial")

index = sample(10000,8000,replace = F)
train = Default[index,]
valid = Default[-index,]

glm.train <- glm(default~ balance + income , data= train, family = "binomial")
glm.prod <- predict(glm.train, valid, type = "response")
glm.prod <- ifelse(glm.prod > 0.5, "Yes", "No")
table(glm.prod, valid[,1])
mean(glm.prod == valid[,1]) #0.974 / AC = 0.026

AC <- NULL
for (i in 1:3){
  index <- sample(10000,8000,replace=F)
  train <- Default[index,] #train set
  valid <- Default[-index,] #validation set
  
  #fit logistic model
  glm.default <- glm(default~ balance + income, data= train, family = "binomial")
  
  #predict validation set
  glm.prod <- predict(glm.default, valid, type = "response")
  glm.prod <- ifelse(glm.prod > 0.5, "Yes", "No")
  
  AC[i] <- mean(glm.prod == valid[,1])
  
}
AC #check results

AC <- NULL
for (i in 1:3){
  
  index <- sample(10000, 8000,replace=F)
  train <- Default[index,] #train set
  valid <- Default[-index,] #validation set
  
  #fit logistic model
  glm.default <- glm(default~ ., data= train, family = "binomial")
  
  #predict validation set
  glm.prod <- predict(glm.default, valid, type = "response")
  glm.prod <- ifelse(glm.prod > 0.5, "Yes", "No")
  
  AC[i] <- mean(glm.prod == valid[,1])
  
}
AC # 넣는게 좋은거 같은데..?






