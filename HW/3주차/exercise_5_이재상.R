# 3주차 과제_이재상

#5.1

# Var(αX + (1 − α)Y ) = Var(aX) + Var((1-a)Y) + 2Cov(aX, (1-a)Y) 
# = a^2Var(X) + (1-a)^2Var(Y) + 2a(1-a)Cov(X,Y) 
# = a^2*sigma_X^2 + (1-a)^2*sigma_Y^2 + 2a(1-a)sigma_XY

# 최소화되는 지점을 찾기 위해 a로 미분
#2a*sigma_X^2 - 2(1-a)*sigma_Y^2 + 2(1-2a)*sigma_XY = 0
#(sigma_X^2 + sigma_Y^2 - 2sigma_XY)*a -sigma_Y^2 + sigma_XY = 0
# a = (sigma_Y^2 - sigma_XY)/(sigma_X^2 + sigma_Y^2 - 2sigma_XY)
#(5.6)의 식 도출





#5.5
#(a)
library(ISLR)
str(Default) #explore data

#fitting logistic model
glm(default~ balance + income , data= Default, family = "binomial")


#(b)
#split the sample (7:3)
set.seed(100)
index <- sample(dim(Default)[1], dim(Default)[1]*0.7)
train_default <- Default[index,] #train set
valid_default <- Default[-index,] #validation set

#fit logistic model
glm.default <- glm(default~ balance + income , data= train_default, family = "binomial")

#predict validation set
glm.prod <- predict(glm.default, valid_default, type = "response")
glm.prod <- ifelse(glm.prod > 0.5, "Yes", "No")
table(glm.prod, valid_default[,1])
mean(glm.prod == valid_default[,1]) #0.976 / ER = 0.024


#(c)

ER <- c()
for (i in 1:3){
  
  index <- sample(dim(Default)[1], dim(Default)[1]*0.7)
  train_default <- Default[index,] #train set
  valid_default <- Default[-index,] #validation set
  
  #fit logistic model
  glm.default <- glm(default~ balance + income , data= train_default, family = "binomial")
  
  #predict validation set
  glm.prod <- predict(glm.default, valid_default, type = "response")
  glm.prod <- ifelse(glm.prod > 0.5, "Yes", "No")
  
  ER[i] <- mean(glm.prod == valid_default[,1])
  
}
ER #check results


#(d)
ER.full <- c()
for (i in 1:3){
  
  index <- sample(dim(Default)[1], dim(Default)[1]*0.7)
  train_default <- Default[index,] #train set
  valid_default <- Default[-index,] #validation set
  
  #fit logistic model
  glm.default <- glm(default~ . , data= train_default, family = "binomial")
  
  #predict validation set
  glm.prod <- predict(glm.default, valid_default, type = "response")
  glm.prod <- ifelse(glm.prod > 0.5, "Yes", "No")
  
  ER.full[i] <- mean(glm.prod == valid_default[,1])
  
}
ER.full #결과를 봤을 때 test error rate가 줄었다고 명확히 말하긴 어렵다.

#추가적으로 다른 cv를 진행해봐도 좋았을 듯하다!