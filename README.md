# ESC_ML_2
ESC 머신러닝 스터디 2조 과제 및 실습 자료 업로드 공간입니다.

5.4 Exercises
Conceptual
1. Using basic statistical properties of the variance, as well as singlevariable
calculus, derive (5.6). In other words, prove that α given by
(5.6) does indeed minimize Var(αX + (1 − α)Y ).

5. In Chapter 4, we used logistic regression to predict the probability of
default using income and balance on the Default data set. We will
now estimate the test error of this logistic regression model using the
validation set approach. Do not forget to set a random seed before
beginning your analysis.
(a) Fit a logistic regression model that uses income and balance to
predict default.
(b) Using the validation set approach, estimate the test error of this
model. In order to do this, you must perform the following steps:
i. Split the sample set into a training set and a validation set
ii. Fit a multiple logistic regression model using only the training
observations.
iii. Obtain a prediction of default status for each individual in
the validation set by computing the posterior probability of
default for that individual, and classifying the individual to
the default category if the posterior probability is greater
than 0.5.
iv. Compute the validation set error, which is the fraction of
the observations in the validation set that are misclassified.
