## In this problem, you will use simulation to evaluate (by Monte Carlo) 
## the expected misclassification error rate given a particular generating model.
## Let yi be equally divided between classes 0 and 1, and let xi∈ℝ10 be normally
## distributed.

## Given yi=0, xi∼N10(0,I10).  Given yi=1, xi∼N10(μ,I10) with μ=(1,1,1,1,1,0,0,0,0,0).
## Now, we would like to know the expected test error rate if we fit an SVM to a 
## sample of 50 random training points from class 1 and 50 more from class 0.  
## We can calculate this to high precision by 
## 1  generating a random training sample to train on, 
## 2  evaluating the number of mistakes we make on a large test set
## 3  repeating (1-2) many times and averaging the error rate for each trial.

## Aside: in real life don't know the generating distribution, so we have to use 
## resampling methods instead of the procedure described above.

## For all of the following, please enter your error rate as a number between zero and 1 
## (e.g., 0.21 instead of 21 if the error rate is 21%).
library(MASS)
library(e1071)
I10 <- diag(rep(1,10))
mean2 <- c(rep(1,5), rep(0,5))
mean1 <- c(rep(0,10))

generate_train <- function(){
  class1 <- mvrnorm(n = 50, mu = mean1, Sigma = I10)
  class2 <- mvrnorm(n = 50, mu = mean2, Sigma = I10)
  dat1 <- cbind(rep(0,50), class1)
  dat2 <- cbind(rep(1,50), class2)
  dat  <- rbind(dat1, dat2)
  dat  <- as.data.frame(dat)
  names(dat) <- c("label","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
  dat
}
add_name <- function(dat){
  names(dat) <- c("label","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
  dat
}

generate_test <- function(n){
  tmp1 = data.frame(rep(0,n), mvrnorm(n=n, mu = mean1, Sigma = I10))
  tmp1 <- add_name(tmp1)
  tmp2 = data.frame(rep(1,n), mvrnorm(n=n, mu = mean2, Sigma = I10))
  tmp2 <- add_name(tmp2)
  dat <- rbind(tmp1, tmp2)
  dat <- add_name(dat)
}


n <- 100
test_mse_logestic = rep(0,n)
test_mse_radial = rep(0,n)
test_mse_linear = rep(0,n)
for(i in 1:n){
  data.train <- generate_train()
  svm.fit <- svm( as.factor(label) ~ ., data = data.train, kernel = "radial", shrinking = TRUE,cost = 1)
  svm_linear.fit  <- svm( as.factor(label) ~ ., data = data.train, kernel = "linear", shrinking = TRUE,cost = 1)
  glm.fit <- glm(label ~ . , data = data.train, family = "binomial")
  summary(glm.fit)
  data.test <- generate_test(100)
  pred.svm <- predict(svm.fit, data.test[,-1])
  pred.svm_linear <- predict(svm_linear.fit, data.test[,-1])
  glm.probs <- predict(glm.fit, data.test[,-1], type = "response")
  pred.glm<- ifelse(glm.probs > .5, 1, 0)
  #table(data.test[,1], pred.svm)
  test_mse_radial[i] = 1 - sum(data.test[,1] == pred.svm) / 200
  test_mse_linear[i] = 1 - sum(data.test[,1] == pred.svm_linear) / 200
  test_mse_logestic[i] = 1 - sum(data.test[,1] == pred.glm) / 200
}
mean(test_mse_radial)
mean(test_mse_linear)
mean(test_mse_logestic)


