library(ISLR)
attach(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

## stepwise feature selection
library(leaps)
## regsubsets() performs best subset selection by identifying the 
## the best model that contains a given number of predictors,
## where best is quantified using RSS. 

regfit.full = regsubsets(Salary ~ ., Hitters)

## In summary(reffit.full) an asterisk indicates that a given variable is
## included in the corresponding model. 
reg.summary = summary(regfit.full)

## the sumamry() function also returns R^2, RSS, C_p, and BIC. 
names(summary(regfit.full))

## View R^2, rsq is short for "r squared"
summary(regfit.full)$rsq

## Noto the type = "l" option tells R to connect the plottd points with lines
par(mfrow = c(2,2))
plot(summary(regfit.full)$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(summary(regfit.full)$adjr2, xlab = "Number of Variables", ylab = "Adjusted R squared", type = "l")

## The points() command works like the plot() command, except it pus
## points on a plot that has already been created, instead of creating
## a new plot. 
## The which.max() function can be used to identify the location of
## the maximum point of vector.
which.max(reg.summary$adjr2)
points(8, reg.summary$adjr2[8], col = "red", pch = 20)

## forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

## choosing among models using the validation approach and Cross-validation
set.seed(1)
train = sample(c(TRUE,F), nrow(Hitters), rep = T)
test = (!train)
regfit.best = regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)

## make a model matrix
## The model.matrix() function is used in many regression packages 
## for building an "X" matrix from data. 
test.mat = model.matrix(Salary ~ . , data = Hitters[test,])
val.errors = rep(NA, 19)
for( i in 1:19){
  coefi = coef(regfit.best, i)
  pred = test.mat[, names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}

val.errors

predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

## choose best ten-variable model on the full data set 
regfit.best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

## we now choose among the models of different sizes using cross-validation
## This approach is somewhat involved, as we must perform best subset selction
## with each of the k trainning sets. 
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k , 19, dimnames = list(NULL, paste(1:19)))

## Now we write a for loop that perform cross-validation. 
for(j in 1:k){
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds != j, ] , nvmax = 19)
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds==j,], id = i)
    cv.errors[j,i] = mean((Hitters$Salary[folds == j] - pred) ^ 2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')

## cross-validation selects an 11-variable model
reg.best = regsubsets(Salary ~ . , data = Hitters, nvmax = 19)
coef(reg.best, 11)
