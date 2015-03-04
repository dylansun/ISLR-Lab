require(ISLR)
x = model.matrix(Salary ~ . , Hitters)[,-1]
y = Hitters$Salary

## glmnet() function has an alpha argument that determines what
## type of model is fit. If alpha = 0 then a ridge regression model is fit
## and if alpha=1 then lasso model is fit. 
library(glmnet)
grid = 10^seq(10,-2, length = 100)
ridge.mod = glmnet(x,y, alpha = 0, lambda = grid)

dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod, s= 50, type="coefficients")[1:20,]

set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y[test])^2)

## predict using a large lambda 
ridge.pred = predict(ridge.mod, s = 1e10, newx =x[test,])
mean((ridge.pred - y.test)^2)

## predict using lambda =0, actually it's just least squares
ridge.pred = predict(ridge.mod, s=0, newx=x[test,],exact = T)
mean((ridge.pred - y[test])^2)


## cv.glmnet() function is a built-in cross-validation, though this
## can be changed using the argument nfolds.
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y[test])^2)

out = glmnet(x,y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

## The lasso
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha =1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y[test])^2)

out = glmnet(x,y, alpha =1 , lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
