require(ISLR)
attach(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

x = model.matrix(Salary ~ . , Hitters)[,-1]
y = Hitters$Salary
grid = 10^seq(10,-2, length = 100)

set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred.train.mse = rep(0,100)
ridge.pred.test.mse = rep(0,100)
for(i in 1:100){
  ridge.pred = predict(ridge.mod, s = grid[i], newx=x[train,])
  ridge.pred.train.mse[i] = mean((ridge.pred - y[train])^2)
  
  ridge.pred = predict(ridge.mod, s = grid[i], newx=x[test,])
  ridge.pred.test.mse[i] = mean((ridge.pred - y[test])^2)
}

ridge.pred.var = rep(0,100)
ridge.pred.bias = rep(0,100)
ridge.pred.irr = rep(0, 100)
ridge.pred.mse = rep(0,100)
for(i in 1:100){
  ridge.pred = predict(ridge.mod, s = grid[i], newx = x[test,])
  Eyhat = mean(ridge.pred)
  ridge.pred.bias[i] = (mean(y[test] - Eyhat))^2
  ridge.pred.var[i] = mean((ridge.pred - Eyhat)^2)
  ridge.pred.mse[i] = mean((ridge.pred - y[test])^2)
  ridge.pred.irr[i] = ridge.pred.mse[i] - ridge.pred.bias[i]-ridge.pred.var[i]
}
par(mfrow = c(2,2))
plot(ridge.pred.bias,ylab = "bias squared", col = "blue", pch = 20)
plot(ridge.pred.var, ylab = "Variance", col = "blue", pch = 20)
plot(ridge.pred.mse, ylab = "MSE", col = "blue", pch = 20)
plot(ridge.pred.irr, ylab = "IrrError", col = "blue", pch = 20)

## The answer of the quiz is not consistent with this plot
## since the answer is judge in a general rule rather than a specific case
## 
