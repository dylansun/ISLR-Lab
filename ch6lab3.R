library(pls)
set.seed(2)
## The scale = TRUE has the effect of standardizing each predictor
pcr.fit = pcr(Salary ~ ., data = Hitters, scale = T, validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

## perform pcr on training set and evaluate its test performance
set.seed(1)
pcr.fit = pcr(Salary ~ ., data = Hitters, subset = train , scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred = predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit = pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

## Partial Least Squares
set.seed(1)
pls.fit = plsr(Salary ~ . , data = Hitters, subset = train , scale = T,validation = "CV")
summary(pls.fit)
