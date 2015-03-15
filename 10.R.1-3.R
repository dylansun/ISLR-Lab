rm(list = ls())
load("10.R.RData")
x.all <- rbind(x, x.test)
pr.out <- prcomp(x.all, scale = T)
## solution to 10.R.1
summary(pr.out)$importance[3,5]

## obtain the first 5 principal component
pc <- pr.out$rotation[,1:5]

## project the train to the principal componet
p1 = as.matrix(x) %*% as.matrix(pc[,1]) 
p2 = as.matrix(x) %*% as.matrix(pc[,2]) 
p3 = as.matrix(x) %*% as.matrix(pc[,3]) 
p4 = as.matrix(x) %*% as.matrix(pc[,4]) 
p5 = as.matrix(x) %*% as.matrix(pc[,5]) 
data <- data.frame(y,p1,p2,p3,p4,p5)
names(data) <- c("y","p1","p2","p3","p4","p5")
#View(data)

# learn a linear model
lm.pr.fit <- lm(y~., data = data)


## project the test to the principal componet
p1.test = as.matrix(x.test) %*% as.matrix(pc[,1]) 
p2.test = as.matrix(x.test) %*% as.matrix(pc[,2]) 
p3.test = as.matrix(x.test) %*% as.matrix(pc[,3]) 
p4.test = as.matrix(x.test) %*% as.matrix(pc[,4]) 
p5.test = as.matrix(x.test) %*% as.matrix(pc[,5]) 

data.test <- data.frame(p1.test,p2.test,p3.test,p4.test,p5.test)
names(data.test) <- c("p1","p2","p3","p4","p5")
View(data.test)

pred1 <- predict(lm.pr.fit,newdata = data.test)
## answer to 10.R.2
sd(y.test - pred1)


## 
train = data.frame(y,x)
lm.raw.fit <- lm(y~., data = train)
pred <- predict(lm.raw.fit, newdata = x.test)
sd(y.test - pred)
