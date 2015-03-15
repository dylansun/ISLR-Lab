load("~/Documents/elementsStasLearn/stat-learning/ch5/5.R.RData")
require(boot)
lm.fit=lm(y~., data = Xy)
summary(lm.fit)

matplot(Xy,type="l")

lm.fn <- function(data, index){
  lm.fit <- lm(y~., data = data[index,])
  lm.fit$coefficients[2]
}
n <- length(Xy[,1])

## do once
lm.fn(Xy, sample(1:n, n, replace = TRUE ))

## using boot to repeat 100 times
boot.out = boot(Xy, lm.fn, R = 1000)
plot(boot.out)


## block bootstrap 
## One of your bootstrap resamples would like,
## new.rows = c(101:200, 401:500, 101:200, 901:1000, 301:400, 1:100, 1:100, 801:900, 201:300, 701:800)
## new.Xy = Xy[new.rows, ]

attach(Xy)
alpha=function(x1,x2,y){ 
  (summary(glm(y~x1 + x2))$coefficients)[2,1] 
}
alpha.fn=function(data, index){ 
  a<-sample(1:10,10,replace=T) 
  newrow =integer()
                                
  for (i in 1:10) { 
    oldrow=newrow 
    v=(1+(a[i]-1)*100):(a[i]*100) 
    newrow=c(oldrow, v) 
  }
  new.data = data[newrow, ] 
  with(new.data[index,],alpha(X1,X2,y))
}
cv = rep(0,1000) 
for (j in 1:1000) { 
  cv[j]=alpha.fn(Xy,1:1000) 
}
sd(cv)



