load("~/Documents/elementsStasLearn/ESL-ch2/7.R.RData")
plot(x,y)

lm.fit = lm(y~x)
summary(lm.fit)

## wrong answer ... fit = lm(y ~ poly(x,2))
fit = lm(y~1 +x + I(x^2)) 
summary(fit)
