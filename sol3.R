# 8. This question involves the use of simple linear regression on 
# the Auto data set.
# (a) Use the lm() function to perform a simple linear regression 
# with mpg as the response and horsepower as the predictor. 
# Use the summary() function to print the results. 
# Comment on the output. For example:
Auto <- read.csv("Auto.csv", head = T, na.strings="?")
names(Auto)
lm.fit <- lm(mpg~horsepower, data = Auto)
summary(lm.fit)
# i. Is there a relationship between the predictor and the response?
# ii. How strong is the relationship between the predictor and the response?
# iii. Is the relationship between the predictor and the response positive or negative?
# iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?
