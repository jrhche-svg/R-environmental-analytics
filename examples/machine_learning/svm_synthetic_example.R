rm(list = ls()) 
graphics.off()

set.seed(3)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))


dat=data.frame(x=x, y=as.factor(y))
library(e1071)
# The support vectors are plotted as crosses and the remaining observations are plotted as circles
svmfit=svm(y~., data=dat, kernel="linear", cost=2,scale=FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

svmfit2 = svmfit=svm(y~., data=dat, kernel="radial",  gamma=1, cost=1)
plot(svmfit2, dat)
svmfit2$index
summary(svmfit2)