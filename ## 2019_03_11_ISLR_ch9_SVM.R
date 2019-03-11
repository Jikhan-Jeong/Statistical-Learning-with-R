## 2019_03_11_ISLR_ch9_SVM
## reference: https://altaf-ali.github.io/ISLR/chapter9/lab.html

getwd()
setwd('C:/rstudio/0_2019_spring/statistical learning')

# 9.6.2 Support Vector Machine

# We begin by generating data with non-linear class boundaries.

library(e1071)

set.seed(1)
x <- matrix(rnorm(200*2), ncol =2)
x
x[1:100, ] <- x[1:100,] +2
x[1]

x[101:150,] <-x[101:150] -2
y <- c(rep(1,150), rep(2,50))
y
dat <-data.frame(x=x, y=as.factor(y))
plot(x, col=y)

#  split the data into training and test subsets and run the SVM classifier with kernel = "radial" 

train <-sample(200,100) # table, size of random sampling
?sample

svmfit <-svm(y ~., data =dat[train,], kernel ="radial", gamma =1, cost =1)
plot(svmfit, dat[train,])
summary(svmfit)

svmfit <-svm(y ~., data =dat[train,], kernel ="radial", gamma =1, cost =1e+05)
plot(svmfit, dat[train,])

## tuning the parameter

set.seed(1)
tune.out <- tune(svm, y~., data = dat[train,], kernel ="radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

table(true = dat[-train, "y"], pred =predict(tune.out$best.model, newdata = dat[-train,]))

# 9.6.3 ROC Curves
# We use the ROCR package to produce ROC curves on the predictions from the test subset.

library(ROCR)

rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

# Instead of getting the class labels, we can also get fitted values from svm()
# using decision.values = TRUE parameter.

svmfit.opt <- svm(y ~ ., data = dat[train,], kernel ="radial", gamma =2, cost =1, decision.values = TRUE)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values

# We generate the ROC curve with the rocplot() function. We can also change the value of (\gamma) 
# and see if it improves our predictions.

par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main ="Training data")

# the higer gamma
svmfit.flex <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 50, cost = 1, decision.values = TRUE) 
fitted <- attributes(predict(svmfit.flex, dat[train, ], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")

fitted <- attributes(predict(svmfit.opt, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train, ], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")


# 9.6.4 SVM with Multiple Classes
# The svm() function can also be used to classify observations from multiple-classes.


set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

svmfit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

## 9.6.5 Application to Gene Expression Data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)

# linear kernel and run SVM classifier on the training subset.

dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)

table(out$fitted, dat$y)


#We can then predict the classes on the test subset using the trained classifier.

dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)