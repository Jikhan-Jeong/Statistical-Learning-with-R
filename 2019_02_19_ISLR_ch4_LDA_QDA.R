## 2019_02_19_ISLR_ch4_LDA_QDA
## reference: http://www-bcf.usc.edu/~gareth/ISL/code.html


getwd()
setwd("C:/rstudio/0_2019_spring/statistical learning/islr")
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
attach(Smarket)


## LDA data setting
library(MASS)

train =(Year<2005)
train
class(train) # logical

Smarket.2005 = Smarket[!train,]
Direction.2005 = Direction[!train]



## LDA 
(lda.fit =lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train))
plot(lda.fit)

head(Smarket.2005,3)
lda.pred = predict(lda.fit, Smarket.2005) # prediction with fitted value from trainset and predict with test set
names(lda.pred) # prediction returns three elements including class, posterior, x in the test set

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)


## recreate the class by changing the thresholds to the posterior probabilities

lda.pred$posterior[,1]
sum(lda.pred$posterior[,1] >=.5)
sum(lda.pred$posterior[,1] <.5)


lda.pred$posterior[1:20, 1]
lda.class[1:20]


## qda = syntax is equal to lda

qda.fit = qda(Direction ~Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
