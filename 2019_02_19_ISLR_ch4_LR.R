## 2019_02_19_ISLR_ch4_LR
## reference: http://www-bcf.usc.edu/~gareth/ISL/code.html


getwd()
setwd("C:/rstudio/0_2019_spring/statistical learning/islr")
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9])
attach(Smarket)
?attach
#  objects in the database can be accessed by simply giving their names.

plot(Volume)
plot(Year, Volume)

## logistic regression

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial) 
summary(glm.fits)

coef(glm.fits)

summary(glm.fits) $ coef
summary(glm.fits) $ coef[,4]

glm.probs = predict(glm.fits, type ="response") # response for P(Y=1|X)
class(glm.probs) # numeric
glm.probs[1:10]
contrasts(Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs > .5] ="Up"

table(glm.pred, Direction) # confusion matrix
(507 + 145) / 1250         # accuracy = true prediction/ number of samples
mean(glm.pred == Direction)

train =(Year<2005)
train
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005) # 252 9
Direction.2005 = Direction[!train]

# fitting logistic regression with train data as a subset funtion
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +Volume, data = Smarket, family = binomial, subset = train)

# prediction ylabel in test set
glm.probs = predict(glm.fits, Smarket.2005, type ="response") # predcition, fitted weight, y_test_label
length(glm.probs) # 252, the number of sample in test set

# outcome
glm.pred= rep("Down", 252)
glm.pred[glm.probs <.5] ="Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # mean of correct prediction
mean(glm.pred != Direction.2005) # mean of incorrect perdiction

# predcition with specific value of x1 x2

glm.fits = glm(Direction ~ Lag1 + Lag2, data = Smarket, family  = binomial, subset = train) # only using 2 features

predict(glm.fits, newdata = data.frame(Lag1 =c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type ="response")


