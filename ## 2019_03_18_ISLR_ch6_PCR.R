## 2019_03_11_ISLR_ch6_PCRegression
## reference: https://altaf-ali.github.io/ISLR/chapter6/lab.html

library(ISLR)
head(Hitters)

getwd()
setwd('C:/rstudio/0_2019_spring/statistical learning')
install.packages('pls')
library(pcr)
library(pls)

set.seed(1)
pcr.fit <- pcr(Salary ~., data = Hitters, scale = TRUE, validation ="CV") 
summary(pcr.fit)

validationplot(pcr.fit, val.type ="MSEP")

set.seed(2)
train <- sample(1:nrow(x), nrow(x)/2)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)

pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")


