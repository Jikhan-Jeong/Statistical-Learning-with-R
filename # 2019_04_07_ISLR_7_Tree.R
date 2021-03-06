# 2019_04_07_ISLR_8_lab_tree
install.packages('tree')
library(tree)
library(ISLR)
attach(Carseats)
head(Carseats)
High = ifelse(Sales <=8, "NO", "Yes")
Carseats = data.frame(Carseats, High)
tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High~.-Sales, Carseats, subset = train) # fitting with subset = training set
tree.pred = predict(tree.carseats, Carseats.test, type ="class") # classification
table(tree.pred, High.test)
(86+57)/200

# 327
