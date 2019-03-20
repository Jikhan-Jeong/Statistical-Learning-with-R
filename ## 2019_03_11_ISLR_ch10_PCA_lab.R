## 2019_03_19_PCA_ISLR
## reference: https://altaf-ali.github.io/ISLR/chapter10/lab.html

library(ISLR)
library(ElemStatLearn)
library(e1071)

getwd()
setwd('C:/rstudio/0_2019_spring/statistical learning')

states = row.names(USArrests)
states
names(USArrests)
head(USArrests,3)

apply(USArrests, 2, mean) # 1 row, 2 column
apply(USArrests, 2, var)  # large differce in variation so we need to scale, normalization

pr.out = prcomp(USArrests, scale = TRUE)
names(pr.out)

# center and scale components correspond to the means and standard deviations 
# of the variables that were used for scaling prior to implementing PCA.

pr.out$center
pr.out$scale

# pr.out$rotation contain the corresponding pricial compoent loading vector
# This function names it the rotation matrix, because when we matrix-multiply the
# X matrix by pr.out$rotation, it gives us the coordinates of the data in the rotated
# coordinate system. These coordinates are the principal component scores.

pr.out$rotation # principal component loading vector

# prcomp() function, we do not need to explicitly multiply the
# data by the principal component loading vectors in order to obtain the
# principal component score vectors. 

dim(pr.out$x)
biplot(pr.out, scale =0) # arrows are scaled to represent the loadings

pr.out$rotation = -pr.out$rotation

# prcomp() function also outputs the standard deviation of each principal component.

pr.out$sdev # sd of each pricipal componet

pr.var = pr.out$sdev ^ 2
pr.var  # the variance explained by each principal component is obtained by squaring these

# To compute the proportion of variance explained by each principal component, 
# we simply divide the variance explained by each principal component
# by the total variance explained by all four principal components

pve = pr.var/sum(pr.var) # the proportion of variance explained (PVE)
pve

# We can plot the PVE explained by each component, as well
# as the cumulative PVE, as follows

plot(pve, xlab ="pc", ylab ="proportion of variance explained", ylim =c(0,1), type ='b')
plot(cumsum(pve), xlab ="principal component", ylab=" cumulative proportion of variance explained",
     ylim =c(0,1), type='b')

a=c(1,2,8,-3)
cumsum(a)

#_______________10.6.1 PCA on NCI60

pr.out = prcomp(nci.data, scale=TRUE)

# The observations (cell lines) corresponding to a given
# cancer type will be plotted in the same color

Cols = function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# rainbow() function takes as its argument a positive integer, rainbow()
# and returns a vector containing that number of distinct colors
# ex) rainbow(3) [1] "#FF0000FF" "#00FF00FF" "#0000FFFF"

par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="z1", ylab ="z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="z1", ylab="z3")

summary(pr.out)
plot(pr.out) # variance, pr.out$sdev

(pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2))

# pve can be directly calculated from summary

(pve = summary(pr.out)$importance[2,])
(cumsum_pve = summary(pr.out)$importance[3,])

par(mfrow=c(1,2))
plot(pve, type="o", ylab ="PVE", xlab="pc", col ="blue")
plot(cumsum(pve), type="o", ylab ="Cumulative PVE", xlab ="pc", col ="brown3")
