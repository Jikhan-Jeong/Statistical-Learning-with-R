## 2019_03_02_ISLR_K_means_p404
## reference: https://github.com/asadoughi/stat-learning


set.seed(2)
x = matrix(rnorm(50*2), ncol=2) # 50*2 sampling generation
x
## mean shifting
x[1:25,1] = x[1:25,1] +3
x[1:25,2] = x[1:25,2] -4

km.out = kmeans(x,2,nstart=20) # clustering with K=2
## if centers is a number, how many random sets should be chosen

km.out$cluster
?kmeans

plot(x, col=(km.out$cluster+1), main ="K-Means Clustering Results with K=2", xlab ="", ylab="", pch=20, cex=2)

set.seed(4)
km.out = kmeans(x,3, nstart=20)
km.out

set.seed(3)
km.out = kmeans(x,3,nstart=1)
km.out$tot.withinss # 104.3319 # total within-cluster sum of suqares, which we seek to minimizae by performation clstuering
km.out = kmeans(x,3,nstart=30)
km.out$tot.withinss # 97.97927

#Q10 p.417
set.seed(2)
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1
# The concept here is to separate the three classes amongst two dimensions.

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))
c(rep(1,20), rep(2,20), rep(3,20))
# ?rep # rep replicates the values in x. 
# rep(1:4,2) #1 2 3 4 1 2 3 4

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

