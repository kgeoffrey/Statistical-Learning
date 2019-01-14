## LAB 9

rm(list=ls())
setwd("../Labs/lab9_stat406")

dat <- read.table("data.txt", header = T, sep=",")
dat

kmobj <- kmeans(dat, centers = 2, nstart=50)
kmobj$withinss
kmobj
plot(dat, col=kmobj$cluster)

sums <- vector()
k <- vector()

for (i in 1:10) {
  kmo <- kmeans(dat, centers = i, nstart = 50)
  sums <- c(sums, sum(kmo$withinss))
  k <- c(k, i)
}

plot(k, sums)

## Question 4
library(mclust)

clust <- Mclust(dat)
plot(dat, col=clust$classification)

summary(clust)

kmobj2 <- kmeans(dat, centers = 4, nstart=50)
kmobj2$centers
plot(dat, col=kmobj2$cluster)
