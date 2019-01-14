## LABB 8 

rm(list=ls())
setwd("../Labs/lab8_stat406/")

dat.tr <- read.csv("training.txt", header = TRUE)
dat.te <- read.csv('test.txt', header=TRUE)

dat.te$V14 <- as.factor(dat.te$V14)
dat.tr$V14 <- as.factor(dat.tr$V14)

library(randomForest)

set.seed(200)
a.rf <- randomForest(V14~., data=dat.tr,ntree=1501 )
varImpPlot(a.rf)
set.seed(200)
a.rf2 <- randomForest(V14~., data=dat.tr, ntree=1501, mtry=13)

length(a.rf$predicted[a.rf$predicted == 'Y'])
a.rf$oob.times

pr.f <- predict(a.rf , newdata = dat.te, type='class')

table(pr.f, dat.te$V14)

length(a.rf$oob.times[a.rf$oob.times == 564])

plot(a.rf, lwd=3, lty=1)
