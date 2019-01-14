## LAB 4

rm(list=ls())
setwd("../Labs/lab4_stat406")
library(MASS)

## Question 1
dat.tr <- read.table("mcycle-1.train.txt", header = TRUE)
dat.test <- read.table("mcycle-1.test.txt", header = TRUE)

k <- 5; kn1 <- as.numeric(quantile(dat.tr$times, (1:k)/(k+1)))
k <- 10; kn2 <- as.numeric(quantile(dat.tr$times, (1:k)/(k+1)))
k <- 20; kn3 <- as.numeric(quantile(dat.tr$times, (1:k)/(k+1)))
rm(k)

## Question 2
library(splines)

# fit the regression models
fit1 <- lm(accel ~ bs(times, degree = 3, knots = kn1), data = dat.tr)
fit2 <- lm(accel ~ bs(times, degree = 3, knots = kn2), data = dat.tr)
fit3 <- lm(accel ~ bs(times, degree = 3, knots = kn3), data = dat.tr)

# make plots
predict1 <- lm(predict(fit1)[order(times)] ~ sort(times), data = dat.tr)
predict2 <- lm(predict(fit2)[order(times)] ~ sort(times), data = dat.tr)
predict3 <- lm(predict(fit3)[order(times)] ~ sort(times), data = dat.tr)

plot(accel ~ times, data = dat.tr, pch = 19, col = "gray", cex = 1.5)
lines(predict1, lwd = 6, col = "tomato3")

plot(accel ~ times, data = dat.tr, pch = 19, col = "gray", cex = 1.5)
lines(predict(fit2)[order(times)] ~ sort(times), data = dat.tr, lwd = 6, col = "tomato3")

plot(accel ~ times, data = dat.tr, pch = 19, col = "gray", cex = 1.5)
lines(predict(fit3)[order(times)] ~ sort(times), data = dat.tr, lwd = 6, col = "tomato3")

## Question 3
library(mosaic)

mosaic::MSPE(predict1, data=dat.tr)

pr.fit1 <- pr.fit2 <- pr.fit3 <- rep(0, nrow(dat.test))

for (i in 1:nrow(dat.test)) {
  pr.fit1[i] <- predict(fit1, newdata = dat.test)
  pr.fit2[i] <- predict(fit2, newdata = dat.test)
  pr.fit3[i] <- predict(fit3, newdata = dat.test)
}

# mspe1 <- mean((dat.test$accel - pr.fit1)^2)
# mspe2 <- mean((dat.test$accel - pr.fit2)^2)
# mspe3 <- mean((dat.test$accel - pr.fit3)^2)

mspe1 <- mean((dat.test$accel-predict.lm(fit1, data=dat.test))^2)
mspe2 <- mean((dat.test$accel-predict.lm(fit2, data=dat.test))^2)
mspe3 <- mean((dat.test$accel-predict.lm(fit3, data=dat.test))^2)

mspe1
mspe2
mspe3

## Question 5
library(psych)

S1 = bs(dat.tr$times, degree = 3, knots = kn1)
S2 = bs(dat.tr$times, degree = 3, knots = kn2)
S3 = bs(dat.tr$times, degree = 3, knots = kn3)
tr(S1)
# matrix.trace(S1)
dim(S1)
dim(S2)
dim(S3)

pr.fit1
