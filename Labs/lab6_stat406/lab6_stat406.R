## LAB 6

rm(list=ls())
setwd("../Labs/lab6_stat406/")
library(MASS)

dat <- read.table("Iris-1.txt", header=TRUE, sep=",")

x1 <- dat$sepal
x2 <- dat$petal
y <- dat$y

plot(x1, x2, pch=c(19,1)[y+1], xlab="Sepal", ylab="Petal", xlim=c(3,8), ylim=c(3,8))

a.lda <- lda(dat$y ~ dat$sepal + dat$petal)

x.sepal <- seq(3, 8, length=100)
x.petal <- seq(3, 8, length=100)
the.grid <- expand.grid(x1, x2)
names(the.grid) <- c("sepal", "petal")

pr.lda <- predict(a.lda, newdata=the.grid)$posterior

image(x.sepal, x.petal, matrix(pr.lda[, 2], 100, 100), col=terrain.colors(100), ylab='sepal', xlab='petal', main='LDA')
points(petal ~ sepal, data=dat, pch=19, cex=1.5,col=c('red', 'blue')[y+1])

contour(x.sepal, x.petal, matrix(pr.lda, 100, 100),
        levels = .5, add = TRUE, drawlabels = FALSE, lwd=4, col='maroon')

# Q3
sepal <- c(4.9, 5.3)
petal <- c(5.9, 4.8)

# new.grid <- matrix(c(x1new, x2new), nrow = 2, ncol = 2, byrow = T)
# new.grid <- as.data.frame(new.grid)
# colnames(new.grid) <- c("sepal", "petal")
# new.grid

dat.te <- data.frame(sepal, petal)
predict(a.lda, newdata = dat.te)$posterior

# pr <- predict(a.lda, newdata=new.grid)$posterior

# pr
