## LAB3

setwd("~../Labs/lab3_stat406/")
dat <- read.table("prostate_mod-2.data", header = TRUE)
library(glmnet)

y <- as.vector(dat$lpsa)
xm <- as.matrix(dat[, -9])

mdl <- glmnet(x=xm, y=y, alpha=1)
plot(mdl, xvar="lambda", label = TRUE)


set.seed(800)
tmp <- cv.glmnet(x=xm, y=y, nfolds = 5, alpha = 1)
tmp$lambda.min

lmbd <- 0.1036847

tmp2 <- glmnet(x=xm, y=y, alpha=1, lambda=lmbd)
coef(tmp2)

X = scale(xm, center = TRUE)
U = svd(X)
D = (U$d)^2
edf=D/(exp(-1.2)+D)
edf = sum(edf)
edf

plot(tmp)
