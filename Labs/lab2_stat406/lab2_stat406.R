## Lab 2 - STAT 406

## Question 1

dat <- read.table("prostate_mod-1.data")

full <- lm(lpsa~., data=dat)
summary(full)
corr(full)
corr(dat)
cor(dat)

## Question 3

y <- as.vector(dat$lpsa)
xm <- as.matrix(dat[, -9])

exps = seq(from = -3, to = 3, by = 0.2)
lam = exp(exps)

fittedobject <- glmnet(x=xm, y=y, lambda=rev(lam),family='gaussian', alpha=0)
fittedobject$beta

## Question 4
set.seed(800)
tmp <- cv.glmnet(x=xm, y=y, lambda=lam, nfolds=5, alpha=0, family='gaussian')
plot(tmp)

lamdamin <- tmp$lambda.min
lamdamin
