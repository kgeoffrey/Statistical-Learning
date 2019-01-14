## LABB1

# setwd("../Labs/lab1_stat406")

dat <- read.table("Lab 1.data")

head(dat)

fullmodel = lm(lpsa ~., data=dat)
fullmodel
partialmodel = lm(lpsa ~ lcavol+lcp+svi, data=dat)
partialmodel
cor(dat)

summary(fullmodel)
summary(partialmodel)

# Question 2

set.seed(400)
gpind <- sample(rep(1:3, each=32))

for (i in 1:3) {
  train <- dat[gpind != 3, ]
  test <- dat[gpind == 3, ]
  fulltrain <- lm(lpsa ~., data=train)
  partialtrain <- lm(lpsa ~ lcavol+lweight+svi, data=train)

  pr.full <- predict(fulltrain, newdata=test)
  pr.partial <- predict(partialtrain, newdata=test)
}

cor(train)

mspe_full <- mean( (dat$lpsa - pr.full)^2 )
mspe_partial <- mean( (dat$lpsa - pr.partial)^2 )
