## LAB 5

setwd("../Labs/lab5_stat406/")

library(MASS)
library(rpart)
dat <- load("CarData")

tree1 <- rpart(Mileage~., data = car.data, method = "anova", control = rpart.control(minsplit=5, cp=0.01))
plot(tree1, uniform = FALSE, margin = 0.05)

mse <- mean((car.data$Mileage-predict(tree1))^2)
mse

tree2 <- rpart(Mileage~., data = car.data, method = "anova", control = rpart.control(minsplit=20, cp=0.01))
plot(tree2)

mse2 <- mean((car.data$Mileage-predict(tree2))^2)
mse2

## Question 5
set.seed(400)
gpind <- sample(rep(1:7, each=7))

k <- 7
N <- nrow(car.data)
ii <- (1:N) %% k + 1
mspe.n <- mspe.st <- rep(0, N)

for (i in 1:N) {
  ii <- sample(ii)
  pr.n <- pr.st <- rep(0, N)
  for (j in 1:k) {
    # tmp.st <- update(tree1, data=car.data[ii != j, ])
    tmp.st <- rpart(Mileage~., data = car.data[ii != j, ], method = "anova", control = rpart.control(minsplit=5, cp=0.01))
    pr.st[ii == j] <- predict(tmp.st, newdata=car.data[ii == j, ])
    pr.n[ii == j] <- with(car.data[ii != j, ], mean(car.data$Mileage))
  }
  mspe.st[i] <- with(car.data, mean((car.data$Mileage - pr.st)^2))
  mspe.n[i] <- with(car.data, mean((car.data$Mileage - pr.n)^2))
}

summary(mspe.st)
summary(mspe.n)
