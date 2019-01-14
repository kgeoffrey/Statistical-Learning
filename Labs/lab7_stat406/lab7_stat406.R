## LAB 7

rm(list = ls())
setwd("../Labs/lab7_stat406/")

data <- read.csv("iris.txt", sep = ",", header = TRUE)

library(rpart)

set.seed(800)

myc  <-  rpart.control(minsplit=3, cp=1e-8)
tree  <- rpart(y ~ sepal + petal, data=data, method= 'class', control=myc)

plot(tree)

x1 <- c(4.9, 5.3)
x2 <- c(5.9, 4.8)

newgrid <- expand.grid(x1, x2)
names(newgrid) <- c("sepal", "petal")

pr.new <- predict(tree, newdata = newgrid, type = 'prob')
pr.new
