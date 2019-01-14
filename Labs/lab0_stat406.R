## Lab 0 - STAT 406 (

## Question 1
linear <- function(y, X) {
  beta <- solve(((t(X)%*%X)^(-1))%*%t(X)%*%y)
  return(beta)
}

## Question 2

# assumes number of observations is a multiple of 5
linear2 <- function(y, X) {
  if (X[,1] == rep(1, 5)) x<- X[,2]
  if (X[,5] == rep(1, 5)) x<- X[,4]

  obs <- data.frame(x, y)

  training <- obs[sample(nrow(obs), 4, replace = FALSE), ]

  beta <- linear(training[,2], training[,1])
  yhats <- X%*%beta
  yhat <- 0
  validation <- matrix(, nrow=5, ncol = 0)

  for (i in x) {
    if (!(x[i] %in% training)) validation <- data.frame(x[i], y[i])
    yhat <- yhats[i]
  }

  MSE <- (validation[1,2]-yhat)^2

  result = c(beta, MSE)

  return(result)
}
