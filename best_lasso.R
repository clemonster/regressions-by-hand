library(CVXR)

fit_lasso <- function(x, y, t){
  ys = scale(y)
  xs = scale(x[,-1])
  betaHat <- Variable(dim(xs)[2])
  objective <- Minimize(sum((ys - xs %*% betaHat)^2))
  constraint <- list(sum(abs(betaHat)) <=  t)
  problem <- Problem(objective, constraint)
  b_lasso <- solve(problem)
  return(c(mean(y), b_lasso$getValue(betaHat)))
}

ts <- seq(from = 0, to = 3, length.out = 100)
errors <- numeric(length(ts))
std_errors <- numeric(length(ts))
betas <- matrix(nrow = length(ts), ncol = 9)

for (i in 1:length(ts)){
  print(i)
  fit <- fit_cv(Xtrainone, Ytrain, function(x,y) fit_lasso(x=x, y=y, t = ts[i]))
  
  errors[i] <- fit$mean_err
  std_errors[i] <- fit$std_err
  betas[i,] <- fit$coeff
}



best_err <- min(errors)
best_std <- std_errors[which.min(errors)]
print(betas[which.min(errors),])

#We have the betahat result for the scaled regression problem, we need to scale back to get betas for original data.
d <- sqrt(diag(var(Xtrain)))
bl <- result$getValue(betaHat)*sqrt(var(Ytrain)) / d
round(1000*bl)/1000
