fit_ridge <- function(x, y, lambda){
  ys <- scale(y)
  xs <- scale(x[,-1])
  
  b_ridge <- solve(t(xs) %*% xs + (lambda * diag(dim(xs)[2])), t(xs) %*% ys)
  d <- sqrt(diag(var(x[,-1])))
  b_ridge <- b_ridge * sqrt(var(y)) / d
  
  return(c(mean(y), b_ridge))
}

lambdas <- (0:100)
errors <- numeric(length(lambdas))
std_errors <- numeric(length(lambdas))
betas <- matrix(nrow = length(lambdas), ncol = 9)

for (i in 1:length(lambdas)){
  fit <- fit_cv(Xtrainone, Ytrain, function(x,y) fit_ridge(x=x, y=y, lambda = lambdas[i]))
  
  errors[i] <- fit$mean_err
  std_errors[i] <- fit$std_err
  betas[i,] <- fit$coeff
}



best_err <- min(errors)
best_std <- std_errors[which.min(errors)]
print(betas[which.min(errors),])

#Now remember, we want the most parsimonious model within one std error of the best model
#Most parsimonious = lowest degrees of freedom
#df is a monotone decreasing function of lambda
#So want model with the highest lambda, within one std error of the best model

errors[errors > best_err - .5*best_std & errors < best_err + .5*best_std]
betas[83,]
betas[26,]

#Still need to figure this one out.