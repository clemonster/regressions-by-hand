dummies = expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
errors <- numeric(dim(dummies)[1])
std_errors <- numeric(dim(dummies)[1])
betas <- matrix(nrow = dim(dummies)[1], ncol = 9)

for (i in (1:dim(dummies)[1])){
  coeff = dummies[i, ]
  curX <- cbind(array(1, dim = c(nrow(Xtrain),1)), Xtrain[,which(coeff == 1)])
  
  fit <- fit_cv(curX,Ytrain, fit_lin_reg)
  errors[i] = fit$mean_err
  std_errors[i] = fit$std_err
  
  result <- numeric(8)
  result[which(coeff == 1)] <- fit$coeff[-1]
  result <- c(fit$coeff[1], result)
  betas[i, ] <- result
}

best_err <- min(errors)
best_std <- std_errors[which.min(errors)]

errors[errors > best_err - .5*best_std & errors < best_err + .5*best_std]

rowSums(dummies[errors > best_err - .5*best_std & errors < best_err + .5*best_std,])

betas[4,]

#print(betas[which(errors),])
