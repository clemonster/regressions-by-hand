fit_cv <- function(x, y, FUN){
  err <- ten_fold_cv(x, y, FUN)
  coeff <- FUN(x, y)
  return(list(coeff = coeff, mean_err = err$mean_err, std_err = err$std_err))
}

ten_fold_cv <- function(x, y, FUN){
  require(caret)
  set.seed(78)
  flds <- createFolds(y, k = 10, list = TRUE)
  
  err <- numeric(10)
  for (i in 1:10){
    names(flds)[] <- "train"
    names(flds)[i] <- "test"
    fit <- FUN(x = x[unlist(flds[names(flds) != "test" ], use.names = FALSE), ],  
               y = y[unlist(flds[names(flds) != "test" ], use.names = FALSE)])
    
    pred <- x[flds$test, ] %*% fit
    err[i] <- mean((y[flds$test] - pred)^2)
  }
  return(list(mean_err = mean(err), std_err = sd(err))) 
}

fit_lin_reg <- function(x, y){
  #This function should be given a matrix of type Xone
  b <- solve(t(x) %*% x, t(x) %*% y)
  
  return(b)
}
