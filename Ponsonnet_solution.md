---
title: "Regressions"
author: "Clement Ponsonnet"
output:
  html_document:
    keep_md : true
  html_notebook: default
---

## The objectives of the lab

The purpose of this lab is to reproduce tables from the third chapter of the book "Elements of
Statistical Learning" from Hastie, Tibshirani and Friedman. These tables can be found in the *tables_to_reproduce folder* (Table 3.1 and 3.2 for exercise 1, table 3.3 for exercise 2).

###Ex 1 - Tables 3.1 and 3.2


1) Prepare the data  
  a) Raw data is available on the web at http://statweb.stanford.edu/~tibs/ElemStatLearn.1stEd/datasets/prostate.data . It has already been downloaded and is in the *data* folder.
  

```r
data <- read.table("data/prostate.data", sep = "")
data[32,2] = 3.8044  # strange science 
```
b) Extract and normalize the explicative variables


```r
X <- scale(data[,1:8])
```
c) Is it wise to normalize these data?

```r
summary(data)
```

```
##      lcavol           lweight           age             lbph        
##  Min.   :-1.3471   Min.   :2.375   Min.   :41.00   Min.   :-1.3863  
##  1st Qu.: 0.5128   1st Qu.:3.376   1st Qu.:60.00   1st Qu.:-1.3863  
##  Median : 1.4469   Median :3.623   Median :65.00   Median : 0.3001  
##  Mean   : 1.3500   Mean   :3.629   Mean   :63.87   Mean   : 0.1004  
##  3rd Qu.: 2.1270   3rd Qu.:3.876   3rd Qu.:68.00   3rd Qu.: 1.5581  
##  Max.   : 3.8210   Max.   :4.780   Max.   :79.00   Max.   : 2.3263  
##       svi              lcp             gleason          pgg45       
##  Min.   :0.0000   Min.   :-1.3863   Min.   :6.000   Min.   :  0.00  
##  1st Qu.:0.0000   1st Qu.:-1.3863   1st Qu.:6.000   1st Qu.:  0.00  
##  Median :0.0000   Median :-0.7985   Median :7.000   Median : 15.00  
##  Mean   :0.2165   Mean   :-0.1794   Mean   :6.753   Mean   : 24.38  
##  3rd Qu.:0.0000   3rd Qu.: 1.1787   3rd Qu.:7.000   3rd Qu.: 40.00  
##  Max.   :1.0000   Max.   : 2.9042   Max.   :9.000   Max.   :100.00  
##       lpsa           train        
##  Min.   :-0.4308   Mode :logical  
##  1st Qu.: 1.7317   FALSE:30       
##  Median : 2.5915   TRUE :67       
##  Mean   : 2.4784                  
##  3rd Qu.: 3.0564                  
##  Max.   : 5.5829
```
In ordinary linear regression, we sometimes normalize data for numerical reasons, when they differ by a large order of magnitude. Here the ranges of the different variables are quite similar so it is not necessary to normalize. However here normalization was done by the textbook. Since we want to reproduce the tables of the textbook, we normalize our data.


```r
X <- scale(data[,1:8])
```

d) Extract the target variable


```r
Y <- as.matrix(data[,"lpsa"])
```

e) Split the data into train and test set


```r
Xtrain <- X[data[["train"]], ]
Ytrain <- Y[data[["train"]],]
Xtest <- X[!data[["train"]], ]
Ytest <- Y[!data[["train"]], ]
```

2. Compute the correlations of predictors in the prostate cancer data as presented in  **Table 3.1**


```r
Xtrainscale <- scale(Xtrain)
C <- cov(as.matrix(Xtrainscale))
round(1000*C)/1000
```

```
##         lcavol lweight   age   lbph    svi    lcp gleason  pgg45
## lcavol   1.000   0.300 0.286  0.063  0.593  0.692   0.426  0.483
## lweight  0.300   1.000 0.317  0.437  0.181  0.157   0.024  0.074
## age      0.286   0.317 1.000  0.287  0.129  0.173   0.366  0.276
## lbph     0.063   0.437 0.287  1.000 -0.139 -0.089   0.033 -0.030
## svi      0.593   0.181 0.129 -0.139  1.000  0.671   0.307  0.481
## lcp      0.692   0.157 0.173 -0.089  0.671  1.000   0.476  0.663
## gleason  0.426   0.024 0.366  0.033  0.307  0.476   1.000  0.757
## pgg45    0.483   0.074 0.276 -0.030  0.481  0.663   0.757  1.000
```

3. Reproduce the results presented Table 3.2
  a) Compute the coefficients of the linear regression model, without using the lm function
(but you can use it validate your code)


```r
Xtrainone <- cbind(array(1, dim = c(nrow(Xtrain),1)), Xtrain)
b <- solve(t(Xtrainone) %*% Xtrainone, t(Xtrainone) %*% Ytrain)
```

  b) Compute the prediction error
  

```r
Ypred <- Xtrainone %*% b
err <- Ytrain - Ypred
```

  c) Compute the standard error for each variable
  

```r
sig2 <- (t(err) %*% err)/ (nrow(Xtrainone) - ncol(X) -1)
v <- diag(solve(t(Xtrainone) %*% Xtrainone))
stderr <- sqrt(as.vector(sig2)) * sqrt(v)
```
  
  d) compute the Z score for each variable

```r
Z <- b/stderr
```

  e) visualize the results and compare with table 3.2


```r
table32 <- cbind(b,stderr,Z)
round(100*table32)/100
```

```
##               stderr      
##          2.46   0.09 27.60
## lcavol   0.68   0.13  5.37
## lweight  0.26   0.10  2.75
## age     -0.14   0.10 -1.40
## lbph     0.21   0.10  2.06
## svi      0.31   0.12  2.47
## lcp     -0.29   0.15 -1.87
## gleason -0.02   0.15 -0.15
## pgg45    0.27   0.15  1.74
```

###Ex. 2 — Your turn

Reproduce Table 3.3, at least the first four columns that is LS, Best Subset, Ridge and Lasso.


####LS column
We already have the LS column, this corresponds to our **b** vector computed earlier. We need to compute the *Test error* and *Std error* entries for the LS column. For this we apply the model to our test set and compute prediction errors. 


```r
Xtestone <- cbind(array(1, dim = c(nrow(Xtest),1)), Xtest)
Y_pred_test <- Xtestone %*% b
err_test <- (Ytest - Y_pred_test)
```


```r
table33 <- as.data.frame(c(b, mean(err_test^2), sd(err_test^2)/sqrt(length(err_test))));
colnames(table33) <- "LS"
```


```r
row.names(table33) <- c("Intercept", "lcavol", "lweight", "age", "lbph", "svi", "lcp", "gleason", "pgg45", "TEST ERROR", "STD ERROR")
round(table33 *1000)/1000
```

```
##                LS
## Intercept   2.465
## lcavol      0.680
## lweight     0.263
## age        -0.141
## lbph        0.210
## svi         0.305
## lcp        -0.288
## gleason    -0.021
## pgg45       0.267
## TEST ERROR  0.521
## STD ERROR   0.179
```


####Best Subset

To get the best subset, we need to iterate over the whole model space and fit all possible models. Some R functions such `leaps` are useful to do this. For this exercise, we will implement everything by hand and not rely on such packages.  
In order find the best model, we will used 10-fold cross-validation on the training set and estimate the average prediction error of each model. We will choose the model that minimizes the cross validation prediction error on the training set. We'll then fit the selected model on the test set and calculate the last two lines of the table - Test Error and Std Error. The cross-validation procedure is explained in detail in section 7.10 of the book *Elements of Statistical Learning*

We will build a function to fit a linear model, a function to calculate the 10-fold cross-validation error, and a function to put everything together.   
We need an efficient way to iterate over all possible models. For this we construct the following dummy matrix:


```r
dummies = expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
head(dummies)
```

```
##   Var1 Var2 Var3 Var4 Var5 Var6 Var7 Var8
## 1    0    0    0    0    0    0    0    0
## 2    1    0    0    0    0    0    0    0
## 3    0    1    0    0    0    0    0    0
## 4    1    1    0    0    0    0    0    0
## 5    0    0    1    0    0    0    0    0
## 6    1    0    1    0    0    0    0    0
```

```r
dim(dummies)
```

```
## [1] 256   8
```
Each row represents a model, a 1 in the ith column indicates that the ith variable is used in the model, while a 0 indicates this variable is absent.
We have 256 total possible models.

Let's first construct a function to fit linear models. It outputs a vector of estimated coefficients.

```r
fit_lin_reg <- function(x, y){
  #This function should be given a matrix of type Xone (with a column of ones for the intercept term)
  b <- solve(t(x) %*% x, t(x) %*% y)
  
  return(b)
}
```

Now let's construct a function which will calculate the average ten-fold cross-validation error. It can take as input any function defining a regression model like our `fit_lin_reg` function. It outputs the mean and std deviation of the average prediction error on each cross validation set.


```r
ten_fold_cv <- function(x, y, FUN){
  require(caret)
  set.seed(78)
  #Divide y and x into 10 equally sized sets.
  #The set.seed(78) guarantees that results are reproducible, and that we always have the same partitioning
  flds <- createFolds(y, k = 10, list = TRUE)
  
  #Create a vector which will take the error over each of the 10 sets
  err <- numeric(10)
  for (i in 1:10){
    #One set is the test set, the 9 remaining are train sets
    names(flds)[] <- "train"
    names(flds)[i] <- "test"
    #Fit the model over the 9 train sets
    fit <- FUN(x = x[unlist(flds[names(flds) != "test" ], use.names = FALSE), ],  
               y = y[unlist(flds[names(flds) != "test" ], use.names = FALSE)])
    
    #Predict on the test set
    pred <- x[flds$test, ] %*% fit
    #Calculate the test error
    err[i] <- mean((y[flds$test] - pred)^2)
  }
  return(list(mean_err = mean(err), std_err = sd(err))) 
}
```

Now let's build one last function which will first call our `ten_fold_cv` function to calculate the cross-validation error and second fit the model one last time over the whole set passed as input. Once again, through the input parameter `FUN`, we can pass any function defining a regression model. It outputs a list with the estimated parameters, the mean and the std deviation of the cross-validation error.


```r
fit_cv <- function(x, y, FUN){
  err <- ten_fold_cv(x, y, FUN)
  coeff <- FUN(x, y)
  return(list(coeff = coeff, mean_err = err$mean_err, std_err = err$std_err))
}
```

The way this whole system works for us is the following:
* We call `fit_cv` with an `x` matrix, a `y` matrix, and a regression function `FUN` as input
  + `fit_cv` will call `ten_fold_cv`
    * `ten_fold_cv` will call `FUN` multiple times
  + `fit_cv` calls `FUN` one last time

Now we can use these functions to find the best subset. We will iterate over all possible models using the `dummies` matrix. Let's first create some vectors and matrices which we'll use to store the output of `fit_cv` function 


```r
#Vector of mean cross-validation errors
errors <- numeric(dim(dummies)[1])
#Vector of cross-validation standard errors
std_errors <- numeric(dim(dummies)[1])
#Matrix of estimated parameters
betas <- matrix(nrow = dim(dummies)[1], ncol = 9)
```

Now iterate over the dummies matrix


```r
for (i in (1:dim(dummies)[1])){
  
  coeff = dummies[i, ]
  #Let's build an X matrix containing only the parameters we want in our model
  curX <- cbind(array(1, dim = c(nrow(Xtrain),1)), Xtrain[,which(coeff == 1)])
  
  #Fit a linear regression with cross-validation
  fit <- fit_cv(curX,Ytrain, fit_lin_reg)
  #Save the mean cv error
  errors[i] = fit$mean_err
  #Save the cv std error
  std_errors[i] = fit$std_err
  
  #Save the estimated parameters
  result <- numeric(8)
  result[which(coeff == 1)] <- fit$coeff[-1]
  result <- c(fit$coeff[1], result)
  betas[i, ] <- result
}
```

```
## Loading required package: caret
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

Now find the best model (with the lowest cross-validation error)


```r
best_err <- min(errors)
best_std <- std_errors[which.min(errors)]
print(best_err)
```

```
## [1] 0.5562241
```

In the table, they don't just take the best model, but choose the least complex model within one standard error of the best.  

Within one standard error, we have all the following models

```r
dummies[errors > best_err - .5*best_std & errors < best_err + .5*best_std,]
```

```
##     Var1 Var2 Var3 Var4 Var5 Var6 Var7 Var8
## 4      1    1    0    0    0    0    0    0
## 8      1    1    1    0    0    0    0    0
## 12     1    1    0    1    0    0    0    0
## 16     1    1    1    1    0    0    0    0
## 20     1    1    0    0    1    0    0    0
## 24     1    1    1    0    1    0    0    0
## 26     1    0    0    1    1    0    0    0
## 28     1    1    0    1    1    0    0    0
## 30     1    0    1    1    1    0    0    0
## 32     1    1    1    1    1    0    0    0
## 36     1    1    0    0    0    1    0    0
## 40     1    1    1    0    0    1    0    0
## 44     1    1    0    1    0    1    0    0
## 48     1    1    1    1    0    1    0    0
## 52     1    1    0    0    1    1    0    0
## 56     1    1    1    0    1    1    0    0
## 58     1    0    0    1    1    1    0    0
## 60     1    1    0    1    1    1    0    0
## 62     1    0    1    1    1    1    0    0
## 64     1    1    1    1    1    1    0    0
## 68     1    1    0    0    0    0    1    0
## 72     1    1    1    0    0    0    1    0
## 76     1    1    0    1    0    0    1    0
## 80     1    1    1    1    0    0    1    0
## 84     1    1    0    0    1    0    1    0
## 88     1    1    1    0    1    0    1    0
## 92     1    1    0    1    1    0    1    0
## 96     1    1    1    1    1    0    1    0
## 100    1    1    0    0    0    1    1    0
## 104    1    1    1    0    0    1    1    0
## 108    1    1    0    1    0    1    1    0
## 112    1    1    1    1    0    1    1    0
## 116    1    1    0    0    1    1    1    0
## 120    1    1    1    0    1    1    1    0
## 124    1    1    0    1    1    1    1    0
## 128    1    1    1    1    1    1    1    0
## 132    1    1    0    0    0    0    0    1
## 136    1    1    1    0    0    0    0    1
## 140    1    1    0    1    0    0    0    1
## 144    1    1    1    1    0    0    0    1
## 148    1    1    0    0    1    0    0    1
## 152    1    1    1    0    1    0    0    1
## 156    1    1    0    1    1    0    0    1
## 160    1    1    1    1    1    0    0    1
## 164    1    1    0    0    0    1    0    1
## 168    1    1    1    0    0    1    0    1
## 172    1    1    0    1    0    1    0    1
## 176    1    1    1    1    0    1    0    1
## 180    1    1    0    0    1    1    0    1
## 184    1    1    1    0    1    1    0    1
## 186    1    0    0    1    1    1    0    1
## 188    1    1    0    1    1    1    0    1
## 190    1    0    1    1    1    1    0    1
## 192    1    1    1    1    1    1    0    1
## 196    1    1    0    0    0    0    1    1
## 200    1    1    1    0    0    0    1    1
## 204    1    1    0    1    0    0    1    1
## 208    1    1    1    1    0    0    1    1
## 212    1    1    0    0    1    0    1    1
## 216    1    1    1    0    1    0    1    1
## 220    1    1    0    1    1    0    1    1
## 224    1    1    1    1    1    0    1    1
## 228    1    1    0    0    0    1    1    1
## 232    1    1    1    0    0    1    1    1
## 236    1    1    0    1    0    1    1    1
## 240    1    1    1    1    0    1    1    1
## 244    1    1    0    0    1    1    1    1
## 248    1    1    1    0    1    1    1    1
## 252    1    1    0    1    1    1    1    1
## 256    1    1    1    1    1    1    1    1
```
We can see how many variables each model contains by summing the rows


```r
rowSums(dummies[errors > best_err - .5*best_std & errors < best_err + .5*best_std,])
```

```
##   4   8  12  16  20  24  26  28  30  32  36  40  44  48  52  56  58  60 
##   2   3   3   4   3   4   3   4   4   5   3   4   4   5   4   5   4   5 
##  62  64  68  72  76  80  84  88  92  96 100 104 108 112 116 120 124 128 
##   5   6   3   4   4   5   4   5   5   6   4   5   5   6   5   6   6   7 
## 132 136 140 144 148 152 156 160 164 168 172 176 180 184 186 188 190 192 
##   3   4   4   5   4   5   5   6   4   5   5   6   5   6   5   6   6   7 
## 196 200 204 208 212 216 220 224 228 232 236 240 244 248 252 256 
##   4   5   5   6   5   6   6   7   5   6   6   7   6   7   7   8
```

And use this to choose the simplest model (the one with the lowest number of variables)


```r
dummies[errors > best_err - .5*best_std & errors < best_err + .5*best_std,][which.min(rowSums(dummies[errors > best_err - .5*best_std & errors < best_err + .5*best_std,])),]
```

```
##   Var1 Var2 Var3 Var4 Var5 Var6 Var7 Var8
## 4    1    1    0    0    0    0    0    0
```

So the simplest model is: Y ~ lcavol + lweigth 
Let's build the column for the output matrix


```r
bestXtrain = Xtrainone[,c(1:3)]
beta <- fit_lin_reg(bestXtrain, Ytrain)

bestXtestone <- cbind(array(1, dim = c(nrow(Xtest),1)), Xtest[,c(1,2)])

Y_pred_test <- bestXtestone %*% beta
err_test <- (Ytest - Y_pred_test)
```


```r
table33$Best_Subset <- c(beta, numeric(6), mean(err_test^2), sd(err_test^2)/sqrt(length(err_test)));
round(table33 *1000)/1000
```

```
##                LS Best_Subset
## Intercept   2.465       2.477
## lcavol      0.680       0.740
## lweight     0.263       0.316
## age        -0.141       0.000
## lbph        0.210       0.000
## svi         0.305       0.000
## lcp        -0.288       0.000
## gleason    -0.021       0.000
## pgg45       0.267       0.000
## TEST ERROR  0.521       0.492
## STD ERROR   0.179       0.143
```



####Ridge Regression

For ridge regression, we will also use cross-validation on the training set to choose the ideal value for the parameter `lambda`. We will be able to reuse our `ten_fold_cv` and `fit_cv` functions, all we need is a new function to fit a ridge regression model. This function will be used as the `FUN` input for `fit_cv` We define this function now:

The only tricky part is that `ten-fold_cv` and `fit_cv` expect as X input a matrix with a leading column of ones. To fit ridge regression, we need to scale the matrices and delete the column of ones as ridge fits a no-intercept model.


```r
fit_ridge <- function(x, y, lambda){
  ys <- scale(y)
  xs <- scale(x[,-1])
  
  b_ridge <- solve(t(xs) %*% xs + (lambda * diag(dim(xs)[2])), t(xs) %*% ys)
  d <- sqrt(diag(var(x[,-1])))
  b_ridge <- b_ridge * sqrt(var(y)) / d
  
  return(c(mean(y), b_ridge))
}
```

Now we are ready to fit ridge regression models. To find the best lambda we do something similar as for best subset. We iterate over a vector of possible lambdas and fit each model, and find the lambda which minimized cross-validation error.  
As before, first initialize some empty vectors / matrices to store results. And initialize the lambda vector (from 0 to 100)


```r
lambdas <- (0:100)
errors <- numeric(length(lambdas))
std_errors <- numeric(length(lambdas))
betas <- matrix(nrow = length(lambdas), ncol = 9)
```

Iterate over `lambdas` and fit the models.


```r
for (i in 1:length(lambdas)){
  fit <- fit_cv(Xtrainone, Ytrain, function(x,y) fit_ridge(x=x, y=y, lambda = lambdas[i]))
  
  errors[i] <- fit$mean_err
  std_errors[i] <- fit$std_err
  betas[i,] <- fit$coeff
}
```

Find the 'best' lambda


```r
best_err <- min(errors)
best_std <- std_errors[which.min(errors)]
best_lambda <- lambdas[which.min(errors)]

print(best_lambda)
```

```
## [1] 3
```

So with lambda = 3, we minimize the cv error. However, in the table they want the lambda which gives the simplest model within one standard error of the best model. This means we want lambda to be as large as possible. All the followng lambdas give models within one standard error.


```r
lambdas[which(errors > best_err - .5*best_std & errors < best_err + .5*best_std)]
```

```
##  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
## [24] 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45
## [47] 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
## [70] 69 70 71 72 73 74 75 76 77 78 79 80 81 82
```

And so what we want is lambda = 82. However this model does not give the same estimates as the ridge model in the table.


```r
#First model is lambda = 0
#83'rd model is lambda = 82
betas[82,]
```

```
## [1] 2.45234509 0.25251382 0.16191314 0.01155548 0.11155403 0.16685578
## [7] 0.08085127 0.05559250 0.09634001
```

This is because in the book, they do not use lambda as the parameter to optimize. Instead they use the *effective degrees of freedom* defined in equation 3.50 by:
$$df(\lambda) = \sum_{j=1}^{p} \frac{d_j^2}{d_j^2 + \lambda}$$
They optimize over discrete values of $df(\lambda)$ and find the best ridge model to be the one with $df(\lambda)=5$. An decrease of 1 in $df(\lambda)$ corresponds to a large increase of lambda. We suppose that the solution we find of lambda = 89 is situated between $df(\lambda) = 5$ and $df(\lambda) = 4$

Anyways, since we want to reproduce the table, we can use lambda = 22 which corresponds more or less to $df(\lambda) = 5$. We will use this even though it is not the "best" lambda we can come up with through cross-validation (that was lambda = 89).


```r
#This corresponds to lambda = 22
beta_ridge <- betas[23,] 

Y_pred_test <- Xtestone %*% beta_ridge
err_test <- (Ytest - Y_pred_test)
```


```r
table33$Ridge <- c(beta_ridge, mean(err_test^2), sd(err_test^2)/sqrt(length(err_test)));
round(table33 *1000)/1000
```

```
##                LS Best_Subset  Ridge
## Intercept   2.465       2.477  2.452
## lcavol      0.680       0.740  0.418
## lweight     0.263       0.316  0.229
## age        -0.141       0.000 -0.048
## lbph        0.210       0.000  0.169
## svi         0.305       0.000  0.234
## lcp        -0.288       0.000  0.000
## gleason    -0.021       0.000  0.042
## pgg45       0.267       0.000  0.132
## TEST ERROR  0.521       0.492  0.493
## STD ERROR   0.179       0.143  0.163
```



####Lasso

For Lasso regression, we need to find the optimal value for the parameter t. We will proceed in a very similar fashion as for Ridge regression.

Once again, we will use `ten_fold_cv` and `fit_cv`, but we also need to specify a function to fit the Lasso regression (which will serve as `FUN` input to `fit_cv`). This time we use the CVXR package as computing the lasso solution is a quadratic programming problem.


```r
library(CVXR)
```

```
## 
## Attaching package: 'CVXR'
```

```
## The following object is masked from 'package:stats':
## 
##     power
```

```r
fit_lasso <- function(x, y, t){
  ys = scale(y)
  xs = scale(x[,-1])
  betaHat <- Variable(dim(xs)[2])
  objective <- Minimize(sum((ys - xs %*% betaHat)^2))
  constraint <- list(sum(abs(betaHat)) <=  t)
  problem <- Problem(objective, constraint)
  b_lasso <- solve(problem)
  
  #Scale back the solution
  d <- sqrt(diag(var(Xtrain)))
  bl <- b_lasso$getValue(betaHat)*sqrt(var(Ytrain)) / d
  return(c(mean(y), bl))
}
```

The following chunks are the exact same procedure as what we did for Ridge regression, adapted to the Lasso problem. 

This time we will be iterating over a vector of 100 possible values for t, ranging from 0 to 3. We also initialize empty vectors/ matrices to store results.


```r
ts <- seq(from = 0, to = 3, length.out = 100)
errors <- numeric(length(ts))
std_errors <- numeric(length(ts))
betas <- matrix(nrow = length(ts), ncol = 9)
```

Iterate over `ts`, the vector of t values.
Attention: this takes about two minutes to compute, as we are doing ten-fold cross validation for all 100 possible values of t.


```r
for (i in 1:length(ts)){
  #Uncomment the following line to track progress of the loop
  #print(paste("Iteration", i))
  fit <- fit_cv(Xtrainone, Ytrain, function(x,y) fit_lasso(x=x, y=y, t = ts[i]))
  
  errors[i] <- fit$mean_err
  std_errors[i] <- fit$std_err
  betas[i,] <- fit$coeff
}
```

Find the *best* t.


```r
best_err <- min(errors)
best_std <- std_errors[which.min(errors)]
best_t <- ts[which.min(errors)]

print(best_t)
```

```
## [1] 1.666667
```

Now find all t's for which the corresponding model is within one standard error of the best model (t=1.666667)


```r
ts[which(errors > best_err - .5*best_std & errors < best_err + .5*best_std)]
```

```
##  [1] 0.6969697 0.7272727 0.7575758 0.7878788 0.8181818 0.8484848 0.8787879
##  [8] 0.9090909 0.9393939 0.9696970 1.0000000 1.0303030 1.0606061 1.0909091
## [15] 1.1212121 1.1515152 1.1818182 1.2121212 1.2424242 1.2727273 1.3030303
## [22] 1.3333333 1.3636364 1.3939394 1.4242424 1.4545455 1.4848485 1.5151515
## [29] 1.5454545 1.5757576 1.6060606 1.6363636 1.6666667 1.6969697 1.7272727
## [36] 1.7575758 1.7878788 1.8181818 1.8484848 1.8787879 1.9090909 1.9393939
## [43] 1.9696970 2.0000000 2.0303030 2.0606061 2.0909091 2.1212121 2.1515152
## [50] 2.1818182 2.2121212 2.2424242 2.2727273 2.3030303 2.3333333 2.3636364
## [57] 2.3939394 2.4242424 2.4545455 2.4848485 2.5151515 2.5454545 2.5757576
## [64] 2.6060606 2.6363636 2.6666667 2.6969697 2.7272727 2.7575758 2.7878788
## [71] 2.8181818 2.8484848 2.8787879 2.9090909 2.9393939 2.9696970 3.0000000
```

Choose the least complex model. This correpsonds to the model with the lowest t, i.e. t = 6969...


```r
ideal_t <- ts[which(errors > best_err - .5*best_std & errors < best_err + .5*best_std)][1]
beta_lasso <- betas[which(ts == ideal_t),]
round(1000 * beta_lasso)/1000
```

```
## [1] 2.452 0.532 0.169 0.000 0.000 0.092 0.000 0.000 0.000
```

This is almost the same solution as the one in the book, except for the intercept which is slightly different for the table in the book. However, the intercept in the book does not make sense as the intercept for a LASSO model should be the mean of the observed responses. So our model with the intercept of 2.452 makes more sense. We prefer our model. We can now show the full table


```r
Y_pred_test <- Xtestone %*% beta_lasso
err_test <- (Ytest - Y_pred_test)
```

Let's print the table. You need to navigate to the second page to see the last line with the std error.


```r
table33$Lasso <- c(beta_lasso, mean(err_test^2), sd(err_test^2)/sqrt(length(err_test)));
round(table33 *1000)/1000
```

```
##                LS Best_Subset  Ridge Lasso
## Intercept   2.465       2.477  2.452 2.452
## lcavol      0.680       0.740  0.418 0.532
## lweight     0.263       0.316  0.229 0.169
## age        -0.141       0.000 -0.048 0.000
## lbph        0.210       0.000  0.169 0.000
## svi         0.305       0.000  0.234 0.092
## lcp        -0.288       0.000  0.000 0.000
## gleason    -0.021       0.000  0.042 0.000
## pgg45       0.267       0.000  0.132 0.000
## TEST ERROR  0.521       0.492  0.493 0.481
## STD ERROR   0.179       0.143  0.163 0.166
```



