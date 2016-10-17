
############################################################# 
## Stat 202A - Homework 2
## Author: Xin Shen
## Date : 2016/10/12
## Description: This script implements relationship between 
## regularization, test error and train error
#############################################################

mySweep <- function(A, m){
  n <- nrow(A) 
  for(k in 1:m){ 
    for(i in 1:n)     
      for(j in 1:n)   
        if(i != k  & j != k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
        
        for(i in 1:n) 
          if(i != k) 
            A[i,k] <- A[i,k]/A[k,k]  
          
          for(j in 1:n) 
            if(j != k) 
              A[k,j] <- A[k,j]/A[k,k]         
            A[k,k] <- - 1/A[k,k]
  }
  return(A) 
}

myRidge <- function(X, Y, lambda){
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  
  A = t(Z) %*% Z
  D = diag(rep(lambda, p + 2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  
  A = A + D
  
  S = mySweep(A, p + 1)
  beta_ridge = S[1:(p+1), p+2]
    
  return(beta_ridge)
}

mySpline <- function(x, Y, lambda, p = 100){
  X = matrix(x)
  for (k in (1 / p: (p - 1) / p)) {
      X = cbind(X, (X > k) * (X - k))
  }
  beta_spline = myRidge(X, Y, lambda)  
  y.hat = cbind(rep(1, n), X) %*% beta_spline
  output <- list(beta_spline = beta_spline, predicted_y = y.hat)
  return(output)
}

## used to caculate MSE
MSE <- function(yhat, yreal) {
    return(sum((yhat - yreal)^2)/dim(yhat)[1])
}

data(Boston, package="MASS")

X <- as.matrix(Boston[,1:13])
  

Y <- Boston[,14]

Xtrain <- X[1:400,]
Ytrain <- Y[1:400]
Xtest <- X[401:500,]
Ytest <- Y[401:500]

Ytrain_error_list = c()
Ytest_error_list = c()
lambda_list = c(0.001, 0.01, 0.05, 0.1, 0.5, 1, 10, 100, 1000, 100000, 1000000, 10000000)
for (lambda in lambda_list){
beta_ridge <- myRidge(Xtrain, Ytrain, lambda)
Ytrain_predict = cbind(rep(1, dim(Xtrain)[1]), Xtrain) %*% beta_ridge
Ytest_predict = cbind(rep(1, dim(Xtest)[1]), Xtest) %*% beta_ridge

Ytrain_error = MSE(Ytrain_predict, Ytrain)
Ytest_error = MSE(Ytest_predict, Ytest)
Ytrain_error_list = c(Ytrain_error_list, Ytrain_error)
Ytest_error_list = c(Ytest_error_list, Ytest_error) 
}
              

plot(lambda_list,log="x", Ytrain_error_list, type = 'l', col='red', xlab ='lamda', ylab='MSE' ,ylim=c(10,90))
lines(lambda_list,Ytest_error_list,col="green")
title(main="error vs lamda", font.main=4)
legend(0.001, 90, legend=c("training error", "test error"),col=c("red", "green"), lty=1:1 )


