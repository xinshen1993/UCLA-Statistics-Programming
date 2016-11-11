
#########################################################
## Stat 202A - Homework 6
## Author: Xin Shen
## Date : 2016/11/10
## Description: This script implements QR decomposition
## and linear regression based on QR
#########################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){
  
  ## Perform QR factorization on the matrix A
  ## FILL IN CODE HERE ##
    n = dim(A)[1]
    m = dim(A)[2]
    
    Q = diag(n)
    
    for (k in 1 : (m - 1)) {
        x = matrix(rep(0, n), nrow = n)
        x[k:n] = A[k:n,k]
        v = x;
        v[k] = x[k] + sign(x[k]) * sqrt(sum(x^2))
        s = sqrt(sum(v^2))
        if(s!=0){
            u = v / s
            A = A - 2 * u %*% t(u) %*% A ## (I - 2uu)A
            Q = Q - 2 * u %*% t(u) %*% Q ## (I - 2uu)Q
        }
    }
  
  
  ## Function should output a list with Q.transpose and R
  return(list("Q" = t(Q), "R" = A))
  
}

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLM <- function(X, Y){
  
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQR inside of this function
  
  ## FILL CODE HERE ##
    p = dim(X)[2]
    Z = cbind(X, Y)
    R = myQR(Z)$R
    R1 = R[1 : p, 1 : p]
    Y1 = R[1 : p, p + 1]
    beta_ls = solve(R1,Y1)
    
  ## Function returns beta_ls, the least squares
  ## solution vector
  return(beta_ls)

}

