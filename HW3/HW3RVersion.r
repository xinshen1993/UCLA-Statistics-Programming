
############################################################# 
## Stat 202A - Homework 3
## Author: Xin Shen
## Date : 2016/10/18
## Description: This script implements the lasso
#############################################################

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

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){
  
  # Find the lasso solution path for various values of 
  # the regularization parameter lambda.
  # 
  # X: Matrix of explanatory variables.
  # Y: Response vector
  # lambda_all: Vector of regularization parameters. Make sure 
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector 
  # beta for each regularization parameter.
    p = dim(X)[2] #p should be the number of columns of the matrix X
    L = length(lambda_all) #L should be the number of lambda
    lambda_all = sort(lambda_all, decreasing = TRUE) #sort lambda_all in decreasing order for efficiency.
    T = 10 # the number of iterations for each lambda

    beta = matrix(rep(0, p), nrow = p) #initrial all the beta to be zero
    beta_all = matrix(rep(0, p * L), nrow = p) #L is the number of lambda, every column is a specific beta under lambda value
    
    R = Y #beta is all zeros at beginning, so initial the R to be Y 
  
    ss = rep(0, p) #ss[j] stores sum(X[, j]^2)
    for (j in 1 : p) {
        ss[j] = sum(X[,j] ^ 2)
    } 
    
    for (l in 1 : L) {
        lambda = lambda_all[l]
        for (t in 1 : T) {
            for (j in 1 : p) {
                db = sum(R * X[,j]) / ss[j] #here R is Y - sum(X_k*beta_k) k include all from 1 to p 
                b = beta[j] + db # we do not want to include minus X_j*beta_j in b, so we make it up
                b = sign(b) * max(0, abs(b) - lambda / ss[j])
                db = b - beta[j] #here is the change of b after updated
                R = R - X[, j] * db #there is some change in R becuase of change of beta[j], so make it up
                beta[j] = b #store the updated beta[j]               
            }
        }
        beta_all[, l] = beta #after 10 times of iteration, we store the final result in l corresponding lth lambda
    }
  
  
  ## Function should output the matrix beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is p x length(lambda_all)
  return(beta_all)
  
}
