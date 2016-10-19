############################################################# 
## Stat 202A - Homework 3
## Author: Xin Shen 1993
## Date : 2016/10/19
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
## Very important: Do not change the working directory anywhere
## in your code. If you do, I will be unable to grade your 
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################

import numpy as np

#####################################
## Function 1: Lasso solution path ##
#####################################

def myLasso(X, Y, lambda_all):
  
  # Find the lasso solution path for various values of 
  # the regularization parameter lambda.
  # 
  # X: Array of explanatory variables.
  # Y: Response array
  # lambda_all: Array of regularization parameters. Make sure 
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns an array containing the lasso solution  
  # beta for each regularization parameter.
    A = np.copy(X) #for python
    B = np.copy(Y)
    p = np.array(X).shape[1] #p should be the number of colums of matrix X
    L = np.array(lambda_all).size #L should be the number of lambda
    lambda_all_sorted = np.sort(lambda_all)[::-1] #sort lambda_all in decreasing order
    T = 10
    
    beta = np.zeros(p) #initrial all the beta to be zero
    beta_all = np.zeros((p, L)) #L is the number of lambda, every column is a specific beta under lambda value

    R = np.copy(Y) #beta is all zeros at beginning, so initial the R to be Y 
    
    ss = np.zeros(p)
    for j in range(p):
        ss[j] = np.sum(pow(X[:,j], 2))
    
    for l in range(L):
        lam = lambda_all_sorted[l]
        for t in range(T):
            for j in range(p):
                db = np.sum(R.dot(X[:,j])) / ss[j] #here R is Y - sum(X_k*beta_k) k include all from 1 to p 
                b = beta[j] + db # we do not want to include minus X_j*beta_j in b, so we make it up
                b = np.sign(b) * np.maximum(0, np.abs(b) - lam / ss[j])
                db = b - beta[j] #here is the change of b after updated
                R = R - X[:,j] * db #there is some change in R becuase of change of beta[j], so make it up
                beta[j] = b #store the updated beta[j]
        beta_all[:,l] = beta #after 10 times of iteration, we store the final result in l corresponding lth lambda

  ## Function should output the array beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is p x length(lambda_all)
    return(beta_all)

# import sklearn.datasets
# X = sklearn.datasets.load_boston().data
# Y = sklearn.datasets.load_boston().target
# myLasso(X, Y, [10, 100, 1000, 10000, 100000, 1000000])

