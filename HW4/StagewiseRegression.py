
############################################################# 
## Stat 202A - Homework 4
## Author: Xin Shen
## Date : 2016/10/26
## Description: This script implements stagewise regression
## (epsilon boosting)
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change the working directory
## in your code. If you do, I will be unable to grade your 
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################

######################################
## Function 1: Stagewise regression ##
######################################

import numpy as np

def swRegression(X, Y, numIter = 3000, epsilon = 0.0001):
    p = X.shape[1]
    R = np.copy(Y)
    beta = np.zeros(p) #initrial all the beta to be zero
    db = np.zeros(p)
    beta_all = np.zeros((p, numIter)) #L is the number of lambda, every column is a specific beta under lambda value
    for t in range(numIter):
        for j in range(p):
            db[j] = np.sum(R.dot(X[:,j]))
        j = np.argmax(db)
        beta[j] = beta[j] + db[j] * epsilon
        R = R - X[:, j] * db[j] * epsilon
        beta_all[:, t] = beta
    return beta_all

# X = np.random.normal(0, 1, 10000)
# X = s.reshape(1000,10)
# beta_true = np.array([1,2,3,4,5,6,7,8,9,10])
# Y = np.dot(X,beta_true)
# swRegression(X, Y)
