
# coding: utf-8

# In[279]:

############################################################# 
## Stat 202A - Homework 5
## Author: 
## Date : 
## Description: This script implements factor analysis and 
## matrix completion
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


def mySweep(A, m):
    """
    Perform a SWEEP operation on A with the pivot element A[m,m].
    
    :param A: a square matrix.
    :param m: the pivot element is A[m, m].
    :returns a swept matrix. Original matrix is unchanged.
    """
    
    ## No need to change anything here
    B = np.copy(A)   
    n = B.shape[0]
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i!=k and j!=k:
                    B[i,j] = B[i,j] - B[i,k]*B[k,j] / B[k,k]
        for i in range(n):
            if i!=k:
                 B[i,k] = B[i,k] / B[k,k]
        for j in range(n):
            if j!=k:
                B[k,j] = B[k,j] / B[k,k]
        B[k,k] = -1/B[k,k]
    
    return(B)

    
def factorAnalysis(n = 10, p = 5, d = 2, sigma = 1, nIter = 1000):
   
    """
    Perform factor analysis on simulated data.
    Simulate data X from the factor analysis model, e.g. 
    X = Z_true * W.T + epsilon
    where W_true is p * d loading matrix (numpy array), Z_true is a n * d matrix 
    (numpy array) of latent factors (assumed normal(0, I)), and epsilon is iid 
    normal(0, sigma^2) noise. You can assume that W_true is normal(0, I)
    
    :param n: Sample size.
    :param p: Number of variables
    :param d: Number of latent factors
    :param sigma: Standard deviation of noise
    :param nIter: Number of iterations
    """

    ## FILL CODE HERE
    
    
# X = np.random.normal(0, 1, 10000)
# X = s.reshape(1000,10)
# beta_true = np.array([1,2,3,4,5,6,7,8,9,10])
# Y = np.dot(X,beta_true)
# swRegression(X, 

    W_true = np.random.normal(0, 1, p * d)
    W_true = W_true.reshape(p, d)
    Z_true = np.random.normal(0, 1, d * n)
    Z_true = Z_true.reshape(d, n)
    epsilon = np.random.normal(0 , 1, p * n) * sigma #TODO: right or not
    epsilon = epsilon.reshape(p, n)
    X = np.dot(W_true, Z_true) + epsilon
    
    sq = 1;
    XX = np.dot(X, X.T)
    W = np.random.normal(0, 1, p * d) * 0.1 #TODO: right or not
    W= W.reshape(p, d)
    
    for it in range(nIter):
        A = np.concatenate((np.hstack((np.dot(W.T, W)/sq + np.eye(d), W.T/sq)), np.hstack((W/sq, np.eye(p)))))  
        AS = mySweep(A, d)
        alpha = AS[:d, d : d + p]
        D = -AS[:d, :d]
        Zh = np.dot(alpha, X)
        ZZ = np.dot(Zh, Zh.T) + D*n
        
        B = np.concatenate((np.hstack((ZZ, np.dot(Zh, X.T))), np.hstack((np.dot(X, Zh.T), XX))))
        
        BS = mySweep(B, d)
        W = BS[:d, d : d+p].T
        sq = np.mean(np.diag(BS[d : d + p, d : d + p]) / n)
        
    print(W)
     
    
    
    
    
    
    
    
    ## Return the p * d np.array w, the estimate of the loading matrix
#     return(w)
    
    
def matrixCompletion(n = 200, p = 100, d = 3, sigma = 0.1, nIter = 100,
                     prob = 0.2, lam = 0.1):
   
    """
    Perform matrix completion on simulated data.
    Simulate data X from the factor analysis model, e.g. 
    X = Z_true * W.T + epsilon
    where W_true is p * d loading matrix (numpy array), Z_true is a n * d matrix 
    (numpy array) of latent factors (assumed normal(0, I)), and epsilon is iid 
    normal(0, sigma^2) noise. You can assume that W_true is normal(0, I)
    
    :param n: Sample size.
    :param p: Number of variables
    :param d: Number of latent factors
    :param sigma: Standard deviation of noise
    :param nIter: Number of iterations
    :param prob: Probability that an entry of the matrix X is not missing
    :param lam: Regularization parameter
    """

    ## FILL CODE HERE

    W_true = np.random.normal(0, 1, p * d)
    W_true = W_true.reshape(p, d)
    Z_true = np.random.normal(0, 1, d * n)
    Z_true = Z_true.reshape(d, n)
    epsilon = np.random.normal(0, 1, n * p) * sigma
    epsilon = epsilon.reshape(p, n)
    X = np.dot(W_true, Z_true) + epsilon
    
    R = np.random.uniform(low=0.0, high=1.0, size=p * n) < prob
    R = R.reshape(p, n)
    W = np.random.normal(0, 1, p * d) * 0.1
    W = W.reshape(p, d)
    Z = np.random.normal(0, 1, d * n) * 0.1
    Z = Z.reshape(d, n)
    
    for it in range(nIter):
        for i in range(n):
            WW = np.dot(np.dot(W.T, np.diag(R[:,i])), W) + lam * np.eye(d)
            WX = np.dot(np.dot(W.T, np.diag(R[:,i])), np.matrix(X[:, i]).T)
            A = np.concatenate((np.hstack((WW, WX)), np.hstack((WX.T, np.zeros((WX.T.shape[0], WX.shape[1]))))))
            AS = mySweep(A, d)
            Z[:, i] = AS[:d, d]
        for j in range(p):
            ZZ = np.dot(np.dot(Z, np.diag(R[j,:])), Z.T) + lam * np.eye(d)
            ZX = np.dot(np.dot(Z, np.diag(R[j,:])), np.matrix(X[j, :]).T)
            B = np.concatenate((np.hstack((ZZ, ZX)), np.hstack((ZX.T, np.zeros((ZX.T.shape[0], ZX.shape[1]))))))
            BS = mySweep(B, d)
            W[j, :] = BS[:d, d]
      
    ## Return estimates of Z and W (both numpy arrays)
    return Z.T, W  
    
    
    

###########################################################
### Optional examples (comment out before submitting!!) ###
###########################################################
    


# In[ ]:



