/*
####################################################
## Stat 202A - Homework 6
## Author: Xin Shen
## Date : 2016/11/10
## Description: This script implements sweep and QR
## operations in Rcpp
####################################################
 
###########################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not change your working directory
## anywhere inside of your code. If you do, I will be unable 
## to grade your work since R will attempt to change my 
## working directory to one that does not exist.
###########################################################
 
 */ 

# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
Sign function for later use 
~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
Problem 1: Sweep operator 
~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
NumericMatrix mySweepC(const NumericMatrix B, int m){
  
  // See R code from previous assignment for description
  // of inputs and outputs. Note the "const" in front 
  // of NumericMatrix B; this is so you don't accidentally
  // change B inside your code.
  
  /* Fill in code below */
  NumericMatrix A = clone(B);
  int n = A.nrow();
  
  for (int k = 0; k < m; k++) {
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if ((i != k) & (j != k)) {
          A(i, j) = A(i, j) - A(i, k) * A(k, j) / A(k,k);
        }
      }
    }
    for (int i = 0; i < n; i++) {
      if (i !=k ) {
        A(i, k) = A(i, k) / A(k,k);
      }
    }
    for (int j = 0; j < n; j++) {
      if (j != k) {
        A(k, j) = A(k, j) / A(k, k); 
      }
    }
    A(k, k) = -1 / A(k, k);
  }  
  // Return swept matrix A
  return(A);
  
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Problem 2: QR decomposition 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */  

// Now let's use Armadillo  

// [[Rcpp::export()]]
List myQRC(const mat A){ 
  
  // A is the input matrix with dimension n x m.
  
  /* Fill in code below */
  mat B = A;
  int n = B.n_rows;
  int m = B.n_cols;
  mat Q;
  Q.eye(n,n);
  
  for (int k = 0; k < m - 1; k++) {
    vec x(n);
    for (int i = k; i < n; i++) {
      x(i) = B(i, k);
    }
    vec v = x;
    v(k) = x(k) + signC(x(k)) * norm(x,2);
    double s = norm(v,2);
    if (s!=0) {
      vec u = v / s;
      B = B - 2 * u * u.t() * B;
      Q = Q - 2 * u * u.t() * Q;
    }
  }  
  List output;
  // Return a list with two named objects, Q and R
  output["Q"] = Q.t();
  output["R"] = B;
  return(output);
  
}
