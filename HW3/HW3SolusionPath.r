
library(Boston, package = "MASS")
X = Boston[, 1 : 13]
Y = Boston[, 14]
beta_all = myLasso(X, Y, c(1000000, 100000, 10000, 1000, 100, 10))
matplot(t(t(matrix(rep(1, 13), nrow = 13)) %*% abs(beta_all)), t(beta_all), type ="l", xlab = "|beta|", ylab = "beta", main = "solution path of lasso")
legend(0, 10, legend = colnames(Boston)[1:13], col = 1:13, lty = 1:13)
