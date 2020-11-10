ssascontr <- function(Y, L) {
  # Inputs
  #   Y - input data series columns vector
  #   L - number of reconstructed components input data series to be decomposed to
  #       (note that L must be less than the length of the input series Y)
  # Outputs:
  #   D - components' contributions to the input series in %
  
  # install.packages('pracma')
  library(pracma)
  
  T <- length(Y)
  
  K <- T - L + 1
  
  X <- matrix(0, nrow = L, ncol = K)
  for (k in seq(K)) {
    X[,k] <- Y[k:(k+L-1)]
  }
  
  D <- eig(X %*% t(X))
  D <- D/sum(D)
  
  return(D)
}