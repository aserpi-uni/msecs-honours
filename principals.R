# Title     : PRINCIPALS
# Objective : Implementation of PRINCIPALS (Young et al., 1978)
# Created by: Alessandro Serpi
# Created on: 18/07/2020

standardise <- function (X) {
  X_star <- scale(X, center = T, scale = F)
  X_star <- scale(X_star, center = F, scale = apply(X_star, 2, function (x) { sqrt(sum(x^2) / nrow(X_star)) }))

  stopifnot(abs(t(X_star) %*% rep(1, nrow(X_star))) <= 1e-15)
  stopifnot(abs(diag((t(X_star) %*% X_star) / nrow(X)) - 1) <= 1e-15)

  return(X_star)
}


principals <- function (X, r = Inf) {
  # TODO: encode categorical and ordinal features

  X_star <- standardise(X)

  # Model parameter estimation
  eigens <- eigen((t(X_star) %*% X_star) / nrow(X_star), only.values = F)
  A <- head(eigens$vectors, cbind(nrow(eigens$vectors), min(r, ncol(eigens$vectors))))
  D <- head(eigens$values, min(r, length(eigens$values)))
  Z <- X_star %*% A

  # TODO
}
