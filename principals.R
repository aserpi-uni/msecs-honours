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

  stopifnot(abs(t(A) %*% A - diag(1, nrow = ncol(A), ncol = ncol(A))) <= 1e-15)
  stopifnot(abs(((t(X_star) %*% X_star) / nrow(X_star)) %*% A - A %*% diag(D)) <= 1e-15)

  # Optimal scaling
  X_hat <- Z %*% t(A)
  X_new_res <- optim(
    matrix(rep(1, nrow(X_hat) * ncol(X_hat)), nrow = nrow(X_hat), ncol = ncol(X_hat)),
    function (X_new) { sum(diag(t(X_new - X_hat)) %*% (X_new - X_hat)) },
    method = "BFGS"
  )
  if (X_new_res$convergence != 0) {
    return(NULL)
  }

  return(list("X_star" = standardise(X_new_res$par), "eig" = D))
}
