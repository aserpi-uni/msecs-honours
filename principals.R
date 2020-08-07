# Title     : PRINCIPALS
# Objective : Implementation of PRINCIPALS (Young et al., 1978)
# Created by: Alessandro Serpi
# Created on: 18/07/2020

standardise <- function (X) {
  X_star <- scale(X, center = T, scale = F)
  X_star <- scale(X_star, center = F, scale = apply(X_star, 2, function (x) { sqrt(sum(x^2) / nrow(X_star)) }))

  stopifnot(abs(t(X_star) %*% rep(1, nrow(X_star))) <= sqrt(.Machine$double.eps))
  stopifnot(abs(diag((t(X_star) %*% X_star) / nrow(X)) - 1) <= sqrt(.Machine$double.eps))

  return(X_star)
}


principals <- function (X, r = Inf) {
  # TODO: encode categorical and ordinal features

  X_star <- integer(nrow(X) * ncol(X))
  X_star_new <- standardise(X)
  D <- eigen((t(X_star_new) %*% X_star_new) / nrow(X_star_new), only.values = T)$values

  iterations <- 0
  while (! all(abs(X_star_new - X_star) <= sqrt(.Machine$double.eps))) {
    new_iteration <- principals_step(X_star_new, r)
    if (is.null(new_iteration)) {
      return(NULL)  # Propagate errors
    }

    D <- new_iteration$eig
    X_star <- X_star_new
    X_star_new <- new_iteration$X_star
    iterations <- iterations + 1
  }

  return(list("X_star" = X_star, "eig" = D, "iterations" = iterations))
}


principals_step <- function (X_star, r) {
  # Model parameter estimation
  eigens <- eigen((t(X_star) %*% X_star) / nrow(X_star), only.values = F)
  A <- head(eigens$vectors, cbind(nrow(eigens$vectors), min(r, ncol(eigens$vectors))))
  D <- head(eigens$values, min(r, length(eigens$values)))
  Z <- X_star %*% A

  stopifnot(abs(t(A) %*% A - diag(1, nrow = ncol(A), ncol = ncol(A))) <= sqrt(.Machine$double.eps))
  stopifnot(abs(((t(X_star) %*% X_star) / nrow(X_star)) %*% A - A %*% diag(D)) <= sqrt(.Machine$double.eps))

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
