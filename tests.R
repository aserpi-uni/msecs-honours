# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi - 1647244
# Created on: 2020-07-01

ade4_test <- function (data, ndims) {
  result <- ade4::dudi.hillsmith(
    df = data,
    row.w = rep(1, nrow(data))/nrow(data),
    scannf = FALSE,
    nf = ndims
  )

  Eigenvalue <- result$eig
  eig <- data.frame(Eigenvalue)

  total_variance <- sum(result$eig)
  eig$Proportion <- eig$Eigenvalue * 100 / total_variance
  eig$Cumulative <- cumsum(eig$Proportion)

  return(eig)
}


caravan <- ISLR::Caravan
caravan$MOSTYPE <- factor(caravan$MOSTYPE)
caravan$MOSHOOFD <- factor(caravan$MOSHOOFD)
caravan$MGODRK <- factor(caravan$MGODRK)
caravan$PWAPART <- factor(caravan$PWAPART)

wine <- read.csv("datasets/winequality-red.csv", sep=";")
wine$quality <- factor(wine$quality, levels = 1:10)

data <- caravan
n_dims <- Inf

ade4_eig <- ade4_test(data = data, ndims = n_dims)
print("ade4")
print(ade4_eig)
