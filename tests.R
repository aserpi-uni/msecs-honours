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


famd_test <- function (data, ndims) {
  result <- FactoMineR::FAMD(
    base = data,
    ncp = ndims,
    graph = FALSE,
    sup.var = NULL,
    ind.sup = NULL,
    axes = c(1,2),
    row.w = NULL,
    tab.disj = NULL
  )

  eig <- result$eig
  colnames(eig)[1:3] <- c("Eigenvalue", "Proportion", "Cumulative")
  row.names(eig) <- seq_len(nrow(eig))

  return(eig)
}


pcamix_test <- function(data, ndims) {
  split_data <- PCAmixdata::splitmix(data)

  result <- PCAmixdata::PCAmix(
    X.quanti = split_data$X.quanti,
    X.quali = split_data$X.quali,
    ndim = ndims,
    rename.level = TRUE,
    weight.col.quanti = NULL,
    weight.col.quali = NULL,
    graph = FALSE
  )

  eig <- result$eig
  row.names(eig) <- seq_len(nrow(eig))

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

famd_eig <- famd_test(data = data, ndims = n_dims)
print("FAMD")
print(famd_eig)

pcamix_eig <- pcamix_test(data = data, ndims = n_dims)
print("PCAmix")
print(pcamix_eig)

# TODO: PCA with on-hot encoding

# TODO: PCoA with gower distances