# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi - 1647244
# Created on: 2020-07-01


is.notfactor <- function (x) { ! is.factor(x) }


ade4_test <- function (data, ndims) {
  result <- ade4::dudi.hillsmith(
    df = data,
    row.w = rep(1, nrow(data))/nrow(data),
    scannf = FALSE,
    nf = ndims
  )

  eig <- matrix(result$eig, dimnames = list(seq_len(length(result$eig)), "Eigenvalue"))

  total_variance <- sum(eig)
  eig <- cbind(eig, Proportion = eig[, "Eigenvalue"] * 100 / total_variance)
  eig <- cbind(eig, Cumulative = cumsum(eig[, "Proportion"]))

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


pca_one_hot_test <- function(data, ndims) {
  one_hot_data <- mltools::one_hot(data.table::data.table(data))
  result <- prcomp(one_hot_data)

  eig <- matrix(result$sdev, dimnames = list(seq_len(length(result$sdev)), "Eigenvalue"))

  total_variance <- sum(eig)
  eig <- cbind(eig, Proportion = eig[, "Eigenvalue"] * 100 / total_variance)
  eig <- cbind(eig, Cumulative = cumsum(eig[, "Proportion"]))

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
wine$quality <- factor(wine$quality)

data <- caravan
num_col <- unlist(lapply(data, is.notfactor))
data[num_col] <- unlist(lapply(data[num_col], scale))
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

pca_one_hot_eig <- pca_one_hot_test(data = data, ndims = n_dims)
print("PCA with one-hot encoding")
print(pca_one_hot_eig)

print("Are ade4 and PCAmix equivalent?")
all.equal(ade4_eig, pcamix_eig)

# TODO: PCoA with gower distances
