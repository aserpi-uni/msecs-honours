# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi - 1647244
# Created on: 2020-07-01

library(ggfortify)

source("dim_red_utils.R")
source("mixed_datasets.R")

data <- gironde()
data_name <- "gironde"

is.notfactor <- function (x) { ! is.factor(x) }


ade4_test <- function (data, ndims) {
  result <- ade4_wrapper(data, ndims)

  eig <- matrix(result$eig, dimnames = list(seq_len(length(result$eig)), "Eigenvalue"))

  total_variance <- sum(eig)
  eig <- cbind(eig, Proportion = eig[, "Eigenvalue"] * 100 / total_variance)
  eig <- cbind(eig, Cumulative = cumsum(eig[, "Proportion"]))

  return(eig)
}


famd_test <- function (data, ndims) {
  result <- famd_wrapper(data, ndims)

  eig <- result$eig
  colnames(eig)[1:3] <- c("Eigenvalue", "Proportion", "Cumulative")
  row.names(eig) <- seq_len(nrow(eig))

  return(eig)
}


pca_one_hot_test <- function (data) {
  result <- pca_one_hot_wrapper(pca_one_hot_pre(data))

  eig <- matrix(result$sdev, dimnames = list(seq_len(length(result$sdev)), "Eigenvalue"))

  total_variance <- sum(eig)
  eig <- cbind(eig, Proportion = eig[, "Eigenvalue"] * 100 / total_variance)
  eig <- cbind(eig, Cumulative = cumsum(eig[, "Proportion"]))

  return(eig)
}


pcamix_test <- function (data, ndims) {
  result <- pcamix_wrapper(pcamix_pre(data), ndims)

  eig <- result$eig
  row.names(eig) <- seq_len(nrow(eig))

  return(eig)
}


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

pca_one_hot_eig <- pca_one_hot_test(data = data)
print("PCA with one-hot encoding")
print(pca_one_hot_eig)

# TODO: PCoA with gower distances

ADE4 <- ade4_eig[, "Proportion"]
FAMD <- famd_eig[, "Proportion"]
PCAmix <- pcamix_eig[, "Proportion"]
PCA_1hot <- pca_one_hot_eig[, "Proportion"]

max_dim <- max(length(FAMD), length(PCAmix), length(PCA_1hot))
length(ADE4) <- max_dim
length(FAMD) <- max_dim
length(PCAmix) <- max_dim
length(PCA_1hot) <- max_dim
Eigenvalues <- 1:max_dim

p <- ggplot2::ggplot(mapping = ggplot2::aes(x = Eigenvalues)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = FAMD / 100), color = "darkred") +
  ggplot2::geom_line(mapping = ggplot2::aes(y = PCAmix / 100), color = "steelblue") +
  ggplot2::geom_line(mapping = ggplot2::aes(y = PCA_1hot / 100), color = "forestgreen") +
  ggplot2::labs(y = "Explained variance", title = "Percentage of variance explained by each eigenvalue") +
  ggplot2::scale_y_continuous(labels = scales::percent)
autoplotly::autoplotly(p)
