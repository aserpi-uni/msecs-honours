# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi - 1647244
# Created on: 2020-07-01

library("tidyverse")
library("ggfortify")
library("autoplotly")

source("dim_red_utils.R")
source("mixed_datasets.R")

data_name <- "gironde"
n_dims <- Inf


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

  positive_eig <- result$sdev[which(result$sdev > sqrt(.Machine$double.eps))]
  eig <- matrix(positive_eig, dimnames = list(seq_len(length(positive_eig)), "Eigenvalue"))

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

pcoa_test <- function (data) {
  result <- pcoa_wrapper(data)

  positive_eig <- result$values$Eigenvalues[which(result$values$Eigenvalues > sqrt(.Machine$double.eps))]
  eig <- matrix(positive_eig, dimnames = list(seq_len(length(positive_eig)), "Eigenvalue"))

  total_variance <- sum(eig)
  eig <- cbind(eig, Proportion = eig[, "Eigenvalue"] * 100 / total_variance)
  eig <- cbind(eig, Cumulative = cumsum(eig[, "Proportion"]))

  return(eig)
}


data <- do.call(data_name, list())

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

pcoa_eig <- pcoa_test(data = data)
print("PCoA with Gower's distance")
print(pcoa_eig)

ADE4 <- ade4_eig[, "Proportion"]
FAMD <- famd_eig[, "Proportion"]
PCAmix <- pcamix_eig[, "Proportion"]
PCA_1hot <- pca_one_hot_eig[, "Proportion"]
PCoA <- pcoa_eig[, "Proportion"]

max_dim <- max(length(FAMD), length(PCAmix), length(PCA_1hot))
length(ADE4) <- max_dim
length(FAMD) <- max_dim
length(PCAmix) <- max_dim
length(PCA_1hot) <- max_dim
length(PCoA) <- max_dim
Eigenvalues <- 1:max_dim

p <- ggplot(mapping = aes(x = Eigenvalues)) +
  geom_line(mapping = aes(y = FAMD / 100), color = "darkred") +
  geom_line(mapping = aes(y = PCAmix / 100), color = "steelblue") +
  geom_line(mapping = aes(y = PCA_1hot / 100), color = "forestgreen") +
  geom_line(mapping = aes(y = PCoA / 100), color = "yellow") +
  labs(y = "Explained variance", title = "Percentage of variance explained by each eigenvalue") +
  scale_y_continuous(labels = scales::percent)
autoplotly::autoplotly(p)

write_result(cbind(Eigenvalues, ADE4, FAMD, PCAmix, PCA_1hot, PCoA), data_name, "eig")
