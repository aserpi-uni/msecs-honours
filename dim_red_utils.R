ade4_wrapper <- function (data, ndims) {
  ade4::dudi.hillsmith(
    df = data,
    row.w = rep(1, nrow(data))/nrow(data),
    scannf = FALSE,
    nf = ndims
  )
}

famd_wrapper <- function (data, ndims) {
  FactoMineR::FAMD(
    base = data,
    ncp = ndims,
    graph = FALSE,
    sup.var = NULL,
    ind.sup = NULL,
    row.w = NULL,
    tab.disj = NULL
  )
}

pca_one_hot_pre <- function (data) {
  mltools::one_hot(data.table::data.table(data))
}
pca_one_hot_wrapper <- function (preprocessed_data) {
  prcomp(preprocessed_data)
}

pcamix_pre <- function (data) {
  PCAmixdata::splitmix(data)
}
pcamix_wrapper <- function (preprocessed_data, ndims) {
  PCAmixdata::PCAmix(
    X.quanti = preprocessed_data$X.quanti,
    X.quali = preprocessed_data$X.quali,
    ndim = ndims,
    rename.level = TRUE,
    weight.col.quanti = NULL,
    weight.col.quali = NULL,
    graph = FALSE
  )
}
