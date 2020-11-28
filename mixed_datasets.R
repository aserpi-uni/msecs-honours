is.notfactor <- function (x) { ! is.factor(x) }

scale_data <- function (data) {
  num_col <- unlist(lapply(data, is.notfactor))
  data[num_col] <- unlist(lapply(data[num_col], scale))
  data
}


caravan <- function (ordinal = FALSE) {
  caravan <- ISLR::Caravan

  caravan$MOSTYPE <- factor(caravan$MOSTYPE)
  caravan$MOSHOOFD <- factor(caravan$MOSHOOFD)
  caravan$Purchase <- factor(caravan$Purchase)

  caravan$MGEMLEEF <- factor(caravan$MGEMLEEF, levels = 1:6, ordered = ordinal)
  caravan[6:64] <- lapply(caravan[6:64], factor, levels = 0:9, ordered = ordinal)

  scale_data(caravan)
}

gironde <- function () {
  data("gironde", package = "PCAmixdata")
  gironde <- cbind(
    gironde$employment, gironde$environment, gironde$housing, gironde$services
  )

  scale_data(gironde[complete.cases(gironde), ])
}

wine <- function (type, ordinal = FALSE) {
  wine <- read.csv(stringr::str_interp("data/winequality-${type}.csv"), sep=";")
  wine$quality <- factor(wine$quality, ordered = ordinal)

  scale_data(wine)
}

wine.red <- function (ordinal = FALSE) {
  wine("red", ordinal)
}

wine.white <- function (ordinal = FALSE) {
  wine("white", ordinal)
}
