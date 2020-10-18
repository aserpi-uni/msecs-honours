is.notfactor <- function (x) { ! is.factor(x) }

scale_data <- function (data) {
  num_col <- unlist(lapply(data, is.notfactor))
  data[num_col] <- unlist(lapply(data[num_col], scale))
  data
}


caravan <- function () {
  caravan <- ISLR::Caravan
  caravan$MOSTYPE <- factor(caravan$MOSTYPE)
  caravan$MOSHOOFD <- factor(caravan$MOSHOOFD)
  caravan$MGODRK <- factor(caravan$MGODRK)
  caravan$PWAPART <- factor(caravan$PWAPART)

  scale_data(caravan)
}

gironde <- function () {
  data("gironde", package = "PCAmixdata")
  gironde <- cbind(
    gironde$employment, gironde$environment, gironde$housing, gironde$services
  )

  scale_data(gironde[complete.cases(gironde), ])
}

wine <- function () {
  wine <- read.csv("data/winequality-red.csv", sep=";")
  wine$quality <- factor(wine$quality)

  scale_data(wine)
}
