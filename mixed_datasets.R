caravan <- function () {
  caravan <- ISLR::Caravan
  caravan$MOSTYPE <- factor(caravan$MOSTYPE)
  caravan$MOSHOOFD <- factor(caravan$MOSHOOFD)
  caravan$MGODRK <- factor(caravan$MGODRK)
  caravan$PWAPART <- factor(caravan$PWAPART)

  return(caravan)
}


gironde <- function () {
  return(data("gironde", package = "PCAmix"))
}


wine <- function () {
  wine <- read.csv("data/winequality-red.csv", sep=";")
  wine$quality <- factor(wine$quality)

  return(wine)
}
