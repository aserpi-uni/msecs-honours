# Title     : Honours Programme
# Objective : Comparing dimensionality reduction algorithms for mixed data
# Created by: Alessandro Serpi - 1647244
# Created on: 2020-07-01

caravan <- ISLR::Caravan
caravan$MOSTYPE <- factor(caravan$MOSTYPE)
caravan$MOSHOOFD <- factor(caravan$MOSHOOFD)
caravan$MGODRK <- factor(caravan$MGODRK)
caravan$PWAPART <- factor(caravan$PWAPART)

wine <- read.csv("datasets/winequality-red.csv", sep=";")
wine$quality <- factor(wine$quality, levels = 1:10)

data <- caravan
n_dims <- Inf
