library(devtools)
library(testthat)

load_all(".")

rdesc = makeResampleDesc("Holdout")
ctrl = makeFeatSelControlRandom(maxit = 1L)
expect_message({
  z = selectFeatures("classif.rpart", task = iris.task, resampling = rdesc, control = ctrl, show.info = TRUE)
}, "1: [0-1]+.*mmce.test.mean=0.[0-9]+")


