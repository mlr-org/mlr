library(devtools)
library(testthat)

load_all(".")
source("todo-files/getROCCoord.R")

tt = as.factor(c("a", "b", "b"))
pp =           c(0.1, 0.2, 0.7)
mm = data.frame(b = yy, a = 1-yy)
task = makeClassifTask(data = data.frame(x = rnorm(length(tt)), y = tt), target = "y", positive = "b")
p = makePrediction(task$task.desc, NULL, id = "foo", predict.type = "prob", truth = tt, y = mm, time = 0)
rc = getROCCoords(p)

expect_equal(rc, as.data.frame(matrix(c(
  1.0, 0.0, 0,
  0.7, 0.5, 0,
  0.2, 1.0, 0,
  0.1, 1.0, 1,
  0.0, 1.0, 1
), byrow = TRUE, ncol = 3L)), check.names = FALSE)

plotROCCurve(p)
