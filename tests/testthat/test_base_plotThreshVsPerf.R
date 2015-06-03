context("plotThreshVsPerf")

test_that("plotThreshVsPerf", {
  gs = 10
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  r = holdout(lrn, binaryclass.task)
  plotThreshVsPerf(r$pred, gridsize = gs)
  plotThreshVsPerfGGVIS(r$pred, gridsize = gs)
})
