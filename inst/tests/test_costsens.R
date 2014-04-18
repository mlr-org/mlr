context("costsens")

test_that("costsens", {
  rdesc = makeResampleDesc("CV", iters = 2L)

  lrn = makeCostSensClassifWrapper(makeLearner("classif.rpart"))
  r = resample(lrn1, costsens.task, rdesc)
  expect_true(!is.na(r$aggr))
  
  lrn = makeCostSensRegrWrapper(makeLearner("regr.rpart"))
  r = resample(lrn, costsens.task, rdesc)
  expect_true(!is.na(r$aggr))
  
  lrn = makeCostSensWeightedPairsWrapper(makeLearner("classif.rpart"))
  r = resample(lrn, costsens.task, rdesc)
  expect_true(!is.na(r$aggr))
})

