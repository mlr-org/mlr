context("costsens")

test_that("costsens", {
  rdesc = makeResampleDesc("CV", iters = 2L)

  lrn = makeCostSensClassifWrapper(makeLearner("classif.rpart"))
  r = resample(lrn, costsens.task, rdesc)
  expect_true(!is.na(r$aggr))

  # check case where some costs are totally equal
  costs = costsens.costs
  costs[1, ] = 1
  costs[2, ] = 2
  task = makeCostSensTask(data = costsens.feat, costs = costs)
  r = resample(lrn, task, rdesc)
  expect_true(!is.na(r$aggr))

  # check case where all costs are totally equal
  costs[, ] = 1
  task = makeCostSensTask(data = costsens.feat, costs = costs)
  r = resample(lrn, task, rdesc)
  expect_true(!is.na(r$aggr))

  # check that hyperpars are propagated
  lrn2 = setHyperPars(lrn, minsplit = 50)
  m = train(lrn2, costsens.task)
  m2 = m$learner.model$next.model$learner.model
  expect_equal(m2$control$minsplit, 50)

  lrn = makeCostSensRegrWrapper(makeLearner("regr.rpart"))
  r = resample(lrn, costsens.task, rdesc)
  expect_true(!is.na(r$aggr))

  lrn = makeCostSensWeightedPairsWrapper(makeLearner("classif.rpart"))
  r = resample(lrn, costsens.task, rdesc)
  expect_true(!is.na(r$aggr))
})
