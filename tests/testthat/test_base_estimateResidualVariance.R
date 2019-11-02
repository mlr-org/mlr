context("estimateResidualVariance")

test_that("estimateResidualVariance", {
  set.seed(getOption("mlr.debug.seed"))
  task = regr.task
  lrn = makeLearner("regr.lm")
  x1 = estimateResidualVariance(lrn, task)
  x2 = estimateResidualVariance(lrn, data = regr.df, target = regr.target)
  expect_equal(round(x1, 4), 21.9382)
  expect_equal(x1, x2)
})
