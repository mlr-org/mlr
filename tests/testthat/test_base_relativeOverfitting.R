context("relativeOverfitting")

test_that("relativeOverfitting works", {
  rdesc = makeResampleDesc("CV", iters = 2)
  ro = estimateRelativeOverfitting(rdesc, acc, multiclass.small.task, makeLearner("classif.knn"))
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works with multiple measures", {
  rdesc = makeResampleDesc("CV", iters = 2)
  ro = estimateRelativeOverfitting(rdesc, list(acc, mmce), multiclass.small.task, makeLearner("classif.knn"))
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_true(is.numeric(ro$relative.overfit.mmce))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works for regression", {
  rdesc = makeResampleDesc("CV", iters = 2)
  ro = estimateRelativeOverfitting(rdesc, mse, regr.small.task, makeLearner("regr.rpart"))
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.mse))
  expect_equal(nrow(ro), 2)
})
