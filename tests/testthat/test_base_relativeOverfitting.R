context("relativeOverfitting")

test_that("relativeOverfitting works", {
  lrn = makeLearner("classif.knn")
  mod = train(lrn, multiclass.small.task, subset = multiclass.small.train.inds)
  pred = predict(mod, newdata = multiclass.small.test)
  ro = relativeOverfitting(pred, measures = acc, task = multiclass.small.task, model = mod)
  expect_true(is.numeric(ro))
  expect_equal(length(ro), 1)
})

test_that("relativeOverfitting works with multiple measures", {
  lrn = makeLearner("classif.knn")
  mod = train(lrn, multiclass.small.task, subset = multiclass.small.train.inds)
  pred = predict(mod, newdata = multiclass.small.test)
  ro = relativeOverfitting(pred, measures = list(acc, mmce), task = multiclass.small.task, model = mod)
  expect_true(is.numeric(ro))
  expect_equal(length(ro), 2)
})

test_that("relativeOverfitting works for regression", {
  lrn = makeLearner("regr.lm")
  mod = train(lrn, regr.small.task, subset = regr.small.train.inds)
  pred = predict(mod, newdata = regr.small.test)
  ro = relativeOverfitting(pred, measures = rmse, task = regr.small.task, model = mod)
  expect_true(is.numeric(ro))
  expect_equal(length(ro), 1)
})

