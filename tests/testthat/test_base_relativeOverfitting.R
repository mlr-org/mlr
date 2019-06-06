context("relativeOverfitting")

test_that("relativeOverfitting works with ResampleDesc", {
  rdesc = makeResampleDesc("CV", iters = 2)
  ro = estimateRelativeOverfitting(rdesc, acc, multiclass.small.task, makeLearner("classif.knn"))
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works with multiple measures with ResampleDesc", {
  rdesc = makeResampleDesc("CV", iters = 2)
  ro = estimateRelativeOverfitting(rdesc, list(acc, mmce), multiclass.small.task, makeLearner("classif.knn"))
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_true(is.numeric(ro$relative.overfit.mmce))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works for regression with ResampleDesc", {
  rdesc = makeResampleDesc("CV", iters = 2)
  ro = estimateRelativeOverfitting(rdesc, mse, regr.small.task, makeLearner("regr.rpart"))
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.mse))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works with ResamplePrediction", {
  rdesc = makeResampleDesc("CV", iters = 2)
  rpred = resample("classif.knn", multiclass.small.task, rdesc)$pred
  ro = estimateRelativeOverfitting(rpred, acc, multiclass.small.task)
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works with multiple measures with ResamplePrediction", {
  rdesc = makeResampleDesc("CV", iters = 2)
  rpred = resample("classif.knn", multiclass.small.task, rdesc)$pred
  ro = estimateRelativeOverfitting(rpred, list(acc, mmce), multiclass.small.task)
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_true(is.numeric(ro$relative.overfit.mmce))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works for regression with ResamplePrediction", {
  rdesc = makeResampleDesc("CV", iters = 2)
  rpred = resample("regr.rpart", regr.small.task, rdesc)$pred
  ro = estimateRelativeOverfitting(rpred, mse, regr.small.task)
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.mse))
  expect_equal(nrow(ro), 2)
})

test_that("relativeOverfitting works with train/test", {
  mod = train(makeLearner("classif.knn"), multiclass.small.task, subset = multiclass.small.train.inds)
  pred.train = predict(mod, task = multiclass.small.task, subset = multiclass.small.train.inds)
  pred.test = predict(mod, task = multiclass.small.task, subset = multiclass.small.test.inds)
  ro = estimateRelativeOverfitting(pred.test, acc, multiclass.small.task, pred.train = pred.train)
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_equal(nrow(ro), 1)
})

test_that("relativeOverfitting works with multiple measures with train/test", {
  mod = train(makeLearner("classif.knn"), multiclass.small.task, subset = multiclass.small.train.inds)
  pred.train = predict(mod, task = multiclass.small.task, subset = multiclass.small.train.inds)
  pred.test = predict(mod, task = multiclass.small.task, subset = multiclass.small.test.inds)
  ro = estimateRelativeOverfitting(pred.test, list(acc, mmce), multiclass.small.task, pred.train = pred.train)
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.acc))
  expect_true(is.numeric(ro$relative.overfit.mmce))
  expect_equal(nrow(ro), 1)
})

test_that("relativeOverfitting works for regression with train/test", {
  mod = train(makeLearner("regr.rpart"), regr.small.task, subset = regr.small.train.inds)
  pred.train = predict(mod, task = regr.small.task, subset = regr.small.train.inds)
  pred.test = predict(mod, task = regr.small.task, subset = regr.small.test.inds)
  ro = estimateRelativeOverfitting(pred.test, mse, regr.small.task, pred.train = pred.train)
  expect_true(is.data.frame(ro))
  expect_true(is.numeric(ro$relative.overfit.mse))
  expect_equal(nrow(ro), 1)
})
