context("BaggingWrapper")

test_that("BaggingWrapper", {
  # classification
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeBaggingWrapper(lrn1, bw.iters = 3L)
  m = train(lrn2, multiclass.task)
  expect_is(m, "BaggingModel")
  expect_is(m, "HomogeneousEnsembleModel")
  expect_true(!inherits(m, "FailureModel"))
  bms = getLearnerModel(m, more.unwrap = FALSE)
  expect_true(is.list(bms) && length(bms) == lrn2$par.vals$bw.iters)
  expect_true(inherits(bms[[1L]], "WrappedModel"))
  bms = getLearnerModel(m, more.unwrap = TRUE)
  expect_true(is.list(bms) && length(bms) == lrn2$par.vals$bw.iters)
  expect_true(inherits(bms[[1L]], "rpart"))
  rdesc = makeResampleDesc("CV", iters = 2)
  r = resample(lrn2, multiclass.task, rdesc)
  expect_true(r$aggr[[1L]] < 0.15)
  lrn2 = makeBaggingWrapper(lrn1, bw.size = 0.1, bw.replace = FALSE)
  m = train(lrn2, multiclass.task)
  bms = getLearnerModel(m)
  expect_equal(unique(sapply(bms, function(m) length(bms[[1]]$subset))), 15L)
  lrn2 = makeBaggingWrapper(lrn1, bw.iters = 3L, bw.feats = 0.5)
  m = train(lrn2, multiclass.task)
  bms = getLearnerModel(m)
  expect_equal(unique(sapply(bms, function(m) length(bms[[1]]$features))), 2L)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeBaggingWrapper(lrn1, bw.iters = 3L)
  lrn2 = setPredictType(lrn2, "prob")
  m = train(lrn2, binaryclass.task)
  p = predict(m, binaryclass.task)
  getPredictionProbabilities(p)
  r = resample(lrn2, binaryclass.task, rdesc, measures = auc)

  # regression
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeBaggingWrapper(lrn1, bw.iters = 3L)
  m = train(lrn2, regr.task)
  p = predict(m, regr.task)
  lrn1 = makeLearner("regr.rpart")
  lrn2 = makeBaggingWrapper(lrn1, bw.iters = 3L)
  lrn2 = setPredictType(lrn2, "se")
  m = train(lrn2, regr.task)
  p = predict(m, regr.task)

  # blocking for BaggingWrapper is testet ind test_base_blocking.R
})

test_that("BaggingWrapper works with feature subsampling", {
  # fnn reported the problem when we had a bug here
  lrn = makeBaggingWrapper(makeLearner("classif.fnn"), bw.iters = 2L, bw.feats = 0.5)
  mod = train(lrn, multiclass.task)
  p = predict(mod, task = multiclass.task)
  expect_true(!is.na(performance(p)))
})

test_that("BaggingWrapper works with 1 obs in newdata", {
  # fnn reported the problem when we had a bug here
  lrn = makeBaggingWrapper("regr.lm", bw.iters = 2L)
  mod = train(lrn, regr.task)
  nd = regr.df[1, , drop = FALSE]
  p = predict(mod, newdata = nd)
  expect_true(!is.na(performance(p)))
})


