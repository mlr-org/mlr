context("resample_cv")


test_that("cv instance works", {
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), size = 25)

  folds = rin$desc$iters
  expect_equal(folds, 3)

  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_equal(sort(c(unique(i1), i2)), 1:25)
  }
})

test_that("SpCV instance works", {

  coords = bc.task.spatial$env$data[, 1:2]

  rin = makeResampleInstance(makeResampleDesc("SpCV", iters = 3), coords = coords)

  folds = rin$desc$iters
  expect_equal(folds, 3)

  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 944)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 944)
    expect_equal(sort(c(unique(i1), i2)), 1:944)
  }
})

test_that("Nested SpRepCV works without errors", {
  bc.task.spatial

  lrn.ksvm = makeLearner("classif.ksvm",
    predict.type = "prob",kernel = "rbfdot")

  ps = makeParamSet(makeNumericParam("C", lower = 1, upper = 1),
    makeNumericParam("sigma", lower = 1, upper = 1))

  ctrl = makeTuneControlRandom(maxit = 1)
  inner = makeResampleDesc("SpCV", iters = 2)

  wrapper.ksvm = makeTuneWrapper(lrn.ksvm, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE, measures = list(auc))

  outer = makeResampleDesc("SpRepCV", folds = 2, reps = 2)

  parallelStart(mode = "multicore", level = "mlr.tuneParams", cpus = 2)

  resa.svm.spatial = resample(wrapper.ksvm, bc.task.spatial,
    resampling = outer, show.info = TRUE, measures = list(auc))
  parallelStop()
})

test_that("cv resampling works", {
  data = multiclass.df
  formula = multiclass.formula
  parset = list(minsplit = 12, cp = 0.09)

  requirePackagesOrSkip("rpart", default.method = "load")
  tt = rpart::rpart
  tp = function(model, newdata) predict(model, newdata, type = "class")

  testCV("classif.rpart", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp, parset = parset)
})

test_that("cv instance works is stochastic", {
  rin = makeResampleInstance(makeResampleDesc("CV", iters = 3), size = 25)

  folds = rin$desc$iters
  expect_equal(folds, 3)

  for (i in 1:folds) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_true(min(i1) >= 1)
    expect_true(max(i1) <= 25)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 25)
    expect_equal(sort(c(unique(i1), i2)), 1:25)
  }
  rin1 = makeResampleInstance(makeResampleDesc("CV", iters = 2L), size = 500)
  rin2 = makeResampleInstance(makeResampleDesc("CV", iters = 2L), size = 500)
  expect_true(!all(sort(rin1$test.inds[[1]]) == sort(rin2$test.inds[[1]])))
})


test_that("test.join works somehow", {
  lrn = makeLearner("classif.rpart", predict.type = "prob")

  # check if test.join computes acc correctly
  mm = setAggregation(acc, test.join)
  r = resample(lrn, sonar.task, cv2, measures = mm)
  rpred = getRRPredictions(r)
  expect_equal(as.numeric(r$aggr),
    mean(getPredictionTruth(rpred) == getPredictionResponse(rpred)))
})
