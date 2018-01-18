
test_that("Nested SpRepCV works without errors", {

  data(bc.task.spatial, package = "mlr", envir = environment())

  lrn.ksvm = makeLearner("classif.ksvm",
                          predict.type = "prob",
                          kernel = "rbfdot")

  ps = makeParamSet(makeNumericParam("C", lower = 1, upper = 1),
    makeNumericParam("sigma", lower = 1, upper = 1))

  ctrl = makeTuneControlRandom(maxit = 1)
  inner = makeResampleDesc("SpCV", iters = 2)

  wrapper.ksvm = makeTuneWrapper(lrn.ksvm, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE, measures = list(auc))

  outer = makeResampleDesc("SpRepCV", folds = 2, reps = 2)

  out = resample(wrapper.ksvm, bc.task.spatial,
    resampling = outer, show.info = TRUE, measures = list(auc))

  expect_vector(out$measures.test$auc, any.missing = FALSE, len = 4)

})

test_that("SpRepCV works without errors", {

  data(bc.task.spatial, package = "mlr", envir = environment())

  learner = makeLearner("classif.ksvm", predict.type = "prob", kernel = "rbfdot")

  resampling = makeResampleDesc("SpRepCV", fold = 2, reps = 2)

  out = resample(learner = learner, task = bc.task.spatial,
    resampling = resampling, measures = list(auc))

  expect_vector(out$measures.test$auc, any.missing = FALSE, len = 4)

})

test_that("SpCV instance works", {

  data(bc.task.spatial, package = "mlr", envir = environment())

  coords = bc.task.spatial$env$data[, 1:2]
  df = bc.task.spatial$env$data
  makeClassifTask(target = "diplo01", data = df, coordinates = coords)

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
