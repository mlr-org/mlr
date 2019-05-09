
test_that("Nested SpRepCV works without errors", {

  if (getRversion() > "3.5.3") {
    suppressWarnings(RNGversion("3.5.0"))
  }
  set.seed(getOption("mlr.debug.seed"))

  data(spatial.task, package = "mlr", envir = environment())

  lrn = makeLearner("classif.ranger",
    predict.type = "prob")

  ps = makeParamSet(makeNumericParam("mtry", lower = 3, upper = 3),
    makeNumericParam("num.trees", lower = 10, upper = 10))

  ctrl = makeTuneControlRandom(maxit = 1)
  inner = makeResampleDesc("SpCV", iters = 2)

  wrapper = makeTuneWrapper(lrn, resampling = inner, par.set = ps,
    control = ctrl, show.info = FALSE, measures = list(auc))

  outer = makeResampleDesc("SpRepCV", folds = 2, reps = 2)

  out = resample(wrapper, spatial.task,
    resampling = outer, show.info = TRUE, measures = list(auc))

  expect_vector(out$measures.test$auc, size = 4)
})

test_that("SpRepCV works without errors", {

  if (getRversion() > "3.5.3") {
    suppressWarnings(RNGversion("3.5.0"))
  }
  set.seed(getOption("mlr.debug.seed"))

  data(spatial.task, package = "mlr", envir = environment())

  learner = makeLearner("classif.ranger", predict.type = "prob")

  resampling = makeResampleDesc("SpRepCV", fold = 2, reps = 2)

  out = resample(learner = learner, task = spatial.task,
    resampling = resampling, measures = list(auc))

  expect_vector(out$measures.test$auc, size = 4)
})
