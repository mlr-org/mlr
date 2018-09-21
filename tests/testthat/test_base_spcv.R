
test_that("Nested SpRepCV works without errors", {

  data(spatial.task, package = "mlr", envir = environment())

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

  out = resample(wrapper.ksvm, spatial.task,
                 resampling = outer, show.info = TRUE, measures = list(auc))

  expect_vector(out$measures.test$auc, any.missing = FALSE, len = 4)

})

test_that("SpRepCV works without errors", {

  data(spatial.task, package = "mlr", envir = environment())

  learner = makeLearner("classif.ksvm", predict.type = "prob", kernel = "rbfdot")

  resampling = makeResampleDesc("SpRepCV", fold = 2, reps = 2)

  out = resample(learner = learner, task = spatial.task,
                 resampling = resampling, measures = list(auc))

  expect_vector(out$measures.test$auc, any.missing = FALSE, len = 4)

})
